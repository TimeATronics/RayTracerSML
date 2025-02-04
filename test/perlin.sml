structure Perlin = struct
  val points_count = 256
  type t = {
    ranfloat : real array,
    perm_x : int array,
    perm_y : int array,
    perm_z : int array
  }

  fun permute (p : int array) (n : int) =
    let
      (* Swap function for permute *)
      fun swap (i, target) =
        let
          val temp = Array.sub (p, i)
        in
          Array.update (p, i, Array.sub (p, target));
          Array.update (p, target, temp)
        end
    in
      List.app (fn i =>
                  let
                    val target = Random.int i
                  in
                    swap (i, target)
                  end) (List.tabulate (n-1, fn x => x))
    end

  fun perlin_generate_perm () =
    let
      val p = Array.tabulate (points_count, fn i => i)
    in
      permute p points_count;
      p
    end

  fun create () =
    let
      val ranfloat =
        Array.tabulate (points_count, fn _ => Random.float 1.0)
      val perm_x = perlin_generate_perm ()
      val perm_y = perlin_generate_perm ()
      val perm_z = perlin_generate_perm ()
    in
      { ranfloat = ranfloat, perm_x = perm_x, perm_y = perm_y, perm_z = perm_z }
    end

  fun noise (p : Vec3.t) ({ ranfloat, perm_x, perm_y, perm_z } : t) =
    let
      val k = 4 * (Real.toInt IEEEReal.TO_NEAREST (#x p))
      val l = 4 * (Real.toInt IEEEReal.TO_NEAREST (#y p))
      val m = 4 * (Real.toInt IEEEReal.TO_NEAREST (#z p))

      val i = IntInf.andb (k, 255)
      val j = IntInf.andb (l, 255)
      val k = IntInf.andb (m, 255)
    in
      Array.sub (ranfloat, IntInf.xorb (IntInf.xorb (Array.sub (perm_x, i), Array.sub (perm_y, j)), Array.sub (perm_z, k)))
    end
end
