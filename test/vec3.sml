structure Vec3 = struct
  type t = { x: real, y: real, z: real }

  fun create x y z = { x = x, y = y, z = z }

  fun +| (v1: t, v2: t) = 
    { x = #x v1 + #x v2, y = #y v1 + #y v2, z = #z v1 + #z v2 }

  fun -| (v1: t, v2: t) = 
    { x = #x v1 - #x v2, y = #y v1 - #y v2, z = #z v1 - #z v2 }

  fun *| (s: real, v: t) = 
    { x = s * #x v, y = s * #y v, z = s * #z v }

  fun /| (v: t, s: real) = 
    let val s_inv = 1.0 / s in *| (s_inv, v) end

  fun dot (v1: t, v2: t) = 
    #x v1 * #x v2 + #y v1 * #y v2 + #z v1 * #z v2

  fun length_square (v: t) = dot (v, v)

  fun length (v: t) = Math.sqrt (length_square v)

  fun negate (v: t) = 
    { x = ~ (#x v), y = ~ (#y v), z = ~ (#z v) }

  fun cross (u: t, v: t) = 
    { x = #y u * #z v - #z u * #y v,
      y = #z u * #x v - #x u * #z v,
      z = #x u * #y v - #y u * #x v }

  fun normalize (v: t) = /| (v, length v)

  fun lerp (v1: t, v2: t, t: real) = 
    +| ( *| (1.0 - t, v1), *| (t, v2))

  val zero = create 0.0 0.0 0.0

  fun elem_wise_product (v1: t, v2: t) = 
    { x = #x v1 * #x v2, y = #y v1 * #y v2, z = #z v1 * #z v2 }

  fun reflect (v: t, n: t) = 
    -| (v, *| (2.0 * dot (v, n), n))

  fun refract (uv: t, n: t, etai_over_etat: real) = 
    let 
      val cos_theta = dot (negate uv, n)
      val r_out_parallel = *| (etai_over_etat, +| (uv, *| (cos_theta, n)))
      val r_out_perp = *| (~ (Math.sqrt (1.0 - length_square r_out_parallel)), n)
    in
      +| (r_out_parallel, r_out_perp)
    end

  fun to_string {x, y, z} = 
    String.concat ["{", Real.toString x, ", ", Real.toString y, ", ", Real.toString z, "}"]

end