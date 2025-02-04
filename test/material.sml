structure Material = struct

  (* Type declarations *)
  datatype t =
    Metal of { albedo: Vec3.t, fuzzness: real }
  | Dielectric of { ref_index: real }

  datatype face_direction =
    FrontFace
  | BackFace

  type hit_record = {
    t: real,
    p: Vec3.t,
    normal: Vec3.t,
    material: t,
    u: real,
    v: real,
    face_direction: face_direction
  }

  (* Random unit vector generator function *)
  fun random_unit_vector () =
    let
      val a = Random.float_range (0.0, 2.0 * Math.pi)
      val z = Random.float_range (~1.0, 1.0)
      val r = Math.sqrt(1.0 - z * z)
    in
      Vec3.create (r * Math.cos(a), r * Math.sin(a), z)
    end

  type scatter_result = { scattered: Ray.t, attenuation: Vec3.t }

  (* Schlick approximation function *)
  fun schlick cosine ref_idx =
    let
      val r0 = (1.0 - ref_idx) / (1.0 + ref_idx)
      val r0_squared = r0 * r0
    in
      r0_squared + (1.0 - r0_squared) * Math.pow(1.0 - cosine, 5.0)
    end

  (* Scatter function *)
  fun scatter (r: Ray.t) (hit_record: hit_record) =
    let
      val cosine = Vec3.dot (Vec3.negate (Vec3.normalize #direction r)) hit_record.normal
    in
      case hit_record.material of
          Metal { albedo, fuzzness } =>
            let
              val reflected = reflect (Vec3.normalize r.direction) hit_record.normal
              val scattered = Ray.create hit_record.p (Vec3.add reflected (fuzzness * random_unit_vector ())) r.time
              val attenuation = albedo
            in
              if Vec3.dot (Ray.direction scattered) hit_record.normal > 0.0 then
                SOME { scattered = scattered, attenuation = attenuation }
              else
                NONE
            end
        | Dielectric { ref_index } =>
            let
              val attenuation = Vec3.create 1.0 1.0 1.0
              val etai_over_etat =
                case hit_record.face_direction of
                    FrontFace => 1.0 / ref_index
                  | BackFace => ref_index
              val unit_direction = Vec3.normalize r.direction
              val cos_theta = Math.min (Vec3.dot (Vec3.negate unit_direction) hit_record.normal) 1.0
              val sin_theta = Math.sqrt (1.0 - cos_theta * cos_theta)
            in
              if etai_over_etat * sin_theta > 1.0
                 orelse Random.float_range (0.0, 1.0) < schlick cos_theta etai_over_etat then
                let
                  val reflected = reflect unit_direction hit_record.normal
                  val scattered = Ray.create hit_record.p reflected r.time
                in
                  SOME { scattered = scattered, attenuation = attenuation }
                end
              else
                let
                  val refracted = refract unit_direction hit_record.normal etai_over_etat
                  val scattered = Ray.create hit_record.p refracted r.time
                in
                  SOME { scattered = scattered, attenuation = attenuation }
                end
            end
    end
end
