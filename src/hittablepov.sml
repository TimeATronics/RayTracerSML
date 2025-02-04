open Vec3;
open AABB;
open BVH;
structure HittablePOV = struct
    (* Define a hit record containing intersection details *)
    type hit = { t: real, p: pos, normal: dir, colour: colour}
    (* Define a sphere with position, color, and radius *)
    type sphere = { pos: pos, colour: colour, radius: real}
    (* Define a ray with an origin and direction *)
    type ray = {origin: pos, dir: dir}
    (* Calculate the point along a ray at parameter t *)
    fun point_at_param (ray: ray) t =
        vec_add (#origin ray) (scale t (#dir ray))
    (* Define a bounding box for a sphere *)
    type objs = sphere bvh
    fun sphere_aabb {pos, colour=_, radius} =
        {min = vec_sub pos {x=radius, y=radius, z=radius},
        max = vec_add pos {x=radius, y=radius, z=radius}}
    (* Check for ray-sphere intersection and return hit details if any *)
    fun sphere_hit {pos, colour, radius} r t_min t_max : hit option =
        let val oc = vec_sub (#origin r) pos
            val a = dot (#dir r) (#dir r)
            val b = dot oc (#dir r)
            val c = dot oc oc - radius*radius
            val discriminant = b*b - a*c
            (* Helper function to check if a potential intersection is valid *)
            fun try temp =
                if temp < t_max andalso temp > t_min
                then SOME { t = temp
                        , p = point_at_param r temp
                        , normal = scale (1.0/radius)
                                    (vec_sub (point_at_param r temp) pos)
                        , colour = colour
                        }
            else NONE
    in if discriminant <= 0.0
        then NONE
        else case try ((~b - Math.sqrt(b*b-a*c))/a) of
                SOME hit => SOME hit
                | NONE => try ((~b + Math.sqrt(b*b-a*c))/a)
        end
    (* Check for ray-axis-aligned bounding box (AABB) intersection *)
    fun aabb_hit aabb ({origin, dir}: ray) tmin0 tmax0 =
    let fun iter min' max' origin' dir' tmin' tmax' =
            let val invD = 1.0 / dir'
                val t0 = (min' - origin') * invD
                val t1 = (max' - origin') * invD
                val (t0', t1') = if invD < 0.0 then (t1, t0) else (t0, t1)
                val tmin'' = max t0' tmin'
                val tmax'' = min t1' tmax'
            in (tmin'', tmax'') end
        (* Check intersection along the x, y, and z axes *)
        val (tmin1, tmax1) =
            iter
            (#x (#min aabb)) (#x (#max aabb))
            (#x origin) (#x dir)
            tmin0 tmax0
    in if tmax1 <= tmin1 then false
        else let val (tmin2, tmax2) =
                    iter (#y (#min aabb)) (#y (#max aabb))
                    (#y origin) (#y dir)
                    tmin1 tmax1
            in if tmax2 <= tmin2 then false
                else let val (tmin3, tmax3) =
                            iter (#z (#min aabb)) (#z (#max aabb))
                            (#z origin) (#z dir)
                            tmin2 tmax2
                    in not (tmax3 <= tmin3) end
            end
    end
    (* Check for intersection of a ray with objects in a BVH *)
    fun objs_hit (bvh_leaf (_, s)) r t_min t_max =
        sphere_hit s r t_min t_max
    | objs_hit (bvh_split (box, left, right)) r t_min t_max =
        if not (aabb_hit box r t_min t_max)
        then NONE
        else case objs_hit left r t_min t_max of
                SOME h => (case objs_hit right r t_min (#t h) of
                                NONE => SOME h
                            | SOME h' => SOME h')
            | NONE => objs_hit right r t_min t_max
    (* Define a camera with position and orientation *)
    type camera = { origin: pos
                , llc: pos (* Lower-left corner of the view *)
                , horizontal: dir
                , vertical: dir
                }
    (* Create a camera based on position, target, up vector, field of view, and aspect ratio *)
    fun camera lookfrom lookat vup vfov aspect =
    let val theta = vfov * Math.pi / 180.0
        val half_height = Math.tan (theta / 2.0)
        val half_width = aspect * half_height
        val origin = lookfrom
        val w = normalise (vec_sub lookfrom lookat) (* Forward direction *)
        val u = normalise (cross vup w) (* Right direction *)
        val v = cross w u (* Up direction *)
    in { origin = lookfrom
        , llc = vec_sub
                (vec_sub (vec_sub origin (scale half_width u))
                        (scale half_height v)) w
        , horizontal = scale (2.0*half_width) u
        , vertical = scale (2.0*half_height) v
        }
    end
    (* Generate a ray from the camera through a pixel *)
    fun get_ray (cam: camera) s t : ray=
        { origin = #origin cam
        , dir = vec_sub (vec_add (vec_add (#llc cam) (scale s (#horizontal cam)))
                                (scale t (#vertical cam)))
                        (#origin cam)
        }
    (* Reflect a vector off a surface with a given normal *)
    fun reflect v n =
        vec_sub v (scale (2.0 * dot v n) n)
    (* Scatter a ray based on the hit information *)
    fun scatter (r: ray) (hit: hit) =
        let val reflected =
                reflect (normalise (#dir r)) (#normal hit)
            val scattered = {origin = #p hit, dir = reflected}
        in if dot (#dir scattered) (#normal hit) > 0.0
        then SOME (scattered, #colour hit) (* Return scattered ray and color *)
        else NONE
        end
    (* Calculate the color of a ray based on intersections and scattering *)
    fun ray_colour objs r depth =
        case objs_hit objs r 0.001 1000000000.0 of
            SOME hit => (case scatter r hit of
                            SOME (scattered, attenuation) =>
                            if depth < 50
                            then vec_mul attenuation (ray_colour objs scattered (depth+1))
                            else black (* Limit recursion depth *)
                        |  NONE => black)
        | NONE => let val unit_dir = normalise (#dir r)
                        val t = 0.5 * (#y unit_dir + 1.0) (* Gradient for background color *)
                        val bg = {x=0.5, y=0.7, z=1.0} (* Sky color *)
                    in vec_add (scale (1.0-t) white) (scale t bg) (* Blend colors *)
                    end
    (* Trace a ray through the scene and return the resulting color *)
    fun trace_ray objs width height cam j i : colour =
        let val u = real i / real width (* Normalized x coordinate *)
            val v = real j / real height (* Normalized y coordinate *)
            val ray = get_ray cam u v (* Generate ray for pixel *)
        in ray_colour objs ray 0 end (* Calculate color for the ray *)
end

(* From Ray Tracing in One Weekend *)
structure Ray = struct
    type t = { origin: Vec3_Alt.t, direction: Vec3_Alt.t, time: real }
    fun create origin direction time = { origin = origin, direction = direction, time = time }
    fun at (t: real) ({ origin, direction, time }: t) : Vec3_Alt.t =
        Vec3_Alt.+| (origin, Vec3_Alt.*| (t, direction))
    fun to_string { origin, direction, time } =
        "Ray { origin = " ^ Vec3_Alt.to_string origin ^ 
        ", direction = " ^ Vec3_Alt.to_string direction ^
        ", time = " ^ Real.toString time ^ " }"
end