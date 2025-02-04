open Vec3;
structure AABB = struct
	(* Type for an axis-aligned bounding box with min and max corners *)
	type aabb = { min: vec3, max: vec3 }
	(* Return the smaller of two values *)
	fun min x y : real = if x < y then x else y
	(* Return the larger of two values *)
	fun max x y : real = if x < y then y else x
	(* Compute the smallest bounding box that contains both input boxes *)
  	fun enclosing (box0: aabb) (box1: aabb) =
    	let
			val small = {
                x = min (#x (#min box0)) (#x (#min box1))
				, y = min (#y (#min box0)) (#y (#min box1))
				, z = min (#z (#min box0)) (#z (#min box1))
			}
			val big = {
                x = max (#x (#max box0)) (#x (#max box1))
			    , y = max (#y (#max box0)) (#y (#max box1))
				, z = max (#z (#max box0)) (#z (#max box1))
			}
		in {min = small, max = big}
		end
	(* Calculate the center of an AABB *)
	fun centre (aabb: aabb) = {
		x = #x (#min aabb) + 0.5 * (#x (#max aabb) - #x (#min aabb)),
		y = #y (#min aabb) + 0.5 * (#y (#max aabb) - #y (#min aabb)),
		z = #z (#min aabb) + 0.5 * (#z (#max aabb) - #z (#min aabb))
	}
end