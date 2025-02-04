structure Ray = struct
  type t = { origin: Vec3.t, direction: Vec3.t, time: real }
  fun create origin direction time = { origin = origin, direction = direction, time = time }
  fun at (t: real) ({ origin, direction, time }: t) : Vec3.t =
    Vec3.+| (origin, Vec3.*| (t, direction))
  fun to_string { origin, direction, time } =
    "Ray { origin = " ^ Vec3.to_string origin ^ 
    ", direction = " ^ Vec3.to_string direction ^ 
    ", time = " ^ Real.toString time ^ " }"
end