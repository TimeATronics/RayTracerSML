(* Define a vector type *)
datatype vector = Vector of real * real * real

(* Vector operations *)
fun vector_add (Vector (x1, y1, z1), Vector (x2, y2, z2)) =
    Vector (x1 + x2, y1 + y2, z1 + z2)

fun vector_sub (Vector (x1, y1, z1), Vector (x2, y2, z2)) =
    Vector (x1 - x2, y1 - y2, z1 - z2)

fun vector_mul (Vector (x, y, z), scalar: real) =
    Vector (x * scalar, y * scalar, z * scalar)

fun vector_dot (Vector (x1, y1, z1), Vector (x2, y2, z2)) =
    x1 * x2 + y1 * y2 + z1 * z2

fun vector_norm (Vector (x, y, z)) =
    Math.sqrt (x * x + y * y + z * z)

(* Define a ray type *)
datatype ray = Ray of vector * vector

(* Define a hit record type *)
datatype hit_record = HitRecord of real * vector * vector

(* Define a hitable interface *)
signature HITABLE =
sig
    type t
    val hit: t -> ray -> real -> real -> hit_record option
end

(* Define a sphere type *)
structure Sphere : HITABLE =
struct
    type t = vector * real (* center and radius *)

    fun hit (center, radius) (Ray (org, dir)) t_min t_max =
        let
            val oc = vector_sub (org, center)
            val a = vector_dot (dir, dir)
            val b = vector_dot (oc, dir)
            val c = vector_dot (oc, oc) - radius * radius
            val discriminant = b * b - a * c
        in
            if discriminant > 0.0 then
                let
                    val temp1 = (-b - Math.sqrt discriminant) / a
                    val temp2 = (-b + Math.sqrt discriminant) / a
                    val hit1 = (temp1 > t_min) andalso (temp1 < t_max)
                    val hit2 = (temp2 > t_min) andalso (temp2 < t_max)
                in
                    if hit1 then
                        SOME (HitRecord (temp1, vector_add (org, vector_mul (dir, temp1)), vector_sub (vector_add (org, vector_mul (dir, temp1)), center)))
                    else if hit2 then
                        SOME (HitRecord (temp2, vector_add (org, vector_mul (dir, temp2)), vector_sub (vector_add (org, vector_mul (dir, temp2)), center)))
                    else
                        NONE
                end
            else
                NONE
        end
end

(* Main function to demonstrate usage *)
fun main () =
    let
        val sphere = Sphere.hit (Vector (0.0, 0.0, -1.0), 0.5) (Ray (Vector (0.0, 0.0, 0.0), Vector (0.0, 0.0, -1.0))) 0.0 1.0
    in
        case sphere of
            SOME (HitRecord (t, p, n)) => 
                print ("Hit at t: " ^ Real.toString t ^ "\n")
            | NONE => 
                print ("No hit\n")
    end

(* Call the main function *)
val _ = main ()