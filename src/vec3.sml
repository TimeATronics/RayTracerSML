structure Vec3 = struct
    (* Type for a 3D vector with x, y, z components *)
    type vec3 = {x: real, y: real, z: real}
    local
        (* Helper function to apply a binary function to each component of two vectors *)
        fun vf f (v1: vec3) (v2: vec3) =
            {x= f (#x v1, #x v2), y= f (#y v1, #y v2), z= f (#z v1, #z v2)}
    in
        (* Vector addition, subtraction, multiplication, and division using vf *)
        val vec_add = vf (op+)
        val vec_sub = vf (op-)
        val vec_mul = vf (op*)
        val vec_div = vf (op/)
        (* Scalar multiplication *)
        fun scale s {x,y,z} = {x=s*x, y=s*y, z=s*z} : vec3
        (* Dot product of two vectors *)
        fun dot (v1: vec3) (v2: vec3) =
            let val v3 = vec_mul v1 v2
            in #x v3 + #y v3 + #z v3 end
        (* Magnitude of a vector *)
        fun norm v = Math.sqrt (dot v v)
        (* Normalize a vector to unit length *)
        fun normalise v = scale (1.0 / norm v) v
        (* Cross product of two vectors *)
        fun cross {x=x1, y=y1, z=z1} {x=x2, y=y2, z=z2} =
            {x=y1*z2-z1*y2, y=z1*x2-x1*z2, z=x1*y2-y1*x2} : vec3
    end
    (* Types for position, direction, and color as vec3 *)
    type pos = vec3
    type dir = vec3
    type colour = vec3
    (* Default colors *)
    val black : vec3 = {x=0.0, y=0.0, z=0.0}
    val white : vec3 = {x=1.0, y=1.0, z=1.0}
end

structure Vec3_Alt = struct
    (* Type for a 3D vector with x, y, z components *)
    type t = { x: real, y: real, z: real }
    (* Create a new vector with given x, y, z values *)
    fun create x y z = { x = x, y = y, z = z }
    (* Vector addition: component-wise sum of two vectors *)
    fun +| (v1: t, v2: t) = {
        x = #x v1 + #x v2,
        y = #y v1 + #y v2,
        z = #z v1 + #z v2
    }
    (* Vector subtraction: component-wise difference of two vectors *)
    fun -| (v1: t, v2: t) = {
        x = #x v1 - #x v2,
        y = #y v1 - #y v2,
        z = #z v1 - #z v2
    }
    (* Scalar multiplication: scale a vector by a scalar *)
    fun *| (s: real, v: t) = {
        x = s * #x v,
        y = s * #y v,
        z = s * #z v
    }
    (* Scalar division: divide a vector by a scalar *)
    fun /| (v: t, s: real) =
        let val s_inv = 1.0 / s
        in *| (s_inv, v)
        end
    (* Dot product of two vectors: sum of component-wise products *)
    fun dot (v1: t, v2: t) = 
        #x v1 * #x v2 + #y v1 * #y v2 + #z v1 * #z v2
    (* Square of the vector's length (dot product of vector with itself) *)
    fun length_square (v: t) = dot (v, v)
    (* Length (magnitude) of the vector: square root of length_square *)
    fun length (v: t) = Math.sqrt (length_square v)
    (* Negate a vector: component-wise negation *)
    fun negate (v: t) = {
        x = ~ (#x v),
        y = ~ (#y v),
        z = ~ (#z v)
    }
    (* Cross product of two vectors: a vector perpendicular to both *)
    fun cross (u: t, v: t) = {
        x = #y u * #z v - #z u * #y v,
        y = #z u * #x v - #x u * #z v,
        z = #x u * #y v - #y u * #x v
    }
    (* Normalize a vector: divide by its length to make it a unit vector *)
    fun normalise (v: t) = /| (v, length v)
    (* Linear interpolation between two vectors *)
    fun lerp (v1: t, v2: t, t: real) = 
        +| ( *| (1.0 - t, v1), *| (t, v2))
    (* Zero vector: {0.0, 0.0, 0.0} *)
    val zero = create 0.0 0.0 0.0
    (* Element-wise product of two vectors: multiply corresponding components *)
    fun elem_wise_product (v1: t, v2: t) = {
        x = #x v1 * #x v2,
        y = #y v1 * #y v2,
        z = #z v1 * #z v2
    }
    (* Reflect a vector across a normal vector *)
    fun reflect (v: t, n: t) = 
        -| (v, *| (2.0 * dot (v, n), n))
    (* Refract a vector through a surface with a given refractive index ratio *)
    fun refract (uv: t, n: t, etai_over_etat: real) = 
        let 
            val cos_theta = dot (negate uv, n)
            val r_out_parallel = *| (etai_over_etat, +| (uv, *| (cos_theta, n)))
            val r_out_perp = *| (~ (Math.sqrt (1.0 - length_square r_out_parallel)), n)
        in
            +| (r_out_parallel, r_out_perp)
        end
    (* Convert a vector to a string representation *)
    fun to_string {x, y, z} = 
        String.concat ["{", 
        Real.toString x, ", ",
        Real.toString y, ", ",
        Real.toString z, "}"
        ]
end

(* Test_Vec3_Alt *)
(*
val v1 = create 1.0 2.0 3.0
val v2 = create 4.0 5.0 6.0
val sum = +| (v1, v2)
val diff = -| (v1, v2)
val scaled = *| (2.0, v1)
val divided = /| (v1, 2.0)
val dot_product = dot (v1, v2)
val cross_prod = cross (v1, v2)
val normalized = normalise v1
val lerp_result = lerp (v1, v2, 0.5)
val normal = create 0.0 1.0 0.0
val reflected = reflect (v1, normal)
val refracted = refract (v1, normal, 1.5)
val elem_prod = elem_wise_product (v1, v2)
*)