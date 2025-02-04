structure Texture = struct
  (* Type definition for the texture function *)
  type t = real -> real -> Vec3.t -> Vec3.t

  (* solid_color function: Returns a function that always returns the same color. *)
  fun solid_color (color: Vec3.t) =
    (* Returns a function that ignores u, v, p and always returns color *)
    let
      fun texture_fn ({u: real, v: real, p: real}) = color
    in
      texture_fn
    end

  (* checker function: Returns a function that alternates between two textures (even and odd) *)
  fun checker (even: Vec3.t) (odd: Vec3.t) : Vec3.t =
    (* Helper function to compute the sines of p.x, p.y, and p.z *)
    let
      fun sines (p: Vec3.t) =
        Math.sin(10.0 * #x p) * Math.sin(10.0 * #y p) * Math.sin(10.0 * #z p)
        
      (* Main function that alternates between even and odd textures based on sines *)
      fun texture_fn (u, v, p) =
        if sines p < 0.0 then
          odd (u, v, p)
        else
          even (u, v, p)
    in
      texture_fn
    end

  (* noise function: Returns a function that generates noise based on Perlin noise *)
  val noise : t =
    let
      (* Create the perlin generator *)
      val perlin = Perlin.create ()
      
      (* Function that computes Perlin noise and multiplies by a constant vector *)
      fun texture_fn (u, v, p) =
        let
          (* Compute the Perlin noise value *)
          val noise_value = Perlin.noise p perlin
          
          (* Scale the Perlin noise by the vector (1.0, 1.0, 1.0) manually *)
          val scaled_noise = Vec3.create (noise_value, noise_value, noise_value)
        in
          scaled_noise
        end
    in
      texture_fn
    end
end
