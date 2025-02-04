open Vec3;
open AABB;
open BVH;
open HittablePOV;
structure Utils = struct
    (* Pixel type representing RGB values as integers *)
    type pixel = int * int * int
    (* Converts a colour vector (r, g, b) to a pixel (integer values) for image representation *)
    fun colour_to_pixel {x=r,y=g,z=b} =
        let
            val ir = trunc (255.99 * r)  (* Scale r to [0, 255] and truncate to int *)
            val ig = trunc (255.99 * g)  (* Scale g to [0, 255] and truncate to int *)
            val ib = trunc (255.99 * b)  (* Scale b to [0, 255] and truncate to int *)
        in (ir, ig, ib)
        end
    (* Image type consisting of an array of pixels and its dimensions *)
    type image = { pixels: pixel Array.array, height: int, width: int }
    (* Converts an image to PPM format and writes it to the output stream *)
    fun image2ppm out ({pixels, height, width}: image) =
        let 
            (* Helper function to output a single pixel to the stream *)
            fun onPixel (r,g,b) =
                TextIO.output(out,
                    Int.toString r ^ " " ^
                    Int.toString g ^ " " ^
                    Int.toString b ^ "\n")
        in
            (* Write PPM header and pixels to the output *)
            TextIO.output(out,
                "P3\n" ^
                Int.toString width ^ " " ^ Int.toString height ^ "\n" ^
                "255\n")
            before Array.app onPixel pixels  (* Apply onPixel function to each pixel in the array *)
        end
    (* Renders an image by casting rays for each pixel based on the scene and camera setup *)
    fun render objs width height cam : image =
        let 
            (* Helper function to calculate and convert the color for a pixel *)
            fun pixel l =
                let 
                    val i = l mod width (* Compute x coordinate based on pixel index *)
                    val j = height - l div width (* Compute y coordinate based on pixel index *)
                in
                    colour_to_pixel (trace_ray objs width height cam j i)  (* Trace the ray and convert to pixel *)
                end
            (* Generate an array of pixels for the image *)
            val pixels = Array.tabulate (height*width, pixel)
        in
            (* Return the image with its dimensions and pixel data *)
            {width = width, height = height, pixels = pixels}
        end
end

(* Random Library: https://github.com/diku-dk/sml-random *)
structure Random = struct
    type generator = {seedref : real ref}
    val a = 16807.0
    val m = 2147483647.0
    fun nextrand seed = let val t = a*seed
			in t - m * real(floor(t/m))
			end
    fun newgenseed seed = {seedref = ref (nextrand seed)}
    fun newgen () = newgenseed (Time.toReal(Time.now()))
    fun random {seedref as ref seed} = (seedref := nextrand seed; seed / m)
    fun randomlist (n, {seedref as ref seed0}) =
      let fun h 0 seed res = (seedref := seed; res)
	    | h i seed res = h (i-1) (nextrand seed) (seed / m :: res)
      in h n seed0 []
      end
    fun range (min, max) =
      if min >= max then raise Fail "Random.range: empty range"
      else fn {seedref as ref seed} =>
	   (seedref := nextrand seed; min + (floor(real(max-min) * seed / m)))
    fun rangelist (min, max) =
      if min >= max then raise Fail "Random.rangelist: empty range"
      else fn (n, {seedref as ref seed0}) =>
	   let fun h 0 seed res = (seedref := seed; res)
		 | h i seed res = h (i-1) (nextrand seed)
	                    (min + floor(real(max-min) * seed / m) :: res)
	   in h n seed0 []
	   end
    (* Additional functions for float / int in range *)
    fun float_range (min: real, max: real) =
        let val rand_value = random (newgen ())
        in min + (rand_value * (max - min))
        end
    fun int (upper: int) : int =
        let
            val g = newgen()
            val rand_value = !(#seedref g)
            val scaled_value = (rand_value / m) * real upper
        in Real.toInt IEEEReal.TO_NEAREST scaled_value
        end
    fun float (upper: real) : real =
        float_range (0.0, upper)
end