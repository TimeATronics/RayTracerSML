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
    fun float_range (min: real, max: real) =
        let
           val rand_value = random (newgen ())
        in
            min + (rand_value * (max - min))
        end

    fun int (upper: int) : int =
        let
            val g = newgen()
            val rand_value = !(#seedref g)
            val scaled_value = (rand_value / m) * real upper
        in
            Real.toInt IEEEReal.TO_NEAREST scaled_value
        end

    fun float (upper: real) : real =
        float_range (0.0, upper)
end