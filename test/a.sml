fun xor (a: int, b: int): int =

    Int.((a + b) - (a land b) * 2)


(* Example usage *)

val result = xor (5, 3) 