structure Option_ext = struct
  fun or_else (f: unit -> 'a option) (opt: 'a option) : 'a option =
    case opt of
         NONE => f ()
       | SOME _ => opt
end