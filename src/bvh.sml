open Vec3;
open AABB;
structure BVH = struct
    (* Define a bounding volume hierarchy (BVH) data type *)
    datatype 'a bvh = bvh_leaf of aabb * 'a (* Leaf node containing an AABB and an object *)
                    | bvh_split of aabb * 'a bvh * 'a bvh (* Split node with AABB and two child BVHs *)
    (* Retrieve the AABB from a BVH node *)
    fun bvh_aabb (bvh_leaf (box, _)) = box
        | bvh_aabb (bvh_split (box, _, _)) = box
        local
        (* Merge two sorted lists based on a comparison function *)
        fun merge cmp ([], ys) = ys
            | merge cmp (xs, []) = xs
            | merge cmp (xs as x::xs', ys as y::ys') =
            case cmp (x, y) of
                GREATER => y :: merge cmp (xs, ys') (* Choose the greater element *)
                | _ => x :: merge cmp (xs', ys) (* Choose the lesser element *)
        (* Sort a list using a comparison function *)
        fun sort cmp [] = []
            | sort cmp [x] = [x]
            | sort cmp xs =
            let
                val ys = List.take (xs, length xs div 2) (* Split in half *)
                val zs = List.drop (xs, length xs div 2)
            in
                merge cmp (sort cmp ys, sort cmp zs) (* Merge Sorted Halves *)
            end
        in
        (* Construct a BVH from a list of objects using a provided function f *)
        fun mk_bvh f all_objs =
            let fun mk _ _ [] = raise Fail "mk_bvh: no nodes" (* Empty Input *)
                | mk _ _ [x] = bvh_leaf(f x, x) (* Create a leaf for a single object *)
                | mk d n xs =
                let val axis = case d mod 3 of 0 => #x (* Determine axis for splitting *)
                            | 1 => #y
                            | _ => #z
                    (* Comparison function for sorting based on the chosen axis *)
                    fun cmp (x, y) =
                        Real.compare(axis(centre(f x)),
                            axis(centre(f y)))
                    val xs_sorted = sort cmp xs
                    val xs_left = List.take(xs_sorted, n div 2)
                    val xs_right = List.drop(xs_sorted, n div 2)
                    (* Recursively build left and right BVH nodes *)
                    fun do_left () = mk (d+1) (n div 2) xs_left
                    fun do_right () = mk (d+1) (n-(n div 2)) xs_right
                    (* Create split node with enclosing AABB *)
                    val (left, right) =
                        if n < 100
                        then (do_left(), do_right())
                        else (do_left(), do_right())
                    val box = enclosing (bvh_aabb left) (bvh_aabb right) (* Enclose both child AABBs *)
                in bvh_split (box, left, right) end (* Return the split node *)
            in mk 0 (length all_objs) all_objs end (* Start building the BVH *)
        end
end