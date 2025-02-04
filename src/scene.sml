open Vec3;
open AABB;
open BVH;
open HittablePOV;
open Utils;
structure Scene = struct
    (* Define a scene type containing camera parameters and a list of spheres *)
    type scene = { camLookFrom: pos (* Camera position *)
                , camLookAt: pos (* Point the camera is looking at *)
                , camFov: real (* Field of view for the camera *)
                , spheres: sphere list (* List of spheres in the scene *)
                }
    (* Create a BVH from the spheres and initialize the camera based on the scene parameters *)
    fun from_scene width height (scene: scene) : objs * camera =
        (mk_bvh sphere_aabb (#spheres scene), (* Build a BVH for the spheres *)
        camera (#camLookFrom scene) (#camLookAt scene) {x=0.0, y=1.0, z=0.0} (* Create camera *)
        (#camFov scene) (real width/real height)) (* Set aspect ratio based on width and height *)
    (* Generate a 2D list by applying f to each (j, i) coordinate *)
    fun tabulate_2d m n f =
        List.concat (List.tabulate (m, fn j => List.tabulate (n, fn i => f (j, i))))
    val gradient_scene : scene =
        let val n = 100 (* Number of spheres along each axis *)
            val k = 60.0 (* Scale factor for positioning spheres *)
            val gradient_surface =
                tabulate_2d n n (fn (x,z) =>
                    let val gradient_color = 
                    { x = real x / real n, y = 0.5, z = real z / real n } (* Calculate color based on position *)
                    in
                    (* Position of the sphere *)
                    { pos={x=(~k/2.0 + (k/real n) * real x), 
                    y=0.0,
                    z=(~k/2.0 + (k/real n) * real z)}
                    , colour = gradient_color
                    , radius = k/(real n * 2.0)
                    }
                end)
            val sphere1 = { pos = {x = 0.0, y = 2.0, z = 0.0}, colour = {x = 1.0, y = 0.0, z = 0.0}, radius = 1.0 }
            val sphere2 = { pos = {x = 3.0, y = 2.0, z = 3.0}, colour = {x = 0.0, y = 1.0, z = 0.0}, radius = 1.0 }
            val sphere3 = { pos = {x = 1.0, y = 2.0, z = 3.0}, colour = {x = 0.0, y = 0.0, z = 1.0}, radius = 1.0 }
            (* Combine the gradient surface spheres with the individual spheres *)
            val all_spheres = gradient_surface @ [sphere1, sphere2, sphere3]
        in
        { spheres = all_spheres
        , camLookFrom = {x=0.0, y=5.0, z=10.0}
        , camLookAt = {x=0.0, y=0.0, z=0.0}
        , camFov = 75.0
        }
    end
end