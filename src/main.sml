PolyML.make "vec3.sml";
PolyML.make "aabb.sml";
PolyML.make "bvh.sml";
PolyML.make "hittablepov.sml";
PolyML.make "utils.sml";
PolyML.make "scene.sml";
open Vec3;
open AABB;
open BVH;
open HittablePOV;
open Utils;
open Scene;

fun main () =
    let
        val height = 2000
        val width = 2000
        val imgfile = "output.ppm"
        val scene_name = "gradient_scene"
        val scene = gradient_scene
        val (objs, cam) = from_scene width height scene
        val result = render objs width height cam
        val writeImage = image2ppm
        val _ =
            let
            val out = TextIO.openOut imgfile
            in
            print ("Writing image to " ^ imgfile ^ ".\n")
            before writeImage out result
            before TextIO.closeOut out
            end
    in () end
val _ = main()