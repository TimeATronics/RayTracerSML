type vec3 = {x: real, y: real, z: real}

local
    fun vf f (v1: vec3) (v2: vec3) =
        {x= f (#x v1, #x v2),
         y= f (#y v1, #y v2),
         z= f (#z v1, #z v2)}
in

val vec_add = vf (op+)
val vec_sub = vf (op-)
val vec_mul = vf (op* )
val vec_div = vf (op/)

fun scale s {x,y,z} = {x=s*x, y=s*y, z=s*z} : vec3

fun dot (v1: vec3) (v2: vec3) =
    let val v3 = vec_mul v1 v2
    in #x v3 + #y v3 + #z v3 end

fun norm v = Math.sqrt (dot v v)

fun normalise v = scale (1.0 / norm v) v

fun cross {x=x1, y=y1, z=z1} {x=x2, y=y2, z=z2} =
    {x=y1*z2-z1*y2, y=z1*x2-x1*z2, z=x1*y2-y1*x2} : vec3

end

type pos = vec3
type dir = vec3
type colour = vec3

val black : vec3 = {x=0.0, y=0.0, z=0.0}
val white : vec3 = {x=1.0, y=1.0, z=1.0}

type ray = {origin: pos, dir: dir}

fun point_at_param (ray: ray) t =
    vec_add (#origin ray) (scale t (#dir ray))

type hit = { t: real
           , p: pos
           , normal: dir
           , colour: colour
           }

type sphere = { pos: pos
              , colour: colour
              , radius: real
              }

fun sphere_aabb {pos, colour=_, radius} =
    {min = vec_sub pos {x=radius, y=radius, z=radius},
     max = vec_add pos {x=radius, y=radius, z=radius}}

fun sphere_hit {pos, colour, radius} r t_min t_max : hit option =
    let val oc = vec_sub (#origin r) pos
        val a = dot (#dir r) (#dir r)
        val b = dot oc (#dir r)
        val c = dot oc oc - radius*radius
        val discriminant = b*b - a*c
        fun try temp =
            if temp < t_max andalso temp > t_min
            then SOME { t = temp
                      , p = point_at_param r temp
                      , normal = scale (1.0/radius)
                                 (vec_sub (point_at_param r temp) pos)
                      , colour = colour
                      }
        else NONE
  in if discriminant <= 0.0
     then NONE
     else case try ((~b - Math.sqrt(b*b-a*c))/a) of
              SOME hit => SOME hit
            | NONE => try ((~b + Math.sqrt(b*b-a*c))/a)
    end

type objs = sphere list

fun objs_hit (objs: objs) r t_min t_max =
    let fun hit_each [] = NONE
          | hit_each (s::ss) =
                case sphere_hit s r t_min t_max of
                    SOME h => SOME h
                  | NONE => hit_each ss
    in hit_each objs end

type camera = { origin: pos
              , llc: pos
              , horizontal: dir
              , vertical: dir
              }

fun camera lookfrom lookat vup vfov aspect =
  let val theta = vfov * Math.pi / 180.0
      val half_height = Math.tan (theta / 2.0)
      val half_width = aspect * half_height
      val origin = lookfrom
      val w = normalise (vec_sub lookfrom lookat)
      val u = normalise (cross vup w)
      val v = cross w u
  in { origin = lookfrom
     , llc = vec_sub
             (vec_sub (vec_sub origin (scale half_width u))
                     (scale half_height v)) w
     , horizontal = scale (2.0*half_width) u
     , vertical = scale (2.0*half_height) v
     }
  end

fun get_ray (cam: camera) s t : ray=
    { origin = #origin cam
    , dir = vec_sub (vec_add (vec_add (#llc cam) (scale s (#horizontal cam)))
                             (scale t (#vertical cam)))
                    (#origin cam)
    }

fun reflect v n =
    vec_sub v (scale (2.0 * dot v n) n)

fun scatter (r: ray) (hit: hit) =
    let val reflected =
            reflect (normalise (#dir r)) (#normal hit)
        val scattered = {origin = #p hit, dir = reflected}
    in if dot (#dir scattered) (#normal hit) > 0.0
       then SOME (scattered, #colour hit)
       else NONE
    end

fun ray_colour objs r depth =
    case objs_hit objs r 0.001 1000000000.0 of
        SOME hit => (case scatter r hit of
                         SOME (scattered, attenuation) =>
                         if depth < 50
                         then vec_mul attenuation (ray_colour objs scattered (depth+1))
                         else black
                      |  NONE => black)
      | NONE => let val unit_dir = normalise (#dir r)
                    val t = 0.5 * (#y unit_dir + 1.0)
                    val bg = {x=0.5, y=0.7, z=1.0}
                in vec_add (scale (1.0-t) white) (scale t bg)
                end

fun trace_ray objs width height cam j i : colour =
    let val u = real i / real width
        val v = real j / real height
        val ray = get_ray cam u v
    in ray_colour objs ray 0 end

type pixel = int * int * int

fun colour_to_pixel {x=r,y=g,z=b} =
    let val ir = trunc (255.99 * r)
        val ig = trunc (255.99 * g)
        val ib = trunc (255.99 * b)
    in (ir, ig, ib) end

type image = { pixels: pixel Array.array
             , height: int
             , width: int}

fun image2ppm out ({pixels, height, width}: image) =
    let fun onPixel (r,g,b) =
            TextIO.output(out,
                          Int.toString r ^ " " ^
                          Int.toString g ^ " " ^
                          Int.toString b ^ "\n")
    in TextIO.output(out,
                     "P3\n" ^
                     Int.toString width ^ " " ^ Int.toString height ^ "\n" ^
                     "255\n")
       before Array.app onPixel pixels
    end

fun render objs width height cam : image =
    let fun pixel l =
            let val i = l mod width
                val j = height - l div width
            in colour_to_pixel (trace_ray objs width height cam j i)
            end
        val pixels = Array.tabulate (height*width, pixel)
    in {width = width,
        height = height,
        pixels = pixels
       }
    end

type scene = { camLookFrom: pos
             , camLookAt: pos
             , camFov: real
             , spheres: sphere list
             }

fun from_scene width height (scene: scene) : objs * camera =
  (scene.spheres, camera (#camLookFrom scene) (#camLookAt scene) {x=0.0, y=1.0, z=0.0}
   (#camFov scene) (real width/real height))

fun getopt needle argv f def =
    case argv of
        opt::x::xs =>
          if opt = needle
          then f x else getopt needle (x::xs) f def
        | _ => def

fun int s = valOf (Int.fromString s)
fun id x = x

fun main () =
    let val height = getopt "-m" (CommandLine.arguments()) int 200
        val width = getopt "-n" (CommandLine.arguments()) int 200
        val imgfile = getopt "-f" (CommandLine.arguments()) SOME NONE
        val scene_name = getopt "-s" (CommandLine.arguments()) id "gradient_scene"

        val scene = case scene_name of
                        "gradient_scene" => { spheres = [{ pos={x=0.0, y=0.0, z= -1.0}, radius=0.5, colour=white }]
                                            , camLookFrom = {x=0.0, y=0.0, z=3.0}
                                            , camLookAt = {x=0.0, y=0.0, z=0.0}
                                            , camFov = 75.0 }
                      | s => raise Fail ("No such scene: " ^ s)

        val _ = print ("Using scene '" ^ scene_name ^ "' (-s to switch).\n")

        val t0 = Time.now ()
        val (objs, cam) = from_scene width height scene
        val t1 = Time.now ()
        val _ = print ("Scene setup in " ^ Time.fmt 4 (Time.- (t1, t0)) ^ "s.\n")

        val t0 = Time.now ()
        val result = render objs width height cam
        val t1 = Time.now ()

        val _ = print ("Rendering in " ^ Time.fmt 4 (Time.- (t1, t0)) ^ "s.\n")

        val writeImage = image2ppm

        val _ = case imgfile of
                    SOME imgfile' =>
                    let val out = TextIO.openOut imgfile'
                    in print ("Writing image to " ^ imgfile' ^ ".\n")
                       before writeImage out result
                       before TextIO.closeOut out
                    end
                  | NONE =>
                    print ("-f not passed, so not writing image to file.\n")
    in () end

val _ = main()
