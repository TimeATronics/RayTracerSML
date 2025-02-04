(*
    int image_width = 256;
    int image_height = 256;
    std::cout << "P3\n" << image_width << ' ' << image_height << "\n255\n";
    for (int j = 0; j < image_height; j++) {
        for (int i = 0; i < image_width; i++) {
            auto r = double(i) / (image_width-1);
            auto g = double(j) / (image_height-1);
            auto b = 0.0;
            int ir = int(255.999 * r);
            int ig = int(255.999 * g);
            int ib = int(255.999 * b);
            std::cout << ir << ' ' << ig << ' ' << ib << '\n';
        }
    }
*)

let
    val image_width = 256
    val image_height = 256

    fun toInt x = Real.floor (255.999 * x)  (* Convert real to int properly *)

    fun printPixel (r, g, b) =
        print (Int.toString (toInt r) ^ " " ^
               Int.toString (toInt g) ^ " " ^
               Int.toString (toInt b) ^ "\n")

    fun renderRow j =
        let fun loop i =
                if i < image_width then
                    let
                        val r = Real.fromInt i / Real.fromInt (image_width - 1)
                        val g = Real.fromInt j / Real.fromInt (image_height - 1)
                        val b = 0.0
                    in
                        printPixel (r, g, b);
                        loop (i + 1)
                    end
                else ()
        in
            loop 0
        end

    fun renderImage j =
        if j < image_height then (renderRow j; renderImage (j + 1))
        else ()
in
    print ("P3\n" ^ Int.toString image_width ^ " " ^ Int.toString image_height ^ "\n255\n");
    renderImage 0
end
