open List

(*
   A simple image library with
   * rasterization (continuous R²->[0,1]³ -> discrete N²->[0,255]³)
   * ppm file format support (import and export)
   * bmp export (not implemented yet)
   * compatibility with SOOCaml 
*)

type 'a image        = float*float -> 'a
type 'a rasterized_image = 
  {
    image: int*int -> 'a;
    width: int;
    height: int
  }

let map_triple f (a,b,c) = (f a, f b, f c)

let rasterize (w,h) img =
  {
    image = (fun (x,y) -> 
      let pixel = img ((float x)/.(float w), (float y)/.(float h)) in
      map_triple (fun x -> (int_of_float (255. *. x))) pixel
    );
    width = w;
    height = h
  }

let continuous_image img =
  fun (x,y) -> let pixel = img.image (int_of_float (x*.float img.width), int_of_float (y*.float img.height)) in
  map_triple (fun x -> (float x)/.255.) pixel

let scale_image (sx,sy) img = fun (x,y) -> img (x/.sx,y/.sy)

let to_ppm (img: (int*int*int) rasterized_image) : string =
  "P3\n" ^ (string_of_int img.width) ^ " " ^ (string_of_int img.height) ^ "\n255\n" ^
  (
    List.init img.height (fun y -> 
      List.init img.width (fun x -> 
        img.image (x,y)
      )
    )
    |> map (map (fun (r,g,b) -> (string_of_int r) ^ " " ^ (string_of_int g) ^ " " ^ (string_of_int b)))
    |> map (String.concat " ")
    |> String.concat "\n"
  )

let save_ppm filename img = 
  let oc = open_out_bin filename in
  output_string oc (to_ppm img);
  close_out oc

let drop_comment s =
  let i = String.index s '#' in
  String.sub s 0 i

let group n xs = 
  let rec aux acc k xs = 
    if k = 0 then (List.rev acc)::(aux [] k xs)
    else match xs with
      | [] -> [List.rev acc]
      | x::xs -> aux (x::acc) (k-1) xs
  in aux [] n xs

let rec iter a f n = if n = 0 then a else iter (f a) f (n-1)


let read_ppm filename =
  let ic = open_in_bin filename in
  let header = input_line ic in
  if header <> "P3" then failwith "Not a P3 PPM file";
  let width, height = 
    let line = drop_comment (input_line ic) in
    let ws = String.split_on_char ' ' line in
    int_of_string (List.nth ws 0), int_of_string (List.nth ws 1)
  in
  let maxval = int_of_string (drop_comment (input_line ic)) in
  let img_arr = Array.make_matrix width height (0,0,0) in
  let parse_value s = 255*(int_of_string s) / maxval in
  iter 0
  (fun y ->
    let line = drop_comment (input_line ic) in
    let ws = String.split_on_char ' ' line in
    let rgb = group 3 ws |> map (map parse_value) in
    List.iteri (fun x -> function 
      [r;g;b] -> img_arr.(x).(y) <- (r,g,b)
      | _ -> failwith "Invalid PPM file"
    ) rgb;
    y+1
  )
  height|> ignore;
  close_in ic;
  {
    image = (fun (x,y) -> img_arr.(x).(y));
    width = width;
    height = height
  }


(*
let eval_image ?(wh=(256,256)) name img =
  img
  |> rasterize wh
  |> save_ppm name
*)

(* for SOOCaml *)
let eval_image ?(wh=(256,256)) name img =
  img
  |> rasterize wh
  |> to_ppm
  |> print_endline 


let black = (0.0,0.0,0.0)
let white = (1.0,1.0,1.0)

;;
