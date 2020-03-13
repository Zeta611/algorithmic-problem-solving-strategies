open Core
open Stdio

type depth = int
type quadtree = W | B | X of quadtree * quadtree * quadtree * quadtree * depth

let depth = function
  | W | B -> 1
  | X (_, _, _, _, d) -> d
let max_depth q1 q2 = max (depth q1) (depth q2)
let max_depth4 q1 q2 q3 q4 = max (max_depth q1 q2) (max_depth q3 q4)

type iq_pair = {i: int; q: quadtree}

let parse_qtree raw_qtree =
  let len = String.length raw_qtree in
  let rec loop i =
    if i = len then failwith "convert: malformed quadtree"
    else begin
      match raw_qtree.[i] with
      | 'b' -> {i = i + 1; q = B}
      | 'w' -> {i = i + 1; q = W}
      | 'x' ->
        let p1 = loop (i + 1) in
        let p2 = loop p1.i in
        let p3 = loop p2.i in
        let p4 = loop p3.i in
        let d = 1 + max_depth4 p1.q p2.q p3.q p4.q in
        {i = p4.i; q = X (p1.q, p2.q, p3.q, p4.q, d)}
      | _ -> raise (
          Invalid_argument "depth: quadtree should only consist of w, b, or x"
        )
    end
  in
  (loop 0).q

let decompress qtree =
  let dim = Int.pow 2 (depth qtree - 1) in
  let image = Array.make_matrix ~dimx:dim ~dimy:dim false in
  let rec fill x y w h =
    if h = 1 then
      Array.fill image.(y) ~pos:x ~len:w true
    else
      let hh = h / 2 in
      fill x y w hh;
      fill x (y + hh) w hh
  in
  let rec loop x y w = function
    | B -> fill x y w w
    | W -> ()
    | X (q1, q2, q3, q4, d) ->
      let hw = w / 2 in
      loop x y hw q1;
      loop (x + hw) y hw q2;
      loop x (y + hw) hw q3;
      loop (x + hw) (y + hw) hw q4
  in
  loop 0 0 dim qtree;
  image

let rec flip = function
  | X (q1, q2, q3, q4, d) -> X (flip q3, flip q4, flip q1, flip q2, d)
  | bw -> bw

let rec rotate = function
  | X (q1, q2, q3, q4, d) -> X (rotate q2, rotate q4, rotate q1, rotate q3, d)
  | bw -> bw

let rec print_qtree = function
  | B -> Out_channel.output_char stdout 'b'
  | W -> Out_channel.output_char stdout 'w'
  | X (q1, q2, q3, q4, _) ->
    Out_channel.output_char stdout 'x';
    List.iter ~f:print_qtree [q1; q2; q3; q4]

let print_img = Array.iter ~f:(
    fun l ->
      Array.iter ~f:(fun b -> printf "%c" (if b then 'X' else ' ')) l;
      Out_channel.newline stdout
  )


let orig = In_channel.input_line_exn stdin |> parse_qtree
let flipped = flip orig
let rotated = rotate orig

let _ =
  printf "Original image:\n";
  print_img (decompress orig);
  Out_channel.newline stdout;

  printf "Flipped qtree: ";
  print_qtree flipped;
  Out_channel.newline stdout;

  printf "Flipped image:\n";
  print_img (decompress flipped);
  Out_channel.newline stdout;

  printf "Rotated qtree: ";
  print_qtree rotated;
  Out_channel.newline stdout;

  printf "Rotated image:\n";
  print_img (decompress rotated);
  Out_channel.newline stdout
