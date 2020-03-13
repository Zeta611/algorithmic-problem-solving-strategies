(* open Base *)
open Core
open Stdio

let print_int_list = List.iter ~f:(printf "%d")

(* Karatsuba algorithm
 * a = a1 * 10^n + a0
 * b = b1 * 10^n + b0
 * a * b = a1 * b1 * 10^(2n) + (a1 * b0 + a0 * b1) * 10^n + a0 * b0
 *       = z2 * 10^(2n) + z1 * 10^n + z0
 * z2 = a1 * b1
 * z0 = a0 * b0
 * z1 = a1 * b0 + a0 * b1 = (a0 + a1) * (b0 + b1) - z0 - z2
 *)
let mult a b =
  let rec add a b k =
    match a, b, k with
    | _, [], _ -> a
    | [], _, _ -> List.init k ~f:(fun _ -> 0) @ b
    | ahd :: atl, bhd :: btl, 0 -> ahd + bhd :: (add atl btl 0)
    | ahd :: atl, _, _ -> ahd :: (add atl b (k - 1))
  in
  let add2 a b k1 c k2 = add (add a b k1) c k2
  in
  let rec sub a b =
    match a, b with
    | _, [] -> a
    | [], _ -> raise (Invalid_argument "b is greater than a")
    | ahd :: atl, bhd :: btl -> ahd - bhd :: sub atl btl
  in
  let sub2 a b c = sub (sub a b) c
  in
  let rec karatsuba a b =
    let len_a = List.length a in
    let len_b = List.length b in
    if len_a = 0 || len_b = 0 then []
    else if len_a < len_b then karatsuba b a
    else if len_a = 1 then
      let _ = assert (len_b = 1) in
      [List.hd_exn a * List.hd_exn b]
    else
      let half_len_a = len_a / 2 in
      let a0, a1 = List.split_n a half_len_a in
      let b0, b1 = List.split_n b half_len_a in
      let z2 = karatsuba a1 b1 in
      let z0 = karatsuba a0 b0 in
      let z1 = sub2 (karatsuba (add a0 a1 0) (add b0 b1 0)) z0 z2 in
      add2 z0 z1 half_len_a z2 (2 * half_len_a)
  in
  let normalize l =
    let rec carry = function
      | [] -> []
      | [hd] -> if hd = 0 then [0] else carry [hd; 0]
      | hd :: sd :: tl ->
        if hd > 9 then hd mod 10 :: carry (sd + hd / 10 :: tl)
        else if hd < 0 then
          let borrow = (abs hd - 1) / 10 + 1 in
          hd + borrow * 10 :: carry (sd - borrow :: tl)
        else hd :: carry (sd :: tl)
    in
    l |> carry |> List.rev |> List.drop_while ~f:((=) 0)
  in
  normalize (karatsuba a b)

let int_list_of_string s =
  let rec loop i =
    if i = String.length s then [] else
      Caml.int_of_string (String.make 1 s.[i]) :: loop (i + 1)
  in
  loop 0

let read_int_list () =
  int_list_of_string (In_channel.input_line_exn stdin)

let n = List.rev (read_int_list ())
let m = List.rev (read_int_list ())

let _ = print_int_list (mult n m)
let _ = Out_channel.newline stdout
