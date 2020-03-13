open Core
open Stdio

let max_size bars =
  let rec loop s e =
    if s = e then 0
    else if s = e - 1 then bars.(s)
    else
      let mid = (s + e) / 2 in
      let max_lr = max (loop s mid) (loop mid e) in
      let rec cross area h s' e' =
        let nw = e' - s' + 1 in
        if s = s' && e = e' then area
        else if s = s' || (e <> e' && bars.(e') >= bars.(s' - 1)) then
          let nh = min h bars.(e') in
          cross (max area (nw * nh)) nh s' (e' + 1)
        else if e = e' || (s <> s' && bars.(e') < bars.(s' - 1)) then
          let nh = min h bars.(s' - 1) in
          cross (max area (nw * nh)) nh (s' - 1) e'
        else failwith "max_size: this branch should never be reached"
      in
      let h = min bars.(mid - 1) bars.(mid) in
      let max_cross = cross (2 * h) h (mid - 1) (mid + 1) in
      max max_lr max_cross
  in
  loop 0 (Array.length bars)

let num_cases = int_of_string (In_channel.input_line_exn stdin)
let rec loop n =
  if n = num_cases then () else
    let _ = In_channel.input_line_exn stdin in
    let bars = In_channel.input_line_exn stdin
      |> String.split ~on:' '
      |> List.map ~f:int_of_string
      |> List.to_array
    in
    printf "%d\n" (max_size bars);
    loop (n + 1)
let _ = loop 0
