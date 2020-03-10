let inf = 999
let n_switches = 10
let n_clocks = 16

let conn = [
  [0; 1; 2];
  [3; 7; 9; 11];
  [4; 10; 14; 15];
  [0; 4; 5; 6; 7];
  [6; 7; 8; 10; 12];
  [0; 2; 14; 15];
  [3; 14; 15];
  [4; 5; 7; 14; 15];
  [1; 2; 3; 4; 5];
  [3; 4; 5; 9; 13]
]

let check_aligned = List.for_all ((==) 0)

let turn time sw_num n =
  if n == 0 then time
  else
    let clocks = List.nth conn sw_num in
    List.mapi (fun i e -> if List.mem i clocks then (e + n) mod 4 else e) time

let rec sync time sw_num =
  if sw_num == n_switches then
    if check_aligned time then 0 else inf
  else
    [0; 1; 2; 3]
      |> List.map (
        fun turns -> (sync (turn time sw_num turns) (sw_num + 1)) + turns
      )
      |> List.fold_left min inf

let process = List.map (fun time -> (time / 3) mod 4)

let rec repeat n block =
  if n > 0 then (block (); repeat (n - 1) block) else ()

let _ = repeat (read_int ()) (
  fun () ->
    read_line ()
      |> String.split_on_char ' '
      |> List.map int_of_string
      |> fun l -> sync (process l) 0
      |> string_of_int
      |> print_endline
)
