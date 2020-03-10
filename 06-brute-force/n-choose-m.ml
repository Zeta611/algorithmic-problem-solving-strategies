let rec list_print = function
  | [] ->
    print_endline ""
  | [hd] ->
    print_int hd;
    print_endline ""
  | hd::tl ->
    print_int hd;
    print_char ',';
    list_print tl
in
let rec choose n m =
  if m == 0 then [[]]
  else if n == 0 then []
  else
    let incl = List.map (fun l -> n :: l) (choose (n - 1) (m - 1)) in
    let excl = choose (n - 1) m in
    incl @ excl
in
let n, m = 5, 3 in
List.iter list_print (choose n m)
