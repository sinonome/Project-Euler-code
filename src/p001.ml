(* print_string "Hello, Ocaml!!\n";; *)

(* func *)
let rec range n = if n = 0 then []
                        else n :: range (n-1)

let sum lst = List.fold_right (+) lst 0

let res n = sum (List.filter (fun x -> x mod 3 = 0 || x mod 5 = 0) (range n))

(* main *)
let () = print_int (res 999); print_newline()