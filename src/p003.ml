(* func *)
let rec main i tar mx = if tar = 1 then mx
                    else if tar mod i = 0 then main i (tar / i) i
                    else main (i + 1) tar mx

let res n = main 2 n 1

(* main *)
let target = 600851475143
let () = print_int (res target); print_newline()