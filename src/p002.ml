(* func *)
let rec fib_sum m n maxi = if n > maxi then 0
                        else if n mod 2 = 0 then n + fib_sum n (m + n) maxi
                                            else fib_sum n (m + n) maxi

let res n = fib_sum 1 1 n

(* main *)
let () = print_int (res 400_0000); print_newline()