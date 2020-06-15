(* func *)
let rec isDiv k n =
    if k * k <= n
        then if n mod k = 0 then false
                        else isDiv (k+2) n
        else true

let isPrime n = if n < 2 then false
            else if n = 2 || n = 3 then true
            else if n mod 2 = 0 then false
            else isDiv 3 n

let rec main_ k loop num = if k < loop
                    then if isPrime num then main_ (k + 1) loop (num + 2)
                                        else main_ k loop (num + 2)
                    else num - 2

let main loop = main_ 1 loop 3

(* main *)
let () = print_int (main 10001); print_newline()
(* 
    * 104743
    * ./program  0.17s user 0.00s system 99% cpu 0.172 total
*)