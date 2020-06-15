(* func *)
let f n =
        let rec f_ n = if n = 0 then 0
                    else n + f_ (n-1)
    in let res = f_ n in res * res

let rec g n = if n = 0 then 0 else n * n + g (n-1)

let main n = f n - g n

(* main *)
let () = print_int (main 100); print_newline()
