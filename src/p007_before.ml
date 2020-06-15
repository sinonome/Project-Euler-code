(* func *)
let rec isDiv n lst = match lst with
    [] -> false
    | first :: rest
        -> if first * first > n then false
            else if n mod first = 0 then true
                            else isDiv n rest


let main loop =
    let rec main_ k loop lst num
        = if k < loop
            then if isDiv num lst
                then main_ k loop lst (num + 2)
                else main_ (k+1) loop (lst @ [num]) (num + 2)
            else num - 2
    in main_ 1 loop [] 3

(* main *)
let () = print_int (main 10000); print_newline ()
(*
    * result
    * 104729
    * ./program  5.23s user 0.00s system 99% cpu 5.242 total
 *)

(* test *)
(* isDiv *)
let test1 = isDiv 1 []
let test2 = isDiv 10 []
let test3 = isDiv 10 [1; 2; 5]
let test4 = isDiv 10 [100]
let test5 = isDiv 10 [1; 2; 5; 20]

(* main *)
let test6 = main 4
let test7 = main 5
let test8 = main 25
let test9 = main 168