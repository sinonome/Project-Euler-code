(* func *)
let isPrime p = if p < 2 then false
    else if p = 2 || p = 3 then true
    else if p mod 2 = 0 then false
    else let rec _process i n = if i * i <= n
        then begin
            if n mod i = 0 then false
                else _process (i + 2) n
        end
        else true in _process 3 p

let main n = let rec _main i n = if i <= n
    then if isPrime i
        then i + _main (i+1) n
        else _main (i+1) n
    else 0 in _main 2 n

(* main *)
let () = print_int (main 2000000); print_newline()
(* 
    * 142913828922
    * ./program  10.41s user 0.01s system 99% cpu 10.448 total
 *)