(* func *)
let process i j k = if i * i + j * j = k * k
        then begin 
            print_int (i * j * k);
            print_newline ()
        end
        else ()

let rec test i j n f = if i + j < n then begin
        if i >= j then begin
            f i j (n - i - j);
            test (i - 1) (j + 1) n f
        end else test (i + j) 1 n f
    end else ()

let main n = test 1 1 n process

(* main *)
let () = main 1000
(* 
    * 31875000
    * ./program  0.07s user 0.00s system 98% cpu 0.074 total
 *)