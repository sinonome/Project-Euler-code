(* func *)
(* input file *)
let inputfile name =
    let ic = open_in name in
        let rec inp ic =
            try
                let line = input_line ic in
                    line ^ (inp ic)
            with e
                -> close_in_noerr ic; ""
        
        in inp ic;;

(* other *)
let zero = int_of_char '0'

let rec times numSTR = let len = String.length numSTR in
    if len = 0 then 1
            else (int_of_char numSTR.[len - 1] - zero)
                            * times (String.sub numSTR 0 (len - 1))

let main numSTR len =
    let rec main_ i len numSTR ans =
        try
            let substr = String.sub numSTR i len in
                let res = times substr in if res > ans then main_ (i + 1) len numSTR res
                                                    else main_ (i + 1) len numSTR ans
        with e ->
            ans
    in main_ 0 len numSTR 0 

(* var *)
let file = "data/p008.dat"
let numSTR = inputfile file

(* main *)
let () = print_int (main numSTR 13); print_newline()

(* 
    * 23514624000
    * ./program  0.01s user 0.00s system 78% cpu 0.013 total
 *)