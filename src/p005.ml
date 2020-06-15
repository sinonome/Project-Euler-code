(* func *)
let rec gcd m n =
        let rec gcd_ m n = if m = 0 then n
                                else gcd_ (n mod m) m
    in if m < n then gcd_ m n
                else gcd_ n m

let lcm m n = m * n / (gcd m n)
let rec makenumset m n = if m <= n then m :: makenumset (m + 1) n
                                else []

let main n = List.fold_right lcm (makenumset 2 n) 1

(* main *)
let () = print_int (main 20); print_newline()

(* test gcd *)
let test1 = gcd 1 1
let test2 = gcd 2 1
let test3 = gcd 1 2
let test4 = gcd 4 100
let test5 = gcd 100 25
let test6 = gcd 40 60

(* test lcm *)
let test7 = lcm 1 1
let test8 = lcm 2 1
let test9 = lcm 1 2
let test10 = lcm 4 100
let test11 = lcm 100 25
let test12 = lcm 40 60

(* test makenumset *)
let test13 = makenumset 1 1
let test14 = makenumset 1 (-1)
let test15 = makenumset 0 10
let test16 = makenumset 10 20