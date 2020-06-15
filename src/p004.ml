(* func *)

let max lst
    = match lst with
        [] -> raise Not_found
        | first :: rest
            -> List.fold_right (
                    fun a b -> if a > b then a else b
                ) rest first

let rec range m n = if m > n then []
                        else n :: range m (n-1)

let revInt n =
    let rec revInt_ res n
        = if n = 0 then res
            else revInt_ (res * 10 + n mod 10) (n / 10)
        in revInt_ 0 n

let isPali n = n = revInt n

let listTM lst1 lst2 = List.fold_right (
        fun a lst -> List.map (( * ) a) lst2 @ lst
    ) lst1 [] 

let res = max (List.filter isPali (listTM (range 101 999) (range 101 999)))

(* main *)
let () = print_int res; print_newline()