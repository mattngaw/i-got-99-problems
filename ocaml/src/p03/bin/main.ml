(* 
 * Find the N'th element of a list.
 *
 * [nth : 'a list -> int -> 'a]
 *
 * Remark: OCaml has List.nth which numbers elements from 0 and raises an
 * exception if the index is out of bounds.
 *)

let rec nth xs n =
    match xs with
    | [] -> raise (Failure "nth")
    | x::xs' -> if n = 0 then x else nth xs' (n-1)

let () = assert (
    try ignore (nth [] 0); false
    with Failure _ -> true
)

let () = assert (
    try ignore (nth [] 3); false
    with Failure _ -> true
)

let () = assert (
    try ignore (nth [1] 1); false
    with Failure _ -> true
)

let () = assert (
    try nth [1] 0 = 1
    with Failure _ -> false
)

let () = assert (
    try nth [1; 2; 3] 2 = 3
    with Failure _ -> false
)

let () = Stdlib.print_endline "Tests passed!"
