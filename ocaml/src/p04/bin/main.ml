(* 
 * Find the number of elements of a list.
 *
 * [length: 'a list -> int]
 *
 * Remark: OCaml standard library has List.length but we ask that you
 * reimplement it. Bonus for a tail recursive solution.
 *)

let rec length = function
    | [] -> 0
    | _::xs -> 1 + length xs

let () = assert (length [] = 0)
let () = assert (length [1] = 1)
let () = assert (length [1; 2] = 2)
let () = assert (length [1; 2; 3] = 3)

let length_tail xs = 
    let rec helper l = function
        | [] -> l
        | _::xs -> helper (l+1) xs
    in
    helper 0 xs

let () = assert (length_tail [] = 0)
let () = assert (length_tail [1] = 1)
let () = assert (length_tail [1; 2] = 2)
let () = assert (length_tail [1; 2; 3] = 3)

let () = Stdlib.print_endline "Tests passed!"
