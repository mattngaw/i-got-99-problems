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

let () = Stdlib.print_endline "Tests passed!"
