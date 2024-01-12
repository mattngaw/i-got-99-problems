(* 
 * Find the last but one (last and penultimate) elements of a list.
 * [last_two : 'a list -> ('a * 'a) option]
 *)

let rec last_two = function
    | [] | [_] -> None
    | [x1; x2] -> Some (x1, x2)
    | _::xs -> last_two xs

let () = assert (last_two [] = None)
let () = assert (last_two [1] = None)
let () = assert (last_two [1; 2] = Some (1, 2))
let () = assert (last_two [1; 2; 3; 4; 5] = Some (4, 5))

let () = Stdlib.print_endline "Tests passed!"
