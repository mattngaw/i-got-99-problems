(* 
 * Write a function [last : 'a list -> 'a option] that returns the last element
 * of a list
 *)

let rec last = function
    | [] -> None
    | [x] -> Some x
    | _::xs' -> last xs'

let () = assert (last [] = None)
let () = assert (last [1] = Some 1)
let () = assert (last [1; 2; 3; 4; 5] = Some 5)

let () = Stdlib.print_endline "Tests passed!"
