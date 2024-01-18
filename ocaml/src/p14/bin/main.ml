(*
 * Duplicate the elements of a list.
 *
 * [duplicate : 'a list -> 'a list]
 *)

let duplicate xs =
    let rec helper acc = function
        | [] -> acc
        | y::ys -> helper (y::y::acc) ys
    in
    List.rev (helper [] xs)

let () = assert(
    duplicate ["a"; "b"; "c"; "c"; "d"]
    = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
)

let () = Stdlib.print_endline "Tests passed!"
