(*
 * Replicate the elements of a list a given number of times.
 *
 * [replicate : 'a list -> int -> 'a list]
 *)

let replicate xs n =
    let rec duplicate acc x = function
        | 0 -> acc
        | k -> duplicate (x::acc) x (k-1)
    in
    let rec helper acc = function
        | [] -> acc
        | y::ys -> helper (duplicate acc y n) ys
    in
    List.rev (helper [] xs)

let () = assert(
    replicate ["a"; "b"; "c"] 3
    = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
)

let () = Stdlib.print_endline "Tests passed!"
