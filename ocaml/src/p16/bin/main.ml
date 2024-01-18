(*
 * Replicate the elements of a list a given number of times.
 *
 * [replicate : 'a list -> int -> 'a list]
 *)

let drop xs n =
    let rec helper acc k = function
        | [] -> acc
        | y::ys -> match k with
            | 1 -> helper acc n ys
            | k -> helper (y::acc) (k-1) ys
    in
    List.rev (helper [] n xs)

let () = assert(
    drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3
    = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
)

let () = assert(
    drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2
    = ["a"; "c"; "e"; "g"; "i"]
)

let () = assert(
    drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 1
    = []
)

let () = Stdlib.print_endline "Tests passed!"
