(*
 * Pack consecutive duplicates of list elements into sublists.
 *
 * [pack : 'a list -> 'a list list]
 *)

let pack xs =
    let rec helper acc packed seen = function
        | [] -> packed::acc
        | y::ys -> if y = seen
            then helper acc (y::packed) y ys
            else helper (packed::acc) [y] y ys
    in
    match xs with
        | [] -> []
        | y::ys -> List.rev (helper [] [y] y ys)

let () = assert(
    pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]
    = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]
)

let () = Stdlib.print_endline "Tests passed!"
