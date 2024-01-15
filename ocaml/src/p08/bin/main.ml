(*
 * Eliminate consecutive duplicates of list elements.
 *
 * [compress : 'a list -> 'a list]
 *)

let compress xs =
    let rec helper acc seen = function
        | [] -> acc 
        | y :: ys -> if y = seen 
            then helper acc seen ys 
            else helper (y::acc) y ys
    in
    match xs with
        | [] -> []
        | y :: ys -> List.rev (helper [y] y ys)

let () = assert(compress [] = [])

let () = assert(compress ["a"] = ["a"])

let () = assert(compress ["a"; "a"] = ["a"])

let () = assert(
    compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
    = ["a"; "b"; "c"; "a"; "d"; "e"]
)

let () = Stdlib.print_endline "Tests passed!"
