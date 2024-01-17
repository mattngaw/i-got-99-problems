(*
 * If you need so, refresh your memory about [run-length encoding](https://en.wikipedia.org/wiki/Run-length_encoding).
 *
 * [encode : 'a list -> (int * 'a) list]
 *)

let encode xs =
    let rec helper acc encoding = function
        | [] -> encoding::acc
        | y::ys ->
            let (count, seen) = encoding in
            if y = seen then helper acc (count+1, y) ys
            else helper (encoding::acc) (1, y) ys
    in
    match xs with
        | [] -> []
        | y::ys -> List.rev (helper [] (1, y) ys)

let () = assert(
    encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
    = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
)

let () = Stdlib.print_endline "Tests passed!"
