(*
 * If you need so, refresh your memory about [run-length encoding](https://en.wikipedia.org/wiki/Run-length_encoding).
 *
 * [encode : 'a list -> (int * 'a) list]
 *)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode xs =
    let rec helper acc encoding = function
        | [] -> encoding::acc
        | y::ys ->
            let (count, seen) = match encoding with
                | One z -> (1, z)
                | Many (c, z) -> (c, z)
            in
            if y = seen then helper acc (Many (count+1, y)) ys
            else helper (encoding::acc) (One y) ys
    in
    match xs with
        | [] -> []
        | y::ys -> List.rev (helper [] (One y) ys)

let () = assert(
    encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
    = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
)

let () = Stdlib.print_endline "Tests passed!"
