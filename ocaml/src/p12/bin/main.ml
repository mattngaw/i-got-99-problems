(*
 * Given a run-length code list generated as specified in the previous problem,
 * construct its uncompressed version.
 *
 * [decode : 'a rle list -> 'a list]
 *)

type 'a rle =
  | One of 'a
  | Many of int * 'a


let decode xs =
    let take_one = function
        | One _ -> raise (Failure "take_one")
        | Many (c, z) -> if c = 2 then One z else Many (c-1, z)
    in
    let rec helper acc = function
        | [] -> acc
        | One z :: ys -> helper (z::acc) ys
        | Many (c, z) :: ys -> helper (z::acc) (take_one (Many (c, z)) :: ys)
    in
    List.rev (helper [] xs)
            

let () = assert(
    decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
    = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
)

let () = Stdlib.print_endline "Tests passed!"
