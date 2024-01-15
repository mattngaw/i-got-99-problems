(*
 * Flatten a nested list structure.
 *
 * [flatten : 'a node list -> 'a list]
 *)

type 'a node =
  | One of 'a 
  | Many of 'a node list

let rec flatten = function
    | [] -> []
    | (One x) :: xs' -> x :: (flatten xs')
    | (Many ys) :: xs' -> (flatten ys) @ (flatten xs')

let () = assert (
    flatten [One "a"; Many [One "b"; Many [One "c"; One "d"]; One "e"]]
    = ["a"; "b"; "c"; "d"; "e"]
)

let flatten_tail xs =
    let rec helper acc = function
        | [] -> acc
        | (One x) :: xs' -> helper (acc @ [x]) xs'
        | (Many ys) :: xs' -> helper (acc @ (helper [] ys)) xs'
    in
    helper [] xs

let () = assert (
    flatten_tail [One "a"; Many [One "b"; Many [One "c"; One "d"]; One "e"]]
    = ["a"; "b"; "c"; "d"; "e"]
)

(* Probably not good to do many appends. I got this idea from the solution *)
let flatten_tail_rev xs = 
    let rec helper acc = function
        | [] -> acc
        | (One x) :: xs' -> helper (x :: acc) xs'
        | (Many ys) :: xs' -> helper (helper acc ys) xs'
    in
    List.rev (helper [] xs)

let () = assert (
    flatten_tail_rev [One "a"; Many [One "b"; Many [One "c"; One "d"]; One "e"]]
    = ["a"; "b"; "c"; "d"; "e"]
)

let () = Stdlib.print_endline "Tests passed!"
