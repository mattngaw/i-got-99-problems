(* 
 * Reverse a list.
 *
 * [rev : 'a list -> 'a list]
 *
 * Remark: OCaml standard library has List.rev but we ask that you 
 * reimplement it.
 *)

(*
 * This solution kinda cheats since it uses @.
 * It's also not tail-recursive.
 *)
let rec rev = function
    | [] -> []
    | x::xs -> rev xs @ [x]

let () = assert (rev [] = [])
let () = assert (rev [1] = [1])
let () = assert (rev [1; 2] = [2; 1])
let () = assert (rev [1; 2; 3] = [3; 2; 1])

(* This is nicer :) *)
let rev_tail xs =
    let rec helper acc = function
        | [] -> acc
        | y::ys -> helper (y::acc) ys
    in
    helper [] xs

let () = assert (rev_tail [] = [])
let () = assert (rev_tail [1] = [1])
let () = assert (rev_tail [1; 2] = [2; 1])
let () = assert (rev_tail [1; 2; 3] = [3; 2; 1])

let () = Stdlib.print_endline "Tests passed!"
