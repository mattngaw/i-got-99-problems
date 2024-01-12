(* 
 * Find out whether a list is a palindrome.
 *
 * [is_palindrome : 'a list -> bool]
 *
 * Hint: A palindrome is its own reverse.
 *)

let is_palindrome xs = (xs = List.rev xs)

let () = assert (is_palindrome [])
let () = assert (is_palindrome [1])
let () = assert (is_palindrome [1; 1])
let () = assert (not (is_palindrome [1; 2]))
let () = assert (not (is_palindrome [1; 2; 3]))
let () = assert (is_palindrome [1; 2; 1])

let () = Stdlib.print_endline "Tests passed!"
