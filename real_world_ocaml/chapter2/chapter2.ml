open Core;;


(* from p. 29 *)
let area_of_ring inner_radius outer_radius =
  let pi = Float.acos (-1.) in
  let area_of_circle r = pi *. r *. r in
  area_of_circle outer_radius -. area_of_circle inner_radius
;;

area_of_ring 1. 3.;;

(* from p. 30 *)
let (ints, strings) =
  List.unzip [(1,"one"); (2,"two"); (3,"three")];;
(* val ints : int list = [1; 2; 3] *)
(* val strings : string list = ["one"; "two"; "three"] *)

(* from p. 31 *)
let upcase_first_entry line =
  match String.split ~on:',' line with
  | [] -> assert false (* false bc split always returns 1 element *)
  | first :: rest -> String.concat ~sep:"," (String.uppercase first :: rest)
;;
(* val upcase_first_entry : string -> string = <fun> *)

upcase_first_entry "andrew,jarrett,ocaml";;
(* string = "ANDREW,jarrett,ocaml" *)


(* p. 32, anonymous fns *)

List.map ~f:(fun x -> x + 1) [1;2;3];;
(* - : int list = [2; 3; 4] *)

(* we can put fns in data structures *)
let increments = [ (fun x -> x + 1); (fun x -> x + 2) ];;

List.map ~f:(fun g -> g 5) increments;;
(* : int list = [6; 7] *)


(* p. 34, recursive fns *)

let rec find_first_stutter list =
  match list with
  | [] | [_] -> None
  | x :: y :: tl ->
    if x = y then Some x else find_first_stutter (y::tl)
;;
(* val find_first_stutter : 'a list -> 'a option = <fun> *)

find_first_stutter [11;4;4;11;1;5];;
(* int option = Some 4 *)


(* p. 35, mutually recursive fns *)

let rec is_even x =
  if x = 0 then true else is_odd (x - 1)
and is_odd x =
  if x = 0 then false else is_even (x - 1)
;;

List.map ~f:is_even [0;1;2;3;4;5;6;7];;
(* : bool list = [true; false; true; false; true; false; true; false] *)

(* p. 36, redefining infix operators *)

let (+!) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2);;
(3,2) +! (-2,4);;
(* : int * int = (1, 6) *)


(* p. 38, compose *)

let path = "/usr/bin:/usr/local/bin:/bin:/sbin";;
String.split ~on:':' path
|> List.dedup_and_sort ~compare:String.compare
|> List.iter ~f:print_endline
;;
(*
/bin
/sbin
/usr/bin
/usr/local/bin
- : unit = ()
*)

(* |> is left-associative. right-associative breaks stuff: *)

let (^>) x f = f x;;
(* val ( ^> ) : 'a -> ('a -> 'b) -> 'b = <fun> *)

(* Sys.getenv_exn "PATH"
 * ^> String.split ~on:':' path
 * ^> List.dedup_and_sort ~compare:String.compare
 * ^> List.iter ~f:print_endline
 * ;; *)

(*
  Error: This expression has type string list -> unit
         but an expression was expected of type
           (string list -> string list) -> 'a
         Type string list is not compatible with type
           string list -> string list 
*)


(* 39: the function keyword *)

let some_or_zero = function
  | Some x -> x
  | None -> 0
;;

List.map ~f:some_or_zero [Some 3; None; Some 4];;
(* : int list = [3; 0; 4] *)

