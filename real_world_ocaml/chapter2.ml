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

