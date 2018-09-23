(* Working through chapter 1, testing out Tuareg 
   *NOTE:* You can send buffer to utop by running:
   
   C-c C-b

*)

#require "Core";;
open Core;;


let rec destutter list =
  match list with
    | [] -> []
    | [hd] -> [hd]  (* This is different than SML, I think... *)
    | hd1 :: hd2 :: tl ->
      if hd1 = hd2 then destutter (hd2 :: tl)
      else hd1 :: destutter (hd2 :: tl);;

destutter ["yo";"yo";"ma"];;


(* from p. 23, practicing with refs *)

type 'a ref = { mutable contents : 'a };;
let ref x = { contents = x };;
let (!) r = r.contents;;
let (:=) r x = r.contents <- x;;

let sum list =
  let sum = ref 0 in
  List.iter list ~f:(fun x -> sum := !sum + x);
  !sum
;;

sum [1;2;3;4;5;6;7;6;5;4;3;3;];;     (* int = 49 *)


(* p. 24, for/while loops *)
let permute array =
  let length = Array.length array in
  for i = 0 to length - 2 do
    (* pick a `j` to swap with *)
    let j = i + Random.int (length - i) in
    (* swap `i` and `j` *)
    let tmp = array.(i) in
    array.(i) <- array.(j);
    array.(j) <- tmp
  done
;;

let ar = Array.init 20 ~f:(fun i -> i);;
permute ar;;

ar;; (* [|9; 0; 2; 1; 15; 19; 18; 14; 8; 16; 5; 3; 4; 6; 12; 17; 7; 11; 10; 13|] *)
