(* 2018-09-23 *)
open Core;;

(* pattern matching for a literal value (0) *)
let rec drop_zero list =
  match list with
  | [] -> []
  | 0  :: tl -> drop_zero tl
  | hd :: tl -> hd :: drop_zero tl
;;

drop_zero [1;0;2;3;0];;
(* [1; 2; 3] *)


(* p. 53: performance, & using core_bench *)
#require "core_bench";;
open Core_bench.Std;;

(* didn't work at first, fix here: 
 * https://github.com/realworldocaml/examples/issues/37 *)
let display_config = Bench.Display_config.create
                       ~display:Textutils.Ascii_table.Display.column_titles
                       ~ascii_table:true
                       ~show_percentage:true
                       ()
;;
(* val display_config : Bench.Display_config.t = <abstr> *)

let run_bench tests =
  Bench.bench
    ~display_config:display_config
    tests
;;
(* val run_bench : Core_bench.Test.t list -> unit = <fun> *)

(* commented this out bc it hangs in emacs *)
(* [ Bench.test.create ~name:"plus_one_match" (fun () ->
 *       ignore (plus_one_match 10))
 * ; Bench.test.create ~name:"plus_one_if" (fun () ->
 *       ignore (plus_one_if 10))
 *   |> run_bench
 * ;; *)


(* p. 56, map2_exn and fold *)

List.map ~f:String.length ["Hello"; "World"];;

(* List.map2_exn is similar to List.map, except that it
 * takes two lists and a function for combining them *)

List.map2_exn ~f:Int.max [1;2;3] [3;2;1];;
(* - : int list = [3; 2; 3] *)

(* List.fold is the most complicated of the three, taking
 * three arguments: a list to process, an initial accumulator
 * value, and a function for updating the accumulator *)

List.fold;;
(* 'a list -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum = <fun> *)

List.fold ~init:0 ~f:(+) [1;2;3;4];;
(* - : int = 10 *)

List.fold ~init:[] ~f:(fun list x -> x :: list) [1;2;3;4];;
(* - : int list = [4; 3; 2; 1] *)

let max_widths header rows =
  let lengths l = List.map ~f:String.length l in
  List.fold rows
    ~init:(lengths header)
    ~f:(fun acc row ->
        List.map2_exn ~f:Int.max acc (lengths row))
;;
(* val max_widths : string list -> string list list -> int list = <fun> *)

let render_separator widths =
  let pieces = List.map widths
      ~f:(fun w -> String.make (w + 2) '-')
  in
  "|" ^ String.concat ~sep:"+" pieces ^ "|"
;;
(* val render_separator : int list -> string = <fun> *)
render_separator [12;3;9];;
(* - : string = "|--------------+-----+-----------|" *)

let pad s length =
  " " ^ s ^ String.make (length - String.length s + 1) ' '
;;
(* val pad : string -> int -> string = <fun> *)
pad "hello" 10;;
(* - : string = " hello      " *)

let render_row row widths =
  let padded = List.map2_exn row widths ~f:pad in
  "|" ^ String.concat ~sep:"|" padded ^ "|"
;;
(* val render_row : string list -> int list -> string = <fun> *)

render_row ["Hello";"World"] [12; 9];;
(* - : string = "| Hello        | World     |" *)

let render_table header rows =
  let widths = max_widths header rows in
  String.concat ~sep:"\n"
    (render_row header widths
     :: render_separator widths
     :: List.map rows ~f:(fun row -> render_row row widths)
    )
;;
(* val render_table : string list -> string list list -> string = <fun> *)

printf "%s\n"
  (render_table
     ["language";"architect";"first release"]
     [ ["Lisp" ;"John McCarthy" ;"1958"] ;
       ["C"    ;"Dennis Ritchie";"1969"] ;
       ["ML"   ;"Robin Milner"  ;"1973"] ;
       ["OCaml";"Xavier Leroy"  ;"1996"] ;
]);;

(* | language | architect      | first release |
 * |----------+----------------+---------------|
 * | Lisp     | John McCarthy  | 1958          |
 * | C        | Dennis Ritchie | 1969          |
 * | ML       | Robin Milner   | 1973          |
 * | OCaml    | Xavier Leroy   | 1996          |
 * - : unit = () *)
     

       




    
  

