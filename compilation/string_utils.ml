(* put string manip stuff with docs here  . imma fix everythiong up l8r for now here is just what sdhould be here *)
(* also rename "_to_" to "_of_" *)

(* do i put list utils in a sep file?? *)
(* also math utils?? *)

let int_to_str_with_padding i =
  if i < 9 then "0" ^ string_of_int i else string_of_int i

let str_to_char_list s = String.fold_right (fun c acc -> c :: acc) s []
let char_list_to_str chars = String.of_seq (List.to_seq chars)

(** [i] and [j] are indices of l that are inclusive *)
let list_slice i j l = 
  let indexed_list = List.mapi (fun index x -> (index, x)) l in
  List.filter (fun (index, _) -> i <= index && index <= j) indexed_list |>
  List.map (fun (_, x) -> x)

let substring s i j =
  let len = String.length s in
  if i < 0 || j >= len || i > j then ""  
  else String.sub s i (j - i + 1)

  (* comment needs to be updated b/c third part kinda just goes away i think  *)
(** splits string into two parts based on two predicates. the first and second parts are from the first and second predicates, respectively. *)
let split_str str pred1 pred2 =
    let str_chars = str_to_char_list str in
    let first_part = List.find_index pred1 str_chars in 
    let second_part = List.find_index pred2 str_chars in 
    match first_part with
      | Some f_i -> (
        match second_part with
        | Some s_i -> 
          if f_i < s_i then
            let first_str = substring str 0 (f_i - 1) in
            let second_str = substring str (f_i + 1) (s_i - 1) in
            (first_str, second_str)
          else
            let second_str = substring str 0 (s_i - 1) in
            let first_str = substring str (s_i + 1) (f_i - 1) in
            (first_str, second_str)
        | None -> 
          (substring str 0 (f_i - 1), "")
        )
      | None -> (
        match second_part with
        | Some s_i ->
          ("", substring str 0 (s_i - 1))
        | None -> ("", "")
      )

let split_and_collect_matches str re =
  Str.full_split re str |> 
  List.map (function Str.Text s -> s | Str.Delim s -> s)

(* consider generalizing this? *)
let split_preserve_quotes_and_parens str =
  Str.full_split (Str.regexp "\"[^\"]*\"\\|([^)]*)\\|[ ]+") str
  |> List.filter (function
       | Str.Delim " " -> false
       | Str.Text "" -> false
       | _ -> true)
  |> List.map (function
       | Str.Text s -> s
       | Str.Delim s -> s)