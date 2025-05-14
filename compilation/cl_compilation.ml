(* CalLang compilation (cl_compilation)

CalLang - Calendar Language
Made in order to automate adding events to calendar, avoiding
potential errors made by entering it in manually. Duplicating the same
event over and over, one at a time, brings more more room for error
than simply entering plaintext and running CalLang. 

This takes a .cal file and turns it into a version more understandable for cl_exec.
*)

(* TODO: make sure to clean this up. lol. at least if u want to add more features. also update docs cuz it is quite diff now! *)
  (* it works tho ... so thats pretty cool ... *)
(* TODO: better error handling *)
(* TODO: make sure A/P clock time cant be negative as that does not make sense *)

(* ...should this be an actual interpreter? thinking about it. *)

(* really only the last bit in the "main" function should be here in cl_compilation. also rename this to main.ml  *)

(* no am/pm specification here; time must be military time *)
(* note: must do arithmetic mod 24 *)
(* also hrs can be like "7.5h" so we convert it into minutes *)

let int_to_str_with_padding i =
  if i < 9 then "0" ^ string_of_int i else string_of_int i

let str_to_char_list s = String.fold_right (fun c acc -> c :: acc) s []
let char_list_to_str chars = String.of_seq (List.to_seq chars)

(** [i] and [j] are indices of l that are inclusive *)
let list_slice i j l = 
  let indexed_list = List.mapi (fun index x -> (index, x)) l in
  List.filter (fun (index, _) -> i <= index && index <= j) indexed_list |>
  List.map (fun (_, x) -> x)

let frac x = x -. Float.floor x
let substring s i j =
  let len = String.length s in
  if i < 0 || j >= len || i > j then ""  
  else String.sub s i (j - i + 1)

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

let parse_hm_int input_num_str =
  let compute_time hours_part mins_part =
    let hrs1 = float_of_string hours_part in
    let hrs_int1 = int_of_float hrs1 in
    let mins_int1 = int_of_float (60. *. frac hrs1) in
    
    let mins2 = int_of_string mins_part in
    let hrs_int2 = mins2 / 60 in
    let mins_int2 = mins2 mod 60 in 

    let total_hrs =  hrs_int1 + hrs_int2 + (mins_int1 + mins_int2) / 60 in
    let total_mins = (mins_int1 + mins_int2) mod 60 in

    Hm_type.HrsMins (total_hrs, total_mins)
  in
  match (split_str input_num_str (fun x -> x = 'h' || x = 'H') (fun x -> x = 'm' || x = 'M')) with
    | ("", "") ->
      (* A/P int *)

      (* 7:30A,7:30P,7A,7P *)
      (* if min above 59, invalid. why would you put 7:60A i hate you. *)
      let hour_adjust = if String.contains input_num_str 'P' then 12 else 0
      in
      (match (split_str input_num_str (fun x -> x = ':') (fun x -> x = 'A' || x = 'P')) with
        | ("", "") -> failwith "invalid time int! (1)"
        | (_, "") -> failwith "invalid time int! (2)"
        | ("", hr_str) ->
          let hr_int = int_of_string hr_str in
          Hm_type.HrsMins (hr_int + hour_adjust, 0)
        | (hr_str, min_str) ->
          let hrs_int = int_of_string hr_str in
          let mins_int = int_of_string min_str in
          if mins_int > 59 then
            failwith "Buddy ... Pal ..."
          else
            Hm_type.HrsMins (hrs_int + hour_adjust, mins_int)
      )
    | ("", mins_str) -> compute_time "0" mins_str
    | (hrs_str, "") -> compute_time hrs_str "0"
    | (hrs_str, mins_str) -> compute_time hrs_str mins_str
  

let hm_int_of_string_opt str =
  try Some (parse_hm_int str)
  with _ -> None

(* let _ = parse_hm_int "6h10m"
let _ = parse_hm_int "6h210m"
let _ = parse_hm_int "12345h210m"
let _ = parse_hm_int "10m6h"
let _ = parse_hm_int "210m6h"
let _ = parse_hm_int "6h"
let _ = parse_hm_int "10m"
let _ = parse_hm_int "60m"
let _ = parse_hm_int "70m"
let _ = parse_hm_int "74230m"
let _ = parse_hm_int "700h"
let _ = parse_hm_int "700.84123h"
let _ = parse_hm_int "7:30A"
let _ = parse_hm_int "7:30P"
let _ = parse_hm_int "7A"
let _ = parse_hm_int "7P" *)


(** returns x mod y but the result is always 0 or more *)
let modulo x y =
  let result = x mod y in
  if result >= 0 then result
  else result + y

let hm_int_to_mins = function
  | Hm_type.HrsMins (h, m) -> 60*h + m

let mins_to_hm_int m = Hm_type.HrsMins (m / 60, m mod 60)

(* hm_int_to_mins and mins_to_hm_int are invertible, in that
hm_int_to_mins mins_to_hm_int m = m  and
mins_to_hm_int hm_int_to_mins hm = hm  *)

let hm_int_apply_op hm1 op hm2 =
  let m1 = hm_int_to_mins hm1 in
  let m2 = hm_int_to_mins hm2 in
  let mins = op m1 m2 in
  mins_to_hm_int (modulo mins (24 * 60))

let hm_int_to_ap_str = function
  | Hm_type.HrsMins (h, m) ->
    let m_str = int_to_str_with_padding m in
    if h = 0 then "12:" ^ m_str ^ "A"
    else if h < 12 then string_of_int h ^ ":" ^ m_str ^ "A"
    else if h = 12 then "12:" ^ m_str ^ "P"
    else string_of_int (h - 12) ^ ":" ^ m_str ^ "P"

type cal_value =
  | IntVal of int
  | StringVal of string
  | HmIntVal of Hm_type.hm_int
  | HmIntListVal of Hm_type.hm_int list
  | FunctionVal of (Hm_type.hm_int list -> string)

(* allowed: + and -   ex:  5h + 10h   7h - 5.5h   5h + 10h + 4h - 6h *)

let eval_hm_int_exp exp =
  let clean_exp = exp |> str_to_char_list |> List.filter (fun x -> x <> ' ')  |> char_list_to_str  in
  let hm_ints = clean_exp |> 
                Str.split (Str.regexp "[+-]") |> 
                List.map (fun str -> match hm_int_of_string_opt str with Some hm -> hm | None -> failwith "malformed expr val")
  in
  let ops = clean_exp |> 
            str_to_char_list |> 
            List.filter (fun x -> x = '+' || x = '-') |> 
            List.map (fun op -> if op = '+' then ( + ) else ( - )) 
  in
  let rec do_eval cur_val operands operators = match operands with
    | [] -> cur_val
    | h :: t -> 
      let new_val = hm_int_apply_op (cur_val) (List.hd operators) h in
      do_eval new_val t (List.tl operators)
  in do_eval (List.hd hm_ints) (List.tl hm_ints) ops

(* let _ = eval_hm_int_exp "5h + 7h"
let _ = eval_hm_int_exp "7A - 9A" 
let _ = eval_hm_int_exp "7A - 9A + 6h12m"
let _ = eval_hm_int_exp "0A - 1m" *)

let parse_str_val_of_str_opt str =
  if String.starts_with ~prefix:"\"" str
    then
      if not (String.ends_with ~suffix:"\"" str) then None
      else Some (substring str 1 ((String.length str) - 2))
  else None

let parse_non_func_vals_opt val_str =
  match int_of_string_opt val_str with
      | Some i -> Some (IntVal i)
      | None -> (match hm_int_of_string_opt val_str with
        | Some hm_int -> Some (HmIntVal hm_int)
        | None ->
          if String.starts_with ~prefix:"[" val_str 
            then let hm_int_list_val = 
              val_str |> 
              String.split_on_char ',' |> 
              List.map (fun s -> s |>
                                  str_to_char_list |> 
                                  List.filter (fun c -> c <> '[' && c <> ']' && c <> ' ') |> 
                                  char_list_to_str) |>
              List.map (fun s -> match hm_int_of_string_opt s with Some hm_int -> hm_int | None -> failwith "malformed time int array")
            in Some (HmIntListVal hm_int_list_val)
          else match parse_str_val_of_str_opt val_str with
            | Some s -> Some (StringVal s)
            | None -> None
      )
      


let split_and_collect_matches str re =
  Str.full_split re str |> 
  List.map (function Str.Text s -> s | Str.Delim s -> s)
  
let parse_name_val name value =
  if String.starts_with ~prefix:"func" name
  then
    let func_def_chunks = String.split_on_char ' ' name in
    let func_name = List.nth func_def_chunks 1 in
    let func_params = list_slice 2 ((List.length func_def_chunks) - 1) func_def_chunks in
    let func_body_str = match parse_str_val_of_str_opt value with
      | Some str -> str
      | None -> failwith "malformed func string" 
    in
    (* matching when func params are in the body like ${t} or ${a - 5h} *)
    let re_arg_ref = Str.regexp "\\$\\({[^}]*}\\)" in
    let func_pieces = split_and_collect_matches func_body_str re_arg_ref in
    (* making parameters into simple names @0, @1, ... in the body *)
    let general_params = List.mapi (fun i param_name -> (param_name, "@" ^ string_of_int i)) func_params in
    let general_func_pieces = List.map (fun s -> 
      if String.starts_with ~prefix:"${" s && String.ends_with ~suffix:"}" s then
        List.fold_left 
        (fun acc (name, gen_name) -> Str.global_replace (Str.regexp name) gen_name acc) s general_params
      else s) func_pieces
    in
    let func_body_to_eval func_body_pieces hm_int_list =
      let values_to_fill = List.mapi (fun i x -> ("@" ^ string_of_int i, x)) hm_int_list in
      let filled_pieces = List.map (fun s -> 
        if String.starts_with ~prefix:"${" s && String.ends_with ~suffix:"}" s then
          let s_no_wrap = substring s 2 ((String.length s) - 2) in (* removes the beginning ${ and }*)
          let expr = List.fold_left 
          (fun acc (name, hm_int) -> Str.global_replace (Str.regexp name) (hm_int_to_ap_str hm_int) acc) s_no_wrap values_to_fill in
          hm_int_to_ap_str (eval_hm_int_exp expr)
        else s) func_body_pieces 
      in String.concat "" filled_pieces
    in  
    (func_name, FunctionVal (func_body_to_eval general_func_pieces))
  else
    match parse_non_func_vals_opt value with
    | Some v -> (name, v)
    | None -> failwith "unknown type"

(* let whats_up_dude = parse_name_val "func sup x y z" "\"So we have ${x + y} and also ${x - z} and ${x - 1h + y}\""
let _ = match whats_up_dude with
            | (_, FunctionVal j) -> j [Hm_type.HrsMins (1, 0); Hm_type.HrsMins (5, 12); Hm_type.HrsMins (17, 54)]
            | _ -> "whatever" *)


(* MAKE 4 28 * 10A 7P job_title * (desc !START) notify_before job_col *)

    (* MAKE month-int day-int year-int start-hm_int end-hm_int title-str loc-str desc-str remind-arr_hm_ints job_col-int *)
    (* for the JSON: 

      {'summary': '...',
      'location': '...',
      'description: '...',
      'start': { 'dateTime': 'YYYY-MM-DDTHH:MM:00', 'timeZone': 'America/New_York',  },
      'end': { 'dateTime': 'YYYY-MM-DDTHH:MM:00', 'timeZone': 'America/New_York',  },
      'reminders': { 'useDefault': False, 'overrides': [
              {'method': 'popup', 'minutes': int},
              {'method': 'popup', 'minutes': int} ] },
      'colorId': '5'
      },
       {...}, ...
    
    *)

type make_config = {
  month: int;
  day: int;
  year: int;
  start_time: Hm_type.hm_int;
  end_time: Hm_type.hm_int;
  title: string;
  location: string;
  description: string;
  reminders: int list;
  job_color_id: string;
}

let hm_int_to_24hr_fmt = function
  | Hm_type.HrsMins (h, m) ->
    let h_str = int_to_str_with_padding h in
    let m_str = int_to_str_with_padding m in
    h_str ^ ":" ^ m_str ^ ":" ^ "00"

let parse_make_to_json_str = function 
  | {month; day; year; start_time; end_time; title; location; description; reminders; job_color_id} ->
    let escape_quotes = Str.global_replace (Str.regexp "\"") "\\\"" in
    let key_val_str key_str val_str = "\"" ^ key_str ^ "\":\"" ^ escape_quotes val_str ^ "\"" in
    let key_val_obj_str key_str val_obj_str = "\"" ^ key_str ^ "\":" ^ val_obj_str in
    let title_str = key_val_str "summary" title in
    let location_str = key_val_str "location" location in
    let description_str = key_val_str "description" description in
    let fmt_time t = 
      string_of_int year ^
      "-" ^ 
      int_to_str_with_padding month ^ 
      "-" ^ 
      int_to_str_with_padding day ^
      "T" ^ 
      hm_int_to_24hr_fmt t
    in
    let start_time_fmt = "\"" ^ fmt_time start_time ^ "\"" in
    let end_time_fmt = "\"" ^ fmt_time end_time ^ "\"" in
    let start_time_str = key_val_obj_str "start" 
      ("{" ^ (key_val_obj_str "dateTime" start_time_fmt) ^ "," ^ (key_val_obj_str "timeZone" "\"America/New_York") ^ "\"}") 
    in
    let end_time_str = key_val_obj_str "end"
      ("{" ^ (key_val_obj_str "dateTime" end_time_fmt) ^ "," ^ (key_val_obj_str "timeZone" "\"America/New_York") ^ "\"}") 
    in
    let reminders_str = key_val_obj_str "reminders"
        ("{ \"useDefault\": false, \"overrides\": [" ^ 
        String.concat "" (List.map (fun rem -> "{\"method\":\"popup\",\"minutes\":" ^ string_of_int rem ^ "},") reminders)) 
    in
    (* removes last comma *)
    let reminders_str = substring reminders_str 0 ((String.length reminders_str) - 2) ^ "]}" in
    let job_col_str = key_val_obj_str "colorId" job_color_id in
    "{" ^ 
    title_str ^ "," ^
    location_str ^ "," ^
    description_str ^ "," ^
    start_time_str ^ "," ^
    end_time_str ^ "," ^
    reminders_str ^ "," ^
    job_col_str ^
    "}"

let split_preserve_quotes_and_parens str =
  Str.full_split (Str.regexp "\"[^\"]*\"\\|([^)]*)\\|[ ]+") str
  |> List.filter (function
       | Str.Delim " " -> false
       | Str.Text "" -> false
       | _ -> true)
  |> List.map (function
       | Str.Text s -> s
       | Str.Delim s -> s)

let () =
    let filename = Sys.argv.(1) in
    let file_contents = File_utils.read_file filename in
    let cleaned_lines = 
      file_contents |> String.split_on_char '\n'
                    |> List.map (fun x -> String.trim x)
                    |> List.filter (fun x -> String.length x <> 0)

                    (* remove comments *)
                    |> List.filter (fun x -> not (String.starts_with ~prefix:"#" x))
    in

    (* now to split the code between stuff that is before the "create {" and stuff afterward *)
    let index_of_create_keyword = match List.find_index (fun x -> x = "create {") cleaned_lines with
      | Some l -> l
      | None -> failwith "create block not found. must be precisely 'create {'"
    in

    let () = 
      if (List.nth cleaned_lines ((List.length cleaned_lines) - 1)) = "}" then ()
      else failwith "script must end with end of create block, '}'"
    in

    let var_defs = list_slice 0 (index_of_create_keyword - 1) cleaned_lines in
    let create_block_stmts = list_slice (index_of_create_keyword + 1) ((List.length cleaned_lines) - 2) cleaned_lines in

    let name_and_defs = 
      var_defs |>
      List.map (fun def_stmt ->
        let equal_index = match String.index_opt def_stmt '=' with
          | Some i -> i
          | None -> failwith "malformed declaration statement; no '=' present" 
        in
        let name = substring def_stmt 0 (equal_index - 1) in
        let value = substring def_stmt (equal_index + 1) (String.length def_stmt - 1) in
        (name, value)
      ) |> 
      List.map (fun (name, value) -> (String.trim name, String.trim value))
    in
    let var_store = List.map (fun (name, val_str) -> parse_name_val name val_str) name_and_defs in
    let make_stmt_pieces = List.map split_preserve_quotes_and_parens create_block_stmts in
    let make_configs = List.map
    (fun make_piece ->
      if (List.nth make_piece 0) <> "MAKE" then failwith "should be a 'MAKE' statement. that is all we support for now."
      else
        let uncover_value expr_str default_val =
          match parse_non_func_vals_opt expr_str with
          | Some e -> e
          | None -> 
            if expr_str = "*" then default_val
            else if String.starts_with ~prefix:"(" expr_str && String.ends_with ~suffix:")" expr_str then
              let func_call = substring expr_str 1 ((String.length expr_str) - 2) in
              let func_call_pieces = Str.split (Str.regexp " ") func_call in
              let func_name = List.hd func_call_pieces in
              let func_args = List.map parse_hm_int (List.tl func_call_pieces) in
              let (_, func_val) = List.find (fun (name, _) -> name = func_name) var_store in
              match func_val with
                | FunctionVal f -> StringVal (f func_args)
                | _ -> failwith "malformed function call"
            else
              match List.find_opt (fun (name, _) -> name = expr_str) var_store with
              | Some (_, cal_val) -> cal_val
              | None -> failwith ("unknown type at MAKE for expr " ^ expr_str)
        in
        (* MAKE month-int day-int year-int start-hm_int end-hm_int title-str loc-str desc-str remind-arr_hm_ints job_col-str *)
        let unconver_int index default = match uncover_value (List.nth make_piece index) (IntVal default) with
          | IntVal i -> i
          | _ -> failwith "month malformed"
        in
        let unconver_hm_int index default = match uncover_value (List.nth make_piece index) (HmIntVal default) with
          | HmIntVal hm_int -> hm_int
          | _ -> failwith "hm_int malformed"
        in
        let unconver_str index default = match uncover_value (List.nth make_piece index) (StringVal default) with
          | StringVal s -> s
          | _ -> failwith "str malformed"
        in
        let unconver_hm_int_list index default = match uncover_value (List.nth make_piece index) (HmIntListVal default) with
          | HmIntListVal hmi -> hmi
          | _ -> failwith "hm_int list malformed"
        in
        let month = unconver_int 1 Default_values.month in
        let day = unconver_int 2 Default_values.day in
        let year = unconver_int 3 Default_values.year in
        let start_time = unconver_hm_int 4 Default_values.start_time in
        let end_time = unconver_hm_int 5 Default_values.end_time in
        let title = unconver_str 6 Default_values.title in
        let location = unconver_str 7 Default_values.location in
        let description = unconver_str 8 Default_values.description in
        let reminders = List.map hm_int_to_mins (unconver_hm_int_list 9 Default_values.reminders) in
        let job_color_id = unconver_str 10 Default_values.job_color_id in
        { month; day; year; start_time; end_time; title; location; description; reminders; job_color_id; }
    )
    make_stmt_pieces
    in
    let json_strs = List.map parse_make_to_json_str make_configs in
    let result = "[" ^ (String.concat "," json_strs) ^ "]" in
    let () = File_utils.write_to_file (filename ^ ".json") result
in () 