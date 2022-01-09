(* Pomožni tip, ki predstavlja mrežo *)

type 'a grid = 'a Array.t Array.t


(* Funkcije za prikaz mreže.
   Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)

(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

let string_of_list string_of_element sep lst =
  lst |> List.map string_of_element |> String.concat sep
(*vrne nek string v katerem so med elementi lista vrinjeni stringi, npr, "1a2a3a4"*)


let string_of_nested_list string_of_element inner_sep outer_sep =
  string_of_list (string_of_list string_of_element inner_sep) outer_sep
(*najprej potlači med inner_sep elemente nek string, potem pa vsak dobljen string potlaci med outer_sep*)

(*Array is like a list of fixed length, and every element must be the same type.*)

let string_of_row string_of_cell row =
  let string_of_cells =
    row |> Array.to_list |> chunkify 3
    |> string_of_nested_list string_of_cell "" "│"
  in
  "┃" ^ string_of_cells ^ "┃\n"


(*string_of_row f [|1;2;3;4;5;6;7;8;9|];;
- : string = "┃123│456│789┃\n"*)

(*funkcija bo iz teh zgornjih vrstic(več teh vrstic pospravljenih v array array) iz vsakega arraya naredila stolpec*)
let print_grid string_of_cell grid =
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big

(* Funkcije za dostopanje do elementov mreže *)


(*[|[|1;2;3;4;5;6;7;8;9|]; [|3;4;5;6;7;8;9;1;2|];[|9;1;2;3;4;5;6;7;8|];[|4;5;6;7;8;9;1;2;3|];[|5;6;7;8;9;1;2;3;4|];[|8;9;1;2;3;4;5;6;7|];[|7;8;9;1;2;3;4;5;6|];[|3;4;5;6;7;8;9;1;2|];[|3;4;5;6;7;8;9;1;2|]|];;*)
let get_row (grid : 'a grid) (row_ind : int) = 
  grid.(row_ind)

let rows grid = List.init 9 (get_row grid)

let get_column (grid : 'a grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind))

let columns grid = List.init 9 (get_column grid)

let get_box (grid : 'a grid) (box_ind : int) = 
  Array.init 9 (fun some_ind -> grid.((box_ind - (box_ind mod 3)) + (some_ind/3)).((box_ind mod 3)*3 + (some_ind mod 3)))
  
let boxes grid = List.init 9 (get_box grid)

(* Funkcije za ustvarjanje novih mrež *)


(*cilj: ustavriti iz že danega grida, takega da bo na vsak element delovala funkcija*)
let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = 
  Array.init 9 (fun row_ind -> Array.map f (get_row grid row_ind))

let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid

let foldi_grid (f : int -> int -> 'a -> 'acc -> 'acc) (grid : 'a grid)
    (acc : 'acc) : 'acc =
  let acc, _ =
    Array.fold_left
      (fun (acc, row_ind) row ->
        let acc, _ =
          Array.fold_left
            (fun (acc, col_ind) cell ->
              (f row_ind col_ind cell acc, col_ind + 1))
            (acc, 0) row
        in
        (acc, row_ind + 1))
      (acc, 0) grid
  in
  acc

let row_of_string cell_of_char str =
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char

let grid_of_string cell_of_char str =
  let grid =
    str |> String.split_on_char '\n'
    |> List.map (row_of_string cell_of_char)
    |> List.filter (function [] -> false | _ -> true)
    |> List.map Array.of_list |> Array.of_list
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  grid
  
(* Model za vhodne probleme *)

type problem = { initial_grid : int option grid }

(*naš problem(sudoku) želimo z znanimi funkcijami zapisati v obliki grida, potrebujemo pomožno funkcijo 
da int spremenimo v string in pustimo prazen kvadratek, če int-a ni*)
let int_option_to_string = function
  |Some x -> string_of_int x
  |None -> " "

let print_problem (problem:problem) : unit = 
  print_grid int_option_to_string problem.initial_grid

let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0'))
    | _ -> None
  in
  { initial_grid = grid_of_string cell_of_char str }

(* Model za izhodne rešitve *)

type solution = int grid

let print_solution solution = print_grid string_of_int solution


(*funkcija da preverimo če vsebuje vsak array vsa st od 1 do 9*) 
let odstevanje_listov list1 list2 = 
  let rec odstevanje_listov_aux l1 l2 acc = match l1 with
  | [] -> acc
  | x::xs when (List.mem x l2) -> (odstevanje_listov_aux xs l2 acc)
  | x::xs -> (odstevanje_listov_aux xs l2 (x::acc)) 
in
  odstevanje_listov_aux list1 list2 []

let all_digit (array : 'a array) =
  let list =  (Array.to_list array) in
  if odstevanje_listov list [1; 2; 3; 4; 5; 6; 7; 8; 9] = [] then true else false


let int_option_to_int x = match x with
  |Some y -> y
  |None -> 0

(*funkcija pogleda če je problem res neka podmnožica rešitve*)
let problem_is_solution (problem:problem) (solution:int array array) = 
  let rec problem_is_solution_aux problem solution (i,j) = match (i,j) with 
    |(8,8) -> if (problem.initial_grid.(i).(j) = None || (int_option_to_int(problem.initial_grid.(i).(j)) = solution.(i).(j))) then true else false
    |(8,_) -> if (problem.initial_grid.(i).(j) = None || (int_option_to_int(problem.initial_grid.(i).(j)) = solution.(i).(j))) then true else false && (problem_is_solution_aux problem solution (0,(j+1)))
    |(i,_) -> if (problem.initial_grid.(i).(j) = None || (int_option_to_int(problem.initial_grid.(i).(j)) = solution.(i).(j))) then true else false && (problem_is_solution_aux problem solution ((i+1),j))
in
problem_is_solution_aux problem solution (0,0)
       


(*problem, rows solution so list array, potrebujem pomožno fun*)
let rec all_digit_array_list array_list  = match array_list with
    | [] -> true
    | x :: xs -> if all_digit x then all_digit_array_list xs else false
   

let is_valid_solution (problem:problem) solution = 
  all_digit_array_list(rows solution) && all_digit_array_list(columns solution) && all_digit_array_list(boxes solution) && (problem_is_solution problem solution)



