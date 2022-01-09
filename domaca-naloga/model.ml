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



(*################################################################SOLVER##############################################################################*)
(*
type available = { loc : int * int; possible : int list } 

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)

type state = { problem : problem; current_grid : int option grid;  moznosti : available list}


let print_state (state : state) : unit =
  print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of solution | Unsolved of state | Fail of state

(*DODANE FUNKCIJE*)
let int_option_to_ind = function
  |Some x -> x
  |None -> 0

let map_list array  = 
  let rec map_array_aux f list acc = match list with
  | [] -> acc
  | x::xs -> map_array_aux f xs ((f x)::acc)
in
  map_array_aux int_option_to_ind (Array.to_list array) []


let odstevanje_listov list1 list2 = 
  let rec odstevanje_listov_aux l1 l2 acc = match l1 with
  | [] -> acc
  | x::xs when (List.mem x l2) -> (odstevanje_listov_aux xs l2 acc)
  | x::xs -> (odstevanje_listov_aux xs l2 (x::acc)) 
in
  odstevanje_listov_aux list1 list2 []

let rec list_without_zero list = match list with
  | [] -> []
  | x::xs -> if (x = 0) then list_without_zero xs else x :: (list_without_zero xs)


(*dobila bom vsa št ki manjkajo v nekem arrayu, pospravljena v int listu*)
let manjkajoca_st array = match (List.sort compare (map_list array)) with
  | [1; 2; 3; 4; 5; 6; 7; 8; 9] -> []
  | _ -> odstevanje_listov [1; 2; 3; 4; 5; 6; 7; 8; 9] (list_without_zero(map_list array))


  let presek_listov list1 list2 =
    let rec presek_listov_aux l1 l2 acc = match l1 with
      | [] -> acc
      | x::xs when (List.mem x l2) -> presek_listov_aux xs l2 (x::acc)
      | x::xs -> presek_listov_aux xs l2 acc
  in
    presek_listov_aux list1 list2 []
  
  
  let vse_moznosti_na_gridu grid (i, j) = match (i, j) with
    | (0, 0) | (0, 1) | (0, 2) | (1, 0) | (1, 1) | (1, 2) | (2, 0) | (2, 1) | (2, 2) ->  (presek_listov (presek_listov(manjkajoca_st (get_row grid i)) (manjkajoca_st (get_column grid j))) (manjkajoca_st (get_box grid 0))) 
    | (0, 3) | (0, 4) | (0, 5) | (1, 3) | (1, 4) | (1, 5) | (2, 3) | (2, 4) | (2, 5) ->  (presek_listov (presek_listov(manjkajoca_st (get_row grid i)) (manjkajoca_st (get_column grid j))) (manjkajoca_st (get_box grid 1))) 
    | (0, 6) | (0, 7) | (0, 8) | (1, 6) | (1, 7) | (1, 8) | (2, 6) | (2, 7) | (2, 8) ->  (presek_listov (presek_listov(manjkajoca_st (get_row grid i)) (manjkajoca_st (get_column grid j))) (manjkajoca_st (get_box grid 2))) 
    | (3, 0) | (3, 1) | (3, 2) | (4, 0) | (4, 1) | (4, 2) | (5, 0) | (5, 1) | (5, 2) ->  (presek_listov (presek_listov(manjkajoca_st (get_row grid i)) (manjkajoca_st (get_column grid j))) (manjkajoca_st (get_box grid 3))) 
    | (3, 3) | (3, 4) | (3, 5) | (4, 3) | (4, 4) | (4, 5) | (5, 3) | (5, 4) | (5, 5) ->  (presek_listov (presek_listov(manjkajoca_st (get_row grid i)) (manjkajoca_st (get_column grid j))) (manjkajoca_st (get_box grid 4))) 
    | (3, 6) | (3, 7) | (3, 8) | (4, 6) | (4, 7) | (4, 8) | (5, 6) | (5, 7) | (5, 8) ->  (presek_listov (presek_listov(manjkajoca_st (get_row grid i)) (manjkajoca_st (get_column grid j))) (manjkajoca_st (get_box grid 5))) 
    | (6, 0) | (6, 1) | (6, 2) | (7, 0) | (7, 1) | (7, 2) | (8, 0) | (8, 1) | (8, 2) ->  (presek_listov (presek_listov(manjkajoca_st (get_row grid i)) (manjkajoca_st (get_column grid j))) (manjkajoca_st (get_box grid 6))) 
    | (6, 3) | (6, 4) | (6, 5) | (7, 3) | (7, 4) | (7, 5) | (8, 3) | (8, 4) | (8, 5) ->  (presek_listov (presek_listov(manjkajoca_st (get_row grid i)) (manjkajoca_st (get_column grid j))) (manjkajoca_st (get_box grid 7))) 
    | (6, 6) | (6, 7) | (6, 8) | (7, 6) | (7, 7) | (7, 8) | (8, 6) | (8, 7) | (8, 8) ->  (presek_listov (presek_listov(manjkajoca_st (get_row grid i)) (manjkajoca_st (get_column grid j))) (manjkajoca_st (get_box grid 8))) 
    | _ -> failwith "ta par ne obstaja"
(* funkcije za initialize_state *)

let urejeni_available_listi (list : available list) =
  let lenght x y = List.length x.possible - List.length y.possible in 
    List.sort lenght list


  (*dobili bomo list "parov" urejenih možnosti na nekem indeksu; za hitrejše delovanje jih sortiramo po dolžini*)
let vse_moznosti grid =
  let rec vse_moznosti_aux grid i j acc = match grid.(i).(j) with
    |None -> if (i,j) = (8,8) then ({loc = (i,j); possible = vse_moznosti_na_gridu grid (i,j)} :: acc) 
      else 
        if j < 8 then vse_moznosti_aux grid i (j + 1) ({loc = (i,j); possible = vse_moznosti_na_gridu grid (i,j)} :: acc) else 
        if i < 8 then vse_moznosti_aux grid (i + 1) 0 ({loc = (i,j); possible = vse_moznosti_na_gridu grid (i,j)} :: acc) else acc
    |Some x-> if (i,j) = (8,8) then acc 
      else 
        if j < 8 then vse_moznosti_aux grid i (j + 1) acc else 
        if i < 8 then vse_moznosti_aux grid (i + 1) 0 acc else acc
  in 
  urejeni_available_listi(vse_moznosti_aux grid 0 0 [])

let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid
  in
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = map_grid Option.get state.current_grid in
    if is_valid_solution state.problem solution then Solved solution
    else Fail state

let eno_vrednost grid x (i, j) = 
  let model_grid = copy_grid grid in 
    model_grid.(i).(j) <- Some x;
      model_grid

(*funkcija bo rešila le celice kjer je natanko ena možnost*)

let resimo_enostavne (state : state) = 
  let trenuten_grid = copy_grid state.current_grid in 
  let moznosti = (vse_moznosti trenuten_grid) in
    let rec aux moznosti acc = match moznosti with
      |[] -> acc
      |x::xs ->
        match x.possible with 
        |[y] -> let (i,j) = x.loc in 
          aux xs (eno_vrednost acc y (i, j)) 
        |_-> aux xs acc
in
  let novejsi_grid = aux moznosti trenuten_grid in
  (*napisimo sedaj novo stanje*)
  {current_grid = novejsi_grid ; moznosti = (vse_moznosti novejsi_grid); problem = state.problem}

let initialize_state (problem : problem) : state =
  { current_grid = copy_grid problem.initial_grid; moznosti = (vse_moznosti problem.initial_grid ); problem = problem } 
 
(*funkcijo ki resuje enostavne uporabimo tolikokrat da ne rši več nobene celice*)
let nov_state (state : state) = 
      let rec ponavljamo_dokler_resuje f stanje = 
        if ((f stanje).current_grid = stanje.current_grid) then stanje
        else ponavljamo_dokler_resuje f (f stanje)
      in
      (ponavljamo_dokler_resuje resimo_enostavne state)


let branch_state (state : state) : (state * state) option =
  (* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
     se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
     v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
     Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
     za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. *)
  match state.moznosti with 
  |[] -> failwith "None"
  |x::xs -> 
    match x.possible with 
    |[] -> None 
    |y::ys -> 
      let (i,j) = x.loc in
      let nov_grid = (eno_vrednost state.current_grid y x.loc ) in
        Some ({problem = state.problem;
          current_grid = nov_grid;
          moznosti = vse_moznosti nov_grid},

          (nov_state {problem = state.problem;
          current_grid = copy_grid state.current_grid;
          moznosti = {possible = ys; loc = (i,j)} :: xs}
          ))
    

(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *)
  match validate_state state with
  | Solved solution ->
      (* če smo našli rešitev, končamo *)
      Some solution
  | Fail fail ->
      (* prav tako končamo, če smo odkrili, da rešitev ni *)
      None
  | Unsolved state' ->
      (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
      explore_state state'

and explore_state (state : state) =
  (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
  match branch_state state with
  | None ->
      (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
      None
  | Some (st1, st2) -> (
      (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
      match solve_state st1 with
      | Some solution ->
          (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
          Some solution
      | None ->
          (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
          solve_state st2 )

let solve_problem (problem : problem) =
  problem |> initialize_state |> solve_state

*)

(*##################################################################KRNEKEJ###################################################################################################*)
(*
let primer2_neresen = [|
[|Some 2 ; None ; None ; None ; Some 8 ; None ; Some 3 ; None ;  None |];
[|None ; Some 6 ; None ; None ; Some 7; None ; None ; Some 8 ; Some 4 |];
[|None ; Some 3 ; None ; Some 5 ; None ; None ; Some 2 ; None ; Some 9 |];
[|None ; None ; None ; Some 1 ; None ; Some 5 ; Some 4 ; None ;Some 8 |];
[|None ; None ; None ; None; None ; None ; None; None ; None |];
[|Some 4 ; None ; Some 2 ; Some 7 ; None ; Some 6 ; None; None ; None |];
[|Some 3 ; None ; Some 1 ; None ; None ; Some 7 ; None ; Some 4 ;None |];
[|Some 7 ; Some 2 ; None ; None ; Some 4 ; None ; None ; Some 6 ;None |];
[|None ; None ; Some 4 ; None ; Some 1 ; None ; None ; None ;Some 3 |]|]
let primer2_problem = {initial_grid = primer2_neresen}
(*
let primer2_state = {
  problem = primer2_problem;
  current_grid = primer2_neresen;
  moznosti = vse_moznosti primer2_neresen
}*)

let primer2resen_int =  [|[|2; 4; 5; 9; 8; 1; 3; 7; 6|]; [|1; 6; 9; 2; 7; 3; 5; 8; 4|];
[|8; 3; 7; 5; 6; 4; 2; 1; 9|]; [|9; 7; 6; 1; 2; 5; 4; 3; 8|];
[|5; 1; 3; 4; 9; 8; 6; 2; 7|]; [|4; 8; 2; 7; 3; 6; 9; 5; 1|];
[|3; 9; 1; 6; 5; 7; 8; 4; 2|]; [|7; 2; 8; 3; 4; 9; 1; 6; 5|];
[|6; 5; 4; 8; 1; 2; 7; 9; 3|]|]
let primer1_neresen_int = 
  [|
    [| 4; 8; 3; 9; 2; 1; 6; 5; 7|];
    [| 9; 6; 7; 3; 0; 5; 8; 2; 1|];
    [| 2; 5; 1; 8; 7; 6; 4; 9; 3|];
    [| 5; 4; 8; 1; 3; 2; 9; 7; 6|];
    [| 7; 2; 9; 0; 6; 4; 0; 3; 8|];
    [| 1; 3; 6; 7; 9; 8; 0; 4; 5|];
    [| 3; 7; 2; 6; 8; 9; 5; 1; 4|];
    [| 8; 1; 4; 2; 5; 3; 7; 6; 9|];
    [| 6; 9; 5; 4; 1; 7; 3; 8; 2|]
  |]

  let primer1_resen = 
    [|
      [|Some 4; Some 8; Some 3; Some 9; Some 2; Some 1; Some 6; Some 5; Some 7|];
      [|Some 9; Some 6; Some 7; Some 3; Some 4; Some 5; Some 8; Some 2; Some 1|];
      [|Some 2; Some 5; Some 1; Some 8; Some 7; Some 6; Some 4; Some 9; Some 3|];
      [|Some 5; Some 4; Some 8; Some 1; Some 3; Some 2; Some 9; Some 7; Some 6|];
      [|Some 7; Some 2; Some 9; Some 5; Some 6; Some 4; Some 1; Some 3; Some 8|];
      [|Some 1; Some 3; Some 6; Some 7; Some 9; Some 8; Some 2; Some 4; Some 5|];
      [|Some 3; Some 7; Some 2; Some 6; Some 8; Some 9; Some 5; Some 1; Some 4|];
      [|Some 8; Some 1; Some 4; Some 2; Some 5; Some 3; Some 7; Some 6; Some 9|];
      [|Some 6; Some 9; Some 5; Some 4; Some 1; Some 7; Some 3; Some 8; Some 2|]
      |]

  
let primer1resen_int = 
  [|
    [| 4;  8;  3;  9;  2;  1;  6;  5;  7|];
    [| 9;  6;  7;  3;  4;  5;  8;  2;  1|];
    [| 2;  5;  1;  8;  7;  6;  4;  9;  3|];
    [| 5;  4;  8;  1;  3;  2;  9;  7;  6|];
    [| 7;  2;  9;  5;  6;  4;  1;  3;  8|];
    [| 1;  3;  6;  7;  9;  8;  2;  4;  5|];
    [| 3;  7;  2;  6;  8;  9;  5;  1;  4|];
    [| 8;  1;  4;  2;  5;  3;  7;  6;  9|];
    [| 6;  9;  5;  4;  1;  7;  3;  8;  2|]
    |]
let primer1_neresen = [|
[|Some 4; Some 8; Some 3; Some 9; Some 2; Some 1; Some 6; Some 5;  Some 7|];
[|Some 9; Some 6; Some 7; Some 3; None; Some 5; Some 8; Some 2; Some 1|];
[|Some 2; Some 5; Some 1; Some 8; Some 7; Some 6; Some 4; Some 9; Some 3|];
[|Some 5; Some 4; Some 8; Some 1; Some 3; Some 2; Some 9; Some 7;Some 6|];
[|Some 7; Some 2; Some 9; None; Some 6; Some 4; None; Some 3; Some 8|];
[|Some 1; Some 3; Some 6; Some 7; Some 9; Some 8; None; Some 4; Some 5|];
[|Some 3; Some 7; Some 2; Some 6; Some 8; Some 9; Some 5; Some 1;Some 4|];
[|Some 8; Some 1; Some 4; Some 2; Some 5; Some 3; Some 7; Some 6;Some 9|];
[|Some 6; Some 9; Some 5; Some 4; Some 1; Some 7; Some 3; Some 8;Some 2|]|]   let primer_1_problem = {initial_grid = primer1_neresen}
*) 

(*
let primer_3_neresen = [|
  [|None;None;Some 1; Some 9; None; None ; None; None ; Some 3|]; 
[|Some 9; None; None; Some 7; None; None; Some 1; Some 6; None|];
[|None; Some 3; None; None; None; Some 5; None; None; Some 7|];
[|None; Some 5; None; None; None; None; None; None; Some 9|];
[|None; None; Some 4; Some 3; None; Some 2; Some 6; None; None|];
[|Some 2; None; None; None; None; None; None; Some 7; None|];
[|Some 6; None; None; Some 1; None; None; None; Some 3; None|];
[|None; Some 4; Some 2; None; None; Some 7; None; None; Some 6|];
[|Some 5; None; None; None; None; Some 6; Some 8; None; None|];
|]

let primer_3_problem ={initial_grid = primer_3_neresen}
let primer_3_state = {
  problem = primer_3_problem;
  current_grid = primer_3_neresen;
  moznosti = vse_moznosti primer_3_neresen
}

let primer_3_resen_int = [|
  [|None;None;Some 1; Some 9; None; None ; None; None ; Some 3|]; 
[|Some 9; None; None; Some 7; None; None; Some 1; Some 6; None|];
[|None; Some 3; None; None; None; Some 5; None; None; Some 7|];
[|None; Some 5; None; None; None; None; None; None; Some 9|];
[|None; None; Some 4; Some 3; None; Some 2; Some 6; None; None|];
[|Some 2; None; None; None; None; None; None; Some 7; None|];
[|Some 6; None; None; Some 1; None; None; None; Some 3; None|];
[|None; Some 4; Some 2; None; None; Some 7; None; None; Some 6|];
[|Some 5; None; None; None; None; Some 6; Some 8; None; None|];
|]

let primer_3_solution = [|
  [|7;6;1;9;2;8;4;5;3|];
  [|9;2;5;7;4;3;1;6;9|];
  [|4;3;8;6;1;5;9;2;7|];
  [|3;5;7;4;6;1;2;8;9|];
  [|8;9;4;3;7;2;6;1;5|];
  [|2;1;6;5;8;9;3;7;4|];
  [|6;8;9;1;5;4;7;3;2|];
  [|1;4;2;8;3;7;5;9;6|];
  [|5;7;3;2;9;6;8;4;1|]
|]
*)