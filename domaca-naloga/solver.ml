type available = { loc : int * int; possible : int list }
(*tip možnosti*)

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { problem : Model.problem; current_grid : int option Model.grid; moznosti : available list }
(*tip stanja*)

(*print state pri katerem so prazna polja označena z ?*)
let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state
(*tip rešitev*)

(*DODANE FUNKCIJE*)
let change_none_to_zero = function
  |Some x -> x
  |None -> 0

let map_list array  = 
  let rec map_array_aux f list = match list with
  | [] -> []
  | x::xs -> (f x) :: map_array_aux f xs
in
  map_array_aux change_none_to_zero (Array.to_list array)


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

let int_option_to_int x = match x with
  |Some y -> y
  |_ -> failwith "None"


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
    | (0, 0) | (0, 1) | (0, 2) | (1, 0) | (1, 1) | (1, 2) | (2, 0) | (2, 1) | (2, 2) ->  (presek_listov (presek_listov(manjkajoca_st (Model.get_row grid i)) (manjkajoca_st (Model.get_column grid j))) (manjkajoca_st (Model.get_box grid 0))) 
    | (0, 3) | (0, 4) | (0, 5) | (1, 3) | (1, 4) | (1, 5) | (2, 3) | (2, 4) | (2, 5) ->  (presek_listov (presek_listov(manjkajoca_st (Model.get_row grid i)) (manjkajoca_st (Model.get_column grid j))) (manjkajoca_st (Model.get_box grid 1))) 
    | (0, 6) | (0, 7) | (0, 8) | (1, 6) | (1, 7) | (1, 8) | (2, 6) | (2, 7) | (2, 8) ->  (presek_listov (presek_listov(manjkajoca_st (Model.get_row grid i)) (manjkajoca_st (Model.get_column grid j))) (manjkajoca_st (Model.get_box grid 2))) 
    | (3, 0) | (3, 1) | (3, 2) | (4, 0) | (4, 1) | (4, 2) | (5, 0) | (5, 1) | (5, 2) ->  (presek_listov (presek_listov(manjkajoca_st (Model.get_row grid i)) (manjkajoca_st (Model.get_column grid j))) (manjkajoca_st (Model.get_box grid 3))) 
    | (3, 3) | (3, 4) | (3, 5) | (4, 3) | (4, 4) | (4, 5) | (5, 3) | (5, 4) | (5, 5) ->  (presek_listov (presek_listov(manjkajoca_st (Model.get_row grid i)) (manjkajoca_st (Model.get_column grid j))) (manjkajoca_st (Model.get_box grid 4))) 
    | (3, 6) | (3, 7) | (3, 8) | (4, 6) | (4, 7) | (4, 8) | (5, 6) | (5, 7) | (5, 8) ->  (presek_listov (presek_listov(manjkajoca_st (Model.get_row grid i)) (manjkajoca_st (Model.get_column grid j))) (manjkajoca_st (Model.get_box grid 5))) 
    | (6, 0) | (6, 1) | (6, 2) | (7, 0) | (7, 1) | (7, 2) | (8, 0) | (8, 1) | (8, 2) ->  (presek_listov (presek_listov(manjkajoca_st (Model.get_row grid i)) (manjkajoca_st (Model.get_column grid j))) (manjkajoca_st (Model.get_box grid 6))) 
    | (6, 3) | (6, 4) | (6, 5) | (7, 3) | (7, 4) | (7, 5) | (8, 3) | (8, 4) | (8, 5) ->  (presek_listov (presek_listov(manjkajoca_st (Model.get_row grid i)) (manjkajoca_st (Model.get_column grid j))) (manjkajoca_st (Model.get_box grid 7))) 
    | (6, 6) | (6, 7) | (6, 8) | (7, 6) | (7, 7) | (7, 8) | (8, 6) | (8, 7) | (8, 8) ->  (presek_listov (presek_listov(manjkajoca_st (Model.get_row grid i)) (manjkajoca_st (Model.get_column grid j))) (manjkajoca_st (Model.get_box grid 8))) 
    | _ -> failwith "ta par ne obstaja"


let urejeni_available_listi (list : available list) =
  let lenght x y = List.length x.possible - List.length y.possible in 
    List.sort lenght list

let vse_moznosti grid =
  let rec vse_moznosti_aux grid i j acc = match grid.(i).(j) with
    |None -> if j < 8 then vse_moznosti_aux grid i (j + 1) ({loc = (i,j); possible = vse_moznosti_na_gridu grid (i,j)} :: acc) else 
      if i < 8 then vse_moznosti_aux grid (i + 1) 0 ({loc = (i,j); possible = vse_moznosti_na_gridu grid (i,j)} :: acc) else acc
    |Some x-> if j < 8 then vse_moznosti_aux grid i (j + 1) acc else 
      if i < 8 then vse_moznosti_aux grid (i + 1) 0 acc else acc
  in 
  urejeni_available_listi(vse_moznosti_aux grid 0 0 [])

let eno_vrednost grid x (i, j) = 
  grid.(i).(j) <- Some x;
  grid

let resimo_enostavne (state : state) = 
  let trenuten_grid = Model.copy_grid state.current_grid in 
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
 
 
let initialize_state (problem : Model.problem) : state =
  { current_grid = Model.copy_grid problem.initial_grid; moznosti = (vse_moznosti problem.initial_grid ); problem = problem }

let nov_state (state : state) = 
      let rec ponavljamo_dokler_resuje f stanje = 
        if ((f stanje).current_grid = stanje.current_grid) then stanje
        else ponavljamo_dokler_resuje f (f stanje)
      in
      (ponavljamo_dokler_resuje resimo_enostavne state)


let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid
  in
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = Model.map_grid Option.get state.current_grid in
    if Model.is_valid_solution state.problem solution then Solved solution
    else Fail state


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
          current_grid = Model.copy_grid state.current_grid;
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

let solve_problem (problem : Model.problem) =
  problem |> initialize_state |> solve_state
