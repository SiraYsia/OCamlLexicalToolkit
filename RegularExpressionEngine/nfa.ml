open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

  

(****************)
(* Part 1: NFAs *)
(****************)

(*We need to traverse the set  of transitions(delta) to see if each starting state
 match what's in the delata and that way we can get the end state*)

let rec move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  let helper start symbol de_lst =
    List.fold_left (fun acc (f, m, n) -> if f = start && m = symbol then n :: acc else acc) [] de_lst
  in
  List.fold_left (fun acc q -> union (helper q s nfa.delta) acc) [] qs

  
let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =

  let rec helper e acc =
    if e = acc then acc
    else helper acc @@
      List.fold_left (fun acc (f, sym, n) ->
        if sym = None && not (List.mem n acc) && List.mem f acc then
          n :: acc
        else acc) acc nfa.delta
  in
  List.fold_left (fun acc closure -> if List.mem closure acc then acc else closure :: acc) [] @@
    helper [] qs




(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  let {sigma; _} = nfa in
    List.map (fun c -> e_closure nfa (move nfa qs (Some c))) sigma


let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  let {sigma; _} = nfa in
  List.map (fun c -> (qs, Some c, e_closure nfa (move nfa qs (Some c)))) sigma

    
let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  let rec helper final_states states = 
    match final_states with 
    |[] -> [] 
    |h :: t -> if (List.mem h states) then [qs] else (helper t states) 
  in
  helper nfa.fs qs

  let  remove_duplicates lst =
  let folder x acc =
  if List.mem x acc then acc else x::acc
 in
 List.fold_right folder lst []
 
 
 let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
  (work: 'q list list) : ('q list, 's) nfa_t =
   match work with
  | [] -> dfa(*we're done building the DFA *)
  | h :: t ->(*process one at a time *)
    (* find all the new states that can be reached from state h in one step, and filter out any duplicates, empty lists, or states that have already been processed. *)
  let find = List.filter (fun q -> q <> [] && not (List.mem q dfa.qs) && not (List.mem q work)) (new_states nfa h) in
    (* concatenate the new unprocessed states with the remaining work list, remove any duplicates, and assign to a new updated_list list. *)
    let updated_list = List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) t (find @ t) in

    let dfa_t =
      { qs = h :: dfa.qs;
        sigma = dfa.sigma;
        delta = new_trans nfa h @ dfa.delta;
        q0 = dfa.q0;
        fs = remove_duplicates (new_finals nfa h @ dfa.fs) }
    in
    (*calling it recurssively *)
    nfa_to_dfa_step nfa dfa_t updated_list
 
 
 let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let dfa_t =
    {
      qs = [];
      sigma =  nfa.sigma;
      delta = [];
      q0 = e_closure nfa [nfa.q0];
      fs = []
    } in nfa_to_dfa_step nfa dfa_t [dfa_t.q0];;
 
    
  let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
        let dfa = nfa_to_dfa nfa in
        let fold_func = (fun acc char -> move dfa acc (Some char)) in
        let chars = explode s in
        let final = List.fold_left fold_func [dfa.q0] chars in
        List.fold_left (fun acc x -> acc || List.mem x dfa.fs) false final      