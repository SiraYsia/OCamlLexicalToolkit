open TokenTypes
open Str

(* for each of the token types in the language we define regular expression*)
let re_posint = Str.regexp "[0-9]+";;
let re_negint = Str.regexp "(-[0-9]+)";;

let re_leftparen = Str.regexp "(" ;;
let re_rightparen = Str.regexp ")" ;;

let re_equal = Str.regexp "=" ;;
let re_notequal = Str.regexp "<>" ;;
let re_greater = Str.regexp ">" ;;
let re_less = Str.regexp "<" ;;
let re_greaterequal = Str.regexp ">=" ;;
let re_lessequal = Str.regexp "<=" ;;

let re_or = Str.regexp "||" ;;
let re_and = Str.regexp "&&" ;;
let re_not = Str.regexp "not" ;; 
let re_if = Str.regexp "if" ;; 
let re_then = Str.regexp "then" ;;  
let re_else = Str.regexp "else" ;;

let re_add = Str.regexp "\\+" ;;
let re_sub = Str.regexp "-" ;;
let re_mult = Str.regexp "\\*" ;;
let re_div = Str.regexp "/" ;;
let re_concat = Str.regexp "\\^" ;;

let re_let = Str.regexp "let" ;;
let re_rec = Str.regexp "rec" ;;
let re_in = Str.regexp "in" ;;
let re_def = Str.regexp "def" ;;
let re_fun = Str.regexp "fun" ;;

let re_arrow = Str.regexp "->" ;;
let re_true = Str.regexp "true" ;;
let re_false = Str.regexp "false" ;;

let re_string = Str.regexp "\"[^\"]*\"" ;;
let invalid_string = Str.regexp "\"[\"]*\"" ;;
let re_ident = Str.regexp "[a-zA-Z][a-zA-Z0-9]*";;
let re_idwithkey = Str.regexp "\\(true\\|false\\|fun\\|if\\|rec\\|else\\|let\\|in\\|def\\|then\\|not\\)[a-zA-Z0-9]+"

let re_doublesemi = Str.regexp ";;" ;;

(*match -> regex string acc *)

(*
  Use an accumalteor to keep track of the postion where our last match happened 
   have an index that  updates itself to so that the next tokenization
  starts from the position immediately after the previous one

*)

let tokenize input = 
  let rec helper acc length= 
    if acc >= length then []
    else if string_match (re_posint)input acc then
      (*extract the part that matches*)
      let token = Str.matched_string input in  
      (*add it to the Int token *)
      (*call helper on the rest by updating accumlator to start from the next string *)
      Tok_Int(int_of_string token) :: helper (acc + String.length token) length

    else if string_match (re_negint)input acc then
      (*extract the part that matches*)
      let token = Str.matched_string input in  
      
      (*Exclude the parethesis in both ends.
         last one not inclusive so -2*)
      Tok_Int(int_of_string (String.sub token 1 ((String.length token) -2)))  :: helper (acc + String.length token) length
   
      (*from the example on ReadMe *)
    else if string_match (invalid_string) input acc then 
      failwith "InvalidInputException"


    else if string_match (re_string)input acc then
      let token = Str.matched_string input in  
      Tok_String(String.sub token 1 ((String.length token) -2))  :: helper (acc + String.length token) length
   
  (******************************)

      (* giving id priority if it appears with key words *)
      else if (Str.string_match (re_idwithkey) input acc) then
        let token = Str.matched_string input in
        Tok_ID(token)::(helper (acc + String.length token) length)      
  

    else if (Str.string_match re_let input acc) then Tok_Let :: helper (3+acc) length
    else if (Str.string_match re_rec input acc) then Tok_Rec :: helper (3+acc) length
    else if (Str.string_match re_in input acc) then Tok_In :: helper (2+acc) length
   
    else if (Str.string_match re_def input acc) then Tok_Def :: helper (3+acc) length
    else if (Str.string_match re_fun input acc) then Tok_Fun :: helper (3+acc) length


    else if (Str.string_match re_or input acc) then Tok_Or :: helper (2+acc) length
    else if (Str.string_match re_and input acc) then Tok_And :: helper (2+acc) length
    else if (Str.string_match re_not input acc) then Tok_Not :: helper (3+acc) length
    else if (Str.string_match re_if input acc) then Tok_If :: helper (2+acc) length
   
    else if (Str.string_match re_then input acc) then Tok_Then :: helper (4+acc) length
    else if (Str.string_match re_else input acc) then Tok_Else :: helper (4+acc) length

    else if (Str.string_match re_true input acc) then Tok_Bool (true) :: helper (4+acc) length
    else if (Str.string_match re_false input acc) then Tok_Bool (false) :: helper (5+acc) length
    
    else if (Str.string_match (re_ident) input acc) then
      let token = Str.matched_string input in
      Tok_ID(token)::(helper (acc + String.length (token)) length)

    (***************************** *)


    else if (Str.string_match re_leftparen input acc) then Tok_LParen :: helper (1+acc) length
    else if (Str.string_match re_rightparen input acc) then Tok_RParen :: helper (1+acc) length
    
    else if (Str.string_match re_equal input acc) then Tok_Equal :: helper (1+acc) length
    else if (Str.string_match re_notequal input acc) then Tok_NotEqual :: helper (2+acc) length
    else if (Str.string_match re_greater input acc) then Tok_Greater :: helper (1+acc) length

    else if (Str.string_match re_less input acc) then Tok_Less ::helper (1+acc) length
    else if (Str.string_match re_greaterequal input acc) then Tok_GreaterEqual :: helper (2+acc) length
    else if (Str.string_match re_lessequal input acc) then Tok_LessEqual :: helper (2+acc) length
    

    else if (Str.string_match re_add input acc) then Tok_Add :: helper (1+acc) length
    else if (Str.string_match re_arrow input acc) then Tok_Arrow :: helper (2+acc) length
    else if (Str.string_match re_sub input acc) then Tok_Sub :: helper (1+acc) length
    else if (Str.string_match re_mult input acc) then Tok_Mult ::helper (1+acc) length
    else if (Str.string_match re_div input acc) then Tok_Div :: helper (1+acc) length
    else if (Str.string_match re_concat input acc) then Tok_Concat :: helper (1+acc) length
    else if (Str.string_match re_doublesemi input acc) then  Tok_DoubleSemi :: helper (2+acc) length
    else helper (acc + 1) length
  in
  helper 0 (String.length input);;