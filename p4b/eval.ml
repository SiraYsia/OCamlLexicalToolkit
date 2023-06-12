open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions
  Helpful for creating an environment. You can do this with references 
  (not taught) or without. 
  You do not have to use these and you can modify them if you want. 
  If you do not use references, you will need the following data type:
*)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(*let extend env x v = (x,v)::env *)

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(*let rec lookup env x = 
  match env with
  [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then value else lookup t x*)

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Removes the most recent variable,value binding from the environment *)
(*let rec remove env x = match env with
  | [] -> []
  | (var,value)::t -> if x = var then t else (var,value)::(remove t x);*)

  
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = match e with 
  |Value v -> v
  |ID id -> lookup env id
  |Fun (arg, body) -> Closure (env, arg, body)
  | Not x ->
    let value = match eval_expr env x with
      | Bool false -> Bool true
      | Bool true -> Bool false
      | _ -> raise (TypeError "Expected type bool") in
    value
  
  | If (e1, e2, e3) -> let result =match (eval_expr env e1) with
                               | Bool true -> (eval_expr env e2)
                               | Bool false -> (eval_expr env e3) 
                               | _ -> raise(TypeError "Expected type bool") in result

  | Let (id, false, init_expr, body_expr) ->
    let init_value = eval_expr env init_expr in
    let extended_env = extend env id init_value in
    eval_expr extended_env body_expr
  |Let (id, true, init_expr, body_expr) -> 
    let e2 = extend_tmp env id in
    let e1 = eval_expr e2 init_expr in
    let _ = (update e2 id e1) in
    eval_expr e2 body_expr  
  | FunctionCall (fn_expr, arg_expr) ->
    let fn_val = eval_expr env fn_expr in
    let arg_val = eval_expr env arg_expr in
    (match fn_val with
      | Closure (env', arg_id, body_expr) ->
          let new_env = extend env' arg_id arg_val in
          eval_expr new_env body_expr
      | _ -> raise (TypeError "Not a function"))
  |Binop (op, e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    match (op, v1, v2) with
    | (Add, Int x, Int y) -> Int (x + y)
    | (Sub, Int x, Int y) -> Int (x - y)
    | (Mult, Int x, Int y) -> Int (x * y)
    | (Div, Int x, Int y) ->
        if y = 0 then raise (DivByZeroError)
        else Int (x / y)
    | (Greater, Int x, Int y) -> Bool (x > y)
    | (GreaterEqual, Int x, Int y) -> Bool (x >= y)
    | (Less, Int x, Int y) -> Bool (x < y)
    | (LessEqual, Int x, Int y) -> Bool (x <= y)
    | (Concat, String x, String y) -> String (x ^ y)
    | (And, Bool x, Bool y) -> Bool (x && y)
    | (Or, Bool x, Bool y) -> Bool (x || y)
    | (Equal, _, _) | (NotEqual, _, _) ->
        eval_relational_op op v1 v2
    | _ -> raise (TypeError "Invalid operands for binary operator")
  

and eval_relational_op op v1 v2 = match (v1, v2) with
    | (Int x, Int y) ->
        Bool (match op with
              | Equal -> x = y
              | NotEqual -> x <> y
              | Greater -> x > y
              | GreaterEqual -> x >= y
              | Less -> x < y
              | LessEqual -> x <= y
              |_ -> raise (TypeError "Invalid operands for relational operator"))
    | (String x, String y) ->
        Bool (match op with
              | Equal -> x = y
              | NotEqual -> x <> y
              | Greater -> x > y
              | GreaterEqual -> x >= y
              | Less -> x < y
              | LessEqual -> x <= y
              | _ -> raise (TypeError "Invalid operands for relational operator"))
    | (Bool x, Bool y) ->
        Bool (match op with
              | Equal -> x = y
              | NotEqual -> x <> y
              | _ -> raise (TypeError "Invalid operands for relational operator"))
    | _ -> raise (TypeError "Invalid operands for relational operator")

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m =  match m with
  |Def (name, expr) ->
    let temp_env = extend_tmp env name in
    let value = eval_expr temp_env expr in
    let _ = update temp_env name value in
    (temp_env, Some value)
  |Expr expr ->
    let value = eval_expr env expr in
    (env, Some value)
  |NoOp -> (env, None)