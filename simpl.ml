type exp =
  |  Integer_ of int        (* リテラル *)
  |  Boolean_ of bool      (* 真理値 *)
  |  Plus of exp * exp     (* 足し算 *)
  |  Times of exp * exp    (* かけ算 *)
  |  Minus of exp * exp    (* ひき算 *)
  |  Div of exp * exp    (* 割り算 *)
  |  Eq of exp * exp  (* 等価か *)
  |  Var of string      (* 変数関係 *)   
  |  Let of string * exp * exp  (* 変数関係なのにどうしてVarと違うの…？ *) 
;;

let emptyenv() = [];;   (* 無意味な引数を受け取る *)
let extend env x v =(x,v) :: env ;;  (* 値の拡張をする感じの *)
let rec lookup x env =    (* 値を見つける関数 *)
  match env with
  | [] -> failwith ("unbound variable: " ^ x)
  | (y,v)::tl -> if x = y then v
    else lookup x tl
;;

(* 値の型　*)
type value =
  | Integer  of int          (* 整数の値 *)
  | Boolean  of bool         (* 真理値 *)
;;


(* 
let rec math e =
  let binop f e1 e2 =
    match (math e1,math e2) with
    | (Integer(n1),Integer(n2)) -> Integer(f n1 n2)
    | _ -> failwith "integer  , boolean are allowed"
  in match e with
  | Integer_(n) -> Integer(n)
  | Plus(e1,e2) -> binop (+) e1 e2
  | Times(e1,e2) -> binop ( * ) e1 e2
  | Minus(e1,e2) -> binop (-) e1 e2
  | Div(e1,Integer_(0)) -> failwith "zero div error"
  | Div(e1,e2) -> binop (/) e1 e2
  | _ -> failwith "unimplemented"
;;
*)

let rec math e =
  let binop f e1 e2=
    match (math e1 ,math e2 ) with
    | (Integer(n1),Integer(n2)) -> Integer(f n1 n2)
    | _ -> failwith "???"
  in match e with
  | Integer_(n) -> Integer(n)
  | Plus(e1,e2) -> binop (+) e1 e2
  | Times(e1,e2) -> binop ( * ) e1 e2
  | Minus(e1,e2) -> binop (-) e1 e2
  | Div(e1,Integer_(0)) -> failwith "zero div error"
  | Div(e1,e2) -> binop (/) e1 e2
  | _ -> failwith "unimplemented"
;;



let _ = math(Plus(Integer_ 7,Integer_ 7));;   (* ->  Integer 14 *)
let _ = math(Times(Integer_ 810,Integer_ 334));;  (* -> Integer 270510 *)
let _ = math(Minus(Integer_ 810,Integer_ 334));;  (* -> Integer 476 *)
let _ = math(Div(Integer_ 810,Integer_ 2));;  (* -> Integer 405 *)

(*

let po = emptyenv();;
(extend po "x" (Integer 1));;
let ans = lookup "x" po;;

*)

let po =  [("x",Integer 1); ("y",Integer 3); ("z", Integer 5)];;
let ans = lookup "x" po;;