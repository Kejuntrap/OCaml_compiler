type exp =
  |  Integer_ of int        (* リテラル *)
  |  Boolean_ of bool
  |  Plus of exp * exp     (* 足し算 *)
  |  Times of exp * exp    (* かけ算 *)
  |  Minus of exp * exp    (* ひき算 *)
  |  Div of exp * exp    (* 割り算 *)
  |  Eq of exp * exp (* 等価か *)
;;

(* 値の型　*)
type value =
  | Integer  of int          (* 整数の値 *)
  | Boolean of bool
;;

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
  | _ -> failwith "???"
;;

let rec lop e = 
  let ope f e1 e2 =
    match (lop e1,lop e2) with
    | (Boolean(n1) , Boolean(n2)) -> Boolean(f n1 n2)
    | (Integer(n1) , Integer(n2)) -> Boolean(f n1 n2)
    | _ -> failwith "illegal type argument"
  in match e with
  | Integer_(n) -> Integer(n)
  | Boolean_(n) -> Boolean(n)
  | Eq(e1,e2) -> ope (=) e1 e2
  | _ -> failwith "???"
;;

let _ = math(Plus(Integer_ 7,Integer_ 7));;   (* ->  Integer 14 *)
let _ = math(Times(Integer_ 810,Integer_ 334));;  (* -> Integer 270510 *)
let _ = math(Minus(Integer_ 810,Integer_ 334));;  (* -> Integer 476 *)
(* let _ = math(Div(Integer_ 810,Integer_ 0));;   -> Error *)
let _ = math(Div(Integer_ 810,Integer_ 2));;  (* -> Error *)
let _ = lop(Eq(Integer_ 810,Integer_ 2));;  (* -> Error *)
(*
let rec calc e =
  match e with
  | Integer_(n) -> Integer(n)
  | Boolean_(b) -> Boolean(b)
  | Plus(e1,e2) -> binop 0 e1 e2
  | Minus(e1,e2) -> binop 1 e1 e2
  | Times(e1,e2) -> binop 2 e1 e2
  | Div(e1,e2) -> binop 3 e1 e2
  | And(e1,e2) -> binop 4 e1 e2
  | Or(e1,e2) -> binop 5 e1 e2
  | Gre(e1,e2) -> binop 6 e1 e2
  | Eq(e1,e2) -> binop 7 e1 e2 
  | _ -> failwith "unknown operation"
  and binop flag e1 e2 =
    match(calc e1,calc e2) with 
    |(Integer(n1),Integer(n2)) ->
    if flag = 0 then Integer(n1+n2)
    else if flag = 1 then Integer(n1-n2) 
    else if flag = 2 then Integer(n1*n2) 
    else if flag = 3 then Integer(n1/n2)
      if e2 = 0 then
        failwith "Division_by_zero"
      else
        Integer(n1/n2)
    | _ -> failwith "e"
    else if flag = 4 then
       begin match (calc e1,calc e2) with
      | (Boolean(b1) , Boolean(b2)) ->
        if b1 = true && b2 = true then
          Boolean(true)
        else
          Boolean(false)
      | _ -> failwith "worng value at AND"
    end 
    else if flag = 5 then
      begin match (calc e1,calc e2) with
      | (Boolean(b1) , Boolean(b2)) -> 
        if b1 = false && b2 = false then
          Boolean(false)
        else
          Boolean(true)
      | _ -> failwith "worng value at OR"
    end 
    else if flag = 6 then
      begin match (calc e1,eval1 e2) with
      | (Integer(n1) , Integer(n2)) -> Boolean(n1>n2)
      | _ -> failwith "worng value at GRE"
    end
    else if flag = 7 then
      begin match(calc e1 ,calc e2) with
      | (Integer(n1), Integer(n2)) -> Boolean(n1=n2)
      | (Boolean(b1), Boolean(b2)) -> Boolean(b1=b2)
      | _ -> failwith "worng value at EQ"
    end

*)


(*
let res = calc(Plus( Integer_ 6 , Integer_ 7));;
let res = calc(Minus( Integer_ 6 , Integer_ 7));;
let res = calc(Times( Integer_ 6 , Integer_ 7));;
let res = (Plus(Boolean_ true, Boolean_ true));;

let _ = calc (Integer_ 1);;
let _ = calc (Integer_ 11);;
let _ = calc (Plus (Integer_ 1, Plus (Integer_ 2, Integer_ 11)));;
let _ = calc (Times (Integer_ 1, Plus (Integer_ 2, Integer_ 11)));;
let _ = eval1 (If (Eq(Integer_ 2, Integer_ 11),
                   Times(Integer_ 1, Integer_ 2),
                   Times(Integer_ 1, Plus(Integer_ 2,Integer_ 3))));;
let _ = calc (Eq (Integer_ 1, Integer_ 1));;
let _ = calc (Eq (Integer_ 1, Integer_ 2));;
let _ = calc (Eq (Boolean_ true, Boolean_ true));;
let _ = calc (Eq (Boolean_ true, Boolean_ false));;

let _ = calc(Gre(Integer_ 1, Integer_ 0));;
let _ = calc(And(Boolean_ false,Boolean_ false));;
let _ = calc(And(Boolean_ true,Boolean_ true));;
let _ = calc(Or(Boolean_ true,Boolean_ false));;
let _ = calc(Or(Boolean_ false,Boolean_ false));;


OCamlではifは式なのでelseまで書いてあげないとダメみたい
*)