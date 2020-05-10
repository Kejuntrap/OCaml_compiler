type exp =
  |  Integer_ of int         (* リテラル *)
  |  Plus of exp * exp     (* 足し算 *)
  |  Times of exp * exp    (* かけ算 *)
  |  Minus of exp * exp     (* ひき算 *)
  |  Div of exp * exp    (*わり算 *)
  |  Boolean_ of bool   (* boolean *)
  |  If of exp * exp * exp   (* if文 *)
  |  Eq of exp * exp        (* 等しいというやつ？e1 = e2 *)
  |  Or of exp * exp      (* ここ論理演算 *)
  |  And of exp * exp     (* ここ論理演算 *)
  |  Gre of exp * exp   (* 大なり A>Bに対してtrueを返す *)
;;

(* 値の型　*)
type value =
  | Integer  of int          (* 整数の値 *)
  | Boolean of bool         (* 真理値の値 *)
;;

let rec eval1 e =
  match e with
  | Integer_(n) -> Integer(n)   (* 「整数のみが書いてある」という式の型という情報が与えられているので整数型に変更する *)
  | Boolean_(b) -> Boolean(b) 
  | Plus(e1,e2) -> 
    begin match(eval1 e1 , eval1 e2) with
    | (Integer(n1),Integer(n2)) -> Integer(n1+n2)
    | _ -> failwith "Integer values expected"
  end
  | Times(e1,e2) -> 
    begin match(eval1 e1 , eval1 e2) with
    | (Integer(n1),Integer(n2)) -> Integer(n1*n2)
    | _ -> failwith "Integer values expected"
  end
  | Minus(e1,e2) -> 
    begin match(eval1 e1 , eval1 e2) with
    | (Integer(n1),Integer(n2)) -> Integer(n1-n2)
    | _ -> failwith "Integer values expected"
  end
  | Div(e1,e2) -> 
    begin match(eval1 e1 , eval1 e2) with
    | (Integer(n1),Integer(n2)) -> 
      if n2 <> 0 then
        Integer(n1/n2)
      else
        failwith "Division_by_zero"
    | _ -> failwith "Integer values expected"
  end
  | Eq(e1,e2) ->
    begin match(eval1 e1 ,eval1 e2) with
    | (Integer(n1), Integer(n2)) -> Boolean(n1=n2)
    | (Boolean(b1), Boolean(b2)) -> Boolean(b1=b2)
    | _ -> failwith "wrong value at EQ"
  end
  | Gre(e1,e2) ->
    begin match (eval1 e1,eval1 e2) with
    | (Integer(n1) , Integer(n2)) -> Boolean(n1>n2)
    | _ -> failwith "worng value at GRE"
  end
  | Or(e1,e2) ->
    begin match (eval1 e1,eval1 e2) with
    | (Boolean(b1) , Boolean(b2)) -> 
      if b1 = false && b2 = false then
        Boolean(false)
      else
        Boolean(true)
    | _ -> failwith "worng value at OR"
  end 
  | And(e1,e2) ->
    begin match (eval1 e1,eval1 e2) with
    | (Boolean(b1) , Boolean(b2)) ->
      if b1 = true && b2 = true then
        Boolean(true)
      else
        Boolean(false)
    | _ -> failwith "worng value at AND"
  end 
  | If(e1,e2,e3) -> (* if(条件) 真のとき 偽の時 的な実装っぽい  excelのアレ *)
    begin match(eval1 e1) with
    | Boolean(true) -> eval1 e2
    | Boolean(false) -> eval1 e3
    | _ -> failwith "wrong value　at IF"
  end
;;


let res = eval1(Plus( Integer_ 6 , Integer_ 7));;
let res = eval1(Minus( Integer_ 6 , Integer_ 7));;
let res = eval1(Times( Integer_ 6 , Integer_ 7));;
let res = (Plus(Boolean_ true, Boolean_ true));;

let _ = eval1 (Integer_ 1);;
let _ = eval1 (Integer_ 11);;
let _ = eval1 (Plus (Integer_ 1, Plus (Integer_ 2, Integer_ 11)));;
let _ = eval1 (Times (Integer_ 1, Plus (Integer_ 2, Integer_ 11)));;
let _ = eval1 (If (Eq(Integer_ 2, Integer_ 11),
                   Times(Integer_ 1, Integer_ 2),
                   Times(Integer_ 1, Plus(Integer_ 2,Integer_ 3))));;
let _ = eval1 (Eq (Integer_ 1, Integer_ 1));;
let _ = eval1 (Eq (Integer_ 1, Integer_ 2));;
let _ = eval1 (Eq (Boolean_ true, Boolean_ true));;
let _ = eval1 (Eq (Boolean_ true, Boolean_ false));;

let _ = eval1(Gre(Integer_ 1, Integer_ 0));;
let _ = eval1(And(Boolean_ false,Boolean_ false));;
let _ = eval1(And(Boolean_ true,Boolean_ true));;
let _ = eval1(Or(Boolean_ true,Boolean_ false));;
let _ = eval1(Or(Boolean_ false,Boolean_ false));;
