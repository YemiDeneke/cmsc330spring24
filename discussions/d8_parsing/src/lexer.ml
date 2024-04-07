(* Type *)
type token =
| Tok_Int of int
| Tok_Mult
| Tok_Plus
| Tok_LParen
| Tok_RParen
| Tok_EOF

let string_of_token tok = match tok with
| Tok_Int(i) -> string_of_int i
| Tok_Mult -> "*"
| Tok_Plus -> "+"
| Tok_LParen -> "("
| Tok_RParen -> ")"
| Tok_EOF -> ""

let rec string_of_list conv lst = 
match lst with
| [] -> ""
| h::[] -> conv h
| h::t -> (conv h) ^ " " ^ (string_of_list conv t)

let strint = Str.regexp "[0-9]+"
(* Given source code returns a token list. *)
let rec lexer (input : string) : token list = 
  let rec tok pos s = 
    if pos >= (String.length s) then [Tok_EOF] 
    else if (Str.string_match (Str.regexp "*") s pos) then 
      Tok_Mult::(tok (pos + 1) s)
    else if (Str.string_match strint s pos) then
      let matched = Str.matched_string s in
      (Tok_int (int_of_string matched))::(tok (pos + 1) s)
    else if Str.string_match (Str.regexp "+") s pos then
      Tok_Add::(tok (pos+1) s
    else some error
