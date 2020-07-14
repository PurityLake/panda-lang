open Tokenizer
open Printf

exception Invalid_statement of string * int
exception End

let stack_trace token_line e idx =
  let token = List.nth token_line.tokens idx in
  let col = token.col - 1 in
  printf "%s:\nError on line %d col %d\n" token_line.filename token.lineno token.col;
  printf "%s\n" token_line.line;
  for _ = 0 to col do
    printf " "
  done;
  for _ = 1 to String.length token.s do
    printf "^"
  done;
  printf " ";
  print_endline e

let rhs token_line i =
  let idx = ref 0 in
  try
    while true; do
      let _ = match (List.nth token_line.tokens (i + !idx)).ttype with
      | Operator(op) -> if op = ";" then raise (End) else printf "Op: %s\n" op
      | PString(s)   -> printf "String \"%s\"\n" s
      | Atom(a)      -> printf "Atom %s\n" a
      | Keyword(_)   -> raise (Invalid_statement("Can't assgin a keyword to a variable", i))
      | _            -> printf "" in
      idx := !idx + 1
    done
  with
    End -> printf ""

let letstatement token_line i =
  printf "let statement\n";
  let _ = 
    match (List.nth token_line.tokens (i + 1)).ttype with
    | Atom(name) ->
      printf "name = %s\n" name;
      name
    | Keyword(name)  -> raise (Invalid_statement("'" ^ name ^ "' is a keyword and cannot be assigned to", i + 1))
    | Operator(name) -> raise (Invalid_statement("'" ^ name ^ "' is an operator and cannot be assigned to", i + 1))
    | PString(_)     -> raise (Invalid_statement("Cannot assigned to a string", i + 1))
    | Start          -> raise (Invalid_statement("Start token is meant for beginning of list", i + 1))
  in
  rhs token_line (i + 3);
  print_endline "valid let statement"

let keyword token_line i key =
  match (List.nth token_line i).ttype with
  | Keyword(k) -> key = k
  | _          -> false

let expr token_line i =
  printf "expression\n";
  if keyword token_line.tokens i "let" then
    letstatement token_line i
  else
    print_endline "nope"


let parse token_line =
  printf "%s\n" token_line.line;
  try expr token_line 1
    with
      Invalid_statement(e, idx) -> stack_trace token_line e idx