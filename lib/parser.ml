open Tokenizer
open Printf

exception Invalid_statement of string * int
exception End

let stack_trace token_line e idx =
  let token = List.nth token_line.tokens idx in
  let col = token.col - 1 in
  print_endline "";
  printf "%c[1m%c[33m%s:\nError on line %d col %d%c[0m\n" (Char.chr 0x001b)
    (Char.chr 0x001b)  token_line.filename token.lineno token.col (Char.chr 0x001b);
  printf "%s\n" token_line.line;
  for _ = 0 to col do
    printf " "
  done;
  printf "%c[31;1m" (Char.chr 0x001b);
  for _ = 1 to String.length token.s do
    printf "^"
  done;
  printf "%c[33m " (Char.chr 0x001b);
  print_endline e;
  printf "%c[0m" (Char.chr 0x001b)

let rhs token_line i =
  let idx = ref 0 in
  let parens = ref 0 in
  let square_parens = ref 0 in
  let curly_parens = ref 0 in
  try
    while true; do
      let _ = match (List.nth token_line.tokens (i + !idx)).ttype with
      | Operator(op) -> begin
        match op with 
        | ";" ->
          if !parens > 0 then
            raise (Invalid_statement("Mismatched ')'", i + !idx))
          else if !square_parens > 0 then
            raise (Invalid_statement("Mismatched ']'", i + !idx))
          else if !curly_parens > 0 then
            raise (Invalid_statement("Mismatched '}", i + !idx))
          else
            raise (End)
        | "(" -> parens := !parens + 1
        | ")" ->
          if !parens - 1 < 0 then
            raise (Invalid_statement("Mismatched ')'", i + !idx))
          else
            parens := !parens - 1
        | "[" -> square_parens := !square_parens + 1
        | "]" ->
          if !square_parens - 1 < 0 then
            raise (Invalid_statement("Mismatched ']'", i + !idx))
          else
            square_parens := !square_parens - 1
        | "{" -> curly_parens := !curly_parens + 1
        | "}" ->
          if !curly_parens - 1 < 0 then
            raise (Invalid_statement("Mismatched '}'", i + !idx))
          else
            curly_parens := !curly_parens - 1
        | _ -> printf "Op: %s\n" op
      end 
      | PString(s)   -> printf "String \"%s\"\n" s
      | Atom(a)      -> printf "Atom %s\n" a
      | Keyword(_)   -> raise (Invalid_statement("Can't assign a keyword to a variable", i))
      | _            -> printf "" 
      in
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