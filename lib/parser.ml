open Tokenizer
open Printf
open Terminal

(* Invalid_statement takes a tuple of an error message and the index
    of the offender in the token list *)
exception Invalid_statement of string * int
exception End

(* Simply prints out an error message  *)
let stack_trace token_line e idx =
  let token = List.nth token_line.tokens idx in
  let col = token.col - 1 in
  print_endline "";
  print_styles [bold; magenta];
  printf "%s:\nError on line %d col %d\n" token_line.filename token.lineno token.col;
  print_style reset;
  printf "%s\n" token_line.line;
  (* fill up empty space to position the ^ characters correctly *)
  for _ = 0 to col do
    printf " "
  done;
  print_style bright_red;
  for _ = 1 to String.length token.s do
    printf "^"
  done;
  print_style yellow;
  printf " %s\n" e;
  print_style reset

(* rhs - right hand side - matches expression that occur on the right side
    of an equals sign*)
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
        | "=" -> 
          raise (Invalid_statement("'=' is not a valid character in an rhs expresion", i + !idx))
        | _ -> printf "Op: %s\n" op
      end 
      | RString(s)       -> printf "String \"%s\"\n" s
      | Atom(a)          -> printf "Atom %s\n" a
      | Keyword(_)       -> raise (Invalid_statement("Can't assign a keyword to a variable", i))
      | Integer(i)       -> printf "Integer %d\n" i
      | FloatingPoint(f) -> printf "Float %f\n" f
      | _                 -> printf "" 
      in
      idx := !idx + 1
    done
  with
    End -> printf ""

let let_statement token_line i =
  printf "let statement\n";
  let _ = 
    match (List.nth token_line.tokens (i + 1)).ttype with
    | Atom(name) ->
      printf "name = %s\n" name;
      name
    | Keyword(name)    -> raise (Invalid_statement("'" ^ name ^ "' is a keyword and cannot be assigned to", i + 1))
    | Operator(name)   -> raise (Invalid_statement("'" ^ name ^ "' is an operator and cannot be assigned to", i + 1))
    | RString(_)       -> raise (Invalid_statement("Cannot assign to a string", i + 1))
    | Start            -> raise (Invalid_statement("Start token is meant for beginning of list", i + 1))
    | FloatingPoint(_) -> raise (Invalid_statement("Cannot assign to a float", i + 1))
    | Integer(_)       -> raise (Invalid_statement("Cannot assign to an int", i + 1))
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
    let_statement token_line i
  else
    print_endline "nope"


let parse token_line =
  printf "%s\n" token_line.line;
  try expr token_line 1
    with
      Invalid_statement(e, idx) -> stack_trace token_line e idx