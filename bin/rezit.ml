open Rezit.Tokenizer

let parse_string = ref ""
let file_name = ref ""

let usage = "usage: " ^ Sys.argv.(0) ^ "[-p string]"

let speclist = [
  ("-p", Arg.Set_string parse_string, ": pass a string to turn into tokens");
  ("-f", Arg.Set_string file_name, ": pass a file name to be read");
]

let () =
  Arg.parse
    speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;
  if String.length !parse_string > 0 then begin
    let line = collect (tokenize !parse_string 1) !parse_string "<stdin>" in
    Rezit.Parser.parse line
  end else if String.length !file_name > 0 then begin
    let ic = open_in !file_name in
    try
      let l = ref 1 in
      while true; do
          let line = input_line ic in
          let tokens = tokenize line l in
          Rezit.Parser.parse (collect tokens line !file_name);
          print_endline "";
          l := !l + 1
      done;
    with End_of_file ->
        close_in ic
  end else
    Printf.printf ""