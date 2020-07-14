type token_type =
	| Start
	| Keyword of string
	| Operator of string
	| Atom of string
	| PString of string

(* individual token *)
type token = {
	(* the type of the token *)
	ttype : token_type;
	(* Even though ttype holds the lexeme in its definition
		to allow for quick access to the contents without pattern matching
		a copy is stored in s *)
	s : string;
	lineno : int;
	col : int;
}

(* This is passed to the parse function *)
type token_line = {
	(* For purposes of error checking so stacktrace can refer to the file name *)
	filename: string;
	line: string;
	tokens: token list;
}

(* operators in the rezit language *)
let operators = "+-/*^&|!;:@<>=()[]{}"
(* whitespace recognized by rezit *)
let whitespace = " \t\r\n"
(* keywords in rezit *)
let keywords = ["let"; "func"; "class"; "return"]

(* recursive function to tokenize a string
		buf: state passed to recursive calls
		s: the string to be tokenized
		i: index in the string  *)
let rec __rec_tokenize_string buf s i =
	let should_quit = i < String.length s && s.[i] == '"' && if i > 0 then s.[i-1] != '\\' else true in
	let ret, idx = 
		if not should_quit && i < String.length s then begin
			let new_buf = buf ^ Char.escaped s.[i] in
			__rec_tokenize_string new_buf s (i + 1)
		end else 
			buf, i + 1
	in
	ret, idx

(* recursive function to tokenize an attom
	buf: state passed to recursive calls
	s: the string to be tokenized
	i: index in the string  *)
let rec __rec_tokenize_atom buf s i =
	let should_quit = if i < String.length s then String.contains whitespace s.[i] || String.contains operators s.[i] else false in
	let ret, idx = 
		if not should_quit && i < String.length s then begin
			let new_buf = buf ^ Char.escaped s.[i] in
			__rec_tokenize_atom new_buf s (i + 1)
		end else
		buf, i
	in
	ret, idx

(* exposed function to tokenize a string
		s: the string to be tokenized
		i: index to start from *)
let tokenize_string s i =
	__rec_tokenize_string "" s i

(* exposed function to tokenize an atom 
		s: the string to be tokenized
		i: index to start from *)
let tokenize_atom s i = 
	__rec_tokenize_atom  "" s i

(* recursive variant of the exposed tokenize function, turns a string
		into list of tokens
		l: state passed to each recursive call
		s: string to be tokenized *)
let rec __tokenize l s i lineno =
	let len = String.length s in
	let ret = if i < len then begin
			let c = s.[i] in (* the current character *)
			let isop = String.contains operators c in (* if c is an operator *)
			let isws = String.contains whitespace c in (* if c is whitespace *)
			if isop then begin
				let new_l = l@[{ 
					ttype = Operator (Char.escaped c);
					s = Char.escaped c;
					lineno = lineno;
					col = i;
				}] in
				__tokenize new_l s (i + 1) lineno
			end else if not isws then
				if c == '"' then begin
					let ret, idx = tokenize_string s (i + 1) in
					let new_l = l@[{ 
						ttype = Atom ret;
						s = ret;
						lineno = lineno;
						col = i;
					}] in
					__tokenize new_l s idx lineno
				end else begin
					let ret, idx = tokenize_atom s i in
					let new_l = 
						match List.find_opt (fun s -> s = ret) keywords with
						| None ->
							l@[{
								ttype = Atom ret;
								s = ret;
								lineno = lineno;
								col = i;
							}]
						| _ ->
							l@[{
								ttype = Keyword ret;
								s = ret;
								lineno = lineno;
								col = i;
							}]
					in
					__tokenize new_l s idx lineno
				end
			else begin
				__tokenize l s (i + 1) lineno
			end
		end else
			l
	in
	ret

(* exposed function to tokenize a string
		s: string to be tokenized *)
let tokenize s lineno =
	let l = [ { ttype = Start; s = ""; lineno = 1; col = 0; } ] in
	__tokenize l s 0 lineno

(* converts a list of tokens into a token_line object 
		t: the list of tokens
		l: the line number of the token list
		fname: filename of the file being read *)
let collect t l fname =
	{
		filename = fname;
		line = l;
		tokens = t;
	}
