type token_type =
	| Start
	| Keyword of string
	| Operator of string
	| Atom of string
	| PString of string

type token = {
	ttype : token_type;
	s : string;
	lineno : int;
	col : int;
}

type token_line = {
	filename: string;
	line: string;
	tokens: token list;
}

let operators = "+-/*^&|!;:@<>=()[]{}"
let whitespace = " \t\r\n"
let keywords = ["let"; "func"; "class"; "return"]

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
	
let tokenize_string s i =
	__rec_tokenize_string "" s i

let tokenize_atom s i = 
	__rec_tokenize_atom  "" s i

let rec _tokenize l s i lineno =
	let len = String.length s in
	let ret = if i < len then begin
			let c = s.[i] in
			let isop = String.contains operators c in
			let isws = String.contains whitespace c in
			if isop then begin
				let new_l = l@[{ 
					ttype = Operator (Char.escaped c);
					s = Char.escaped c;
					lineno = 1;
					col = i;
				}] in
				_tokenize new_l s (i + 1) lineno
			end else if not isws then
				if c == '"' then begin
					let ret, idx = tokenize_string s (i + 1) in
					let new_l = l@[{ 
						ttype = Atom ret;
						s = ret;
						lineno = 1;
						col = i;
					}] in
					_tokenize new_l s idx lineno
				end else begin
					let ret, idx = tokenize_atom s i in
					let new_l = 
						match List.find_opt (fun s -> s = ret) keywords with
						| None ->
							l@[{
								ttype = Atom ret;
								s = ret;
								lineno = 1;
								col = i;
							}]
						| _ ->
							l@[{ 
								ttype = Keyword ret;
								s = ret;
								lineno = 1;
								col = i;
							}]
					in
					_tokenize new_l s idx lineno
				end
			else begin
				_tokenize l s (i + 1) lineno
			end
		end else
			l
	in
	ret

let tokenize s lineno =
	let l = [ { ttype = Start; s = ""; lineno = 1; col = 0; } ] in
	_tokenize l s 0 lineno

let collect t l fname =
	{
		filename = fname;
		line = l;
		tokens = t;
	}
