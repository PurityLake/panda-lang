let escape = Char.chr 0x001b

let reset = "[0m"
let black = "[30m"
let red = "[31m"
let green = "[32m"
let yellow = "[33m"
let blue = "[34m"
let magenta = "[35m"
let cyan = "[36m"
let white = "[37m"

let bright_black = "[30;1m"
let bright_red = "[31;1m"
let bright_green = "[32;1m"
let bright_yellow = "[33;1m"
let bright_blue = "[34;1m"
let bright_magenta = "[35;1m"
let bright_cyan = "[36;1m"
let bright_white = "[37;1m"

let background_black = "[40m"
let background_red = "[41m"
let background_green = "[42m"
let background_yellow = "[43m"
let background_blue = "[44m"
let background_magenta = "[45m"
let background_cyan = "[46m"
let background_white = "[47m"

let background_bright_black = "[40;1m"
let background_bright_red = "[41;1m"
let background_bright_green = "[42;1m"
let background_bright_yellow = "[43;1m"
let background_bright_blue = "[44;1m"
let background_bright_magenta = "[45;1m"
let background_bright_cyan = "[46;1m"
let background_bright_white = "[47;1m"

let bold = "[1m"
let unerline = "[4m"
let reversed = "[7m"

let print_style style =
  Printf.printf "%c%s" escape style

let print_styles styles =
  let _ = List.map print_style styles in
  Printf.printf ""