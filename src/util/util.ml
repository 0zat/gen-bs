
module Escape = struct

  let ocaml_reserved_words = [
    "and" ; "as" ; "assert" ; "asr" ; "begin" ; "class" ; 
    "closed" ; "constraint" ; "do" ; "done" ; "downto" ; "else" ;
    "end" ; "exception" ; "external" ; "false" ; "for" ; "fun" ;
    "function" ; "functor" ; "if" ; "in" ; "include" ; "inherit" ;
    "land" ; "lazy" ; "let" ; "lor" ; "lsl" ; "lsr" ;
    "lxor" ; "match" ; "method" ; "mod" ; "module" ; "mutable" ;
    "new" ; "of" ; "open" ; "object"; "or" ; "parser" ; "private" ;
    "rec" ; "sig" ; "struct" ; "then" ; "to" ; "true" ;
    "try" ; "type" ; "val" ; "virtual" ; "when" ; "while" ; "with" ;
  ]

  let replace_char = Str.regexp "[^A-Za-z0-9_]"

  let should_be_add_bar str =
    (List.mem str ocaml_reserved_words)
    || Str.(string_partial_match (regexp "^[0-9]") str 0)

  let escape str =
    let str = if str = "" then "None" else str in
    let str = if str = "2d" then "_2d" else str in
    let str = BatString.nreplace str "+" "_" in
    let str = BatString.nreplace str "/" "_" in
    let str = BatString.nreplace str "-" "_" in
    match should_be_add_bar str with
    | true -> str ^ "_"
    | false -> str

end

let escape = Escape.escape

let classify to_key list =
  let keys = 
    List.map (fun x -> to_key x) list
    |> List.sort_uniq Pervasives.compare
  in
  let gather_by_key key =
    List.filter (fun x -> to_key x = key) list
  in
  List.map gather_by_key keys