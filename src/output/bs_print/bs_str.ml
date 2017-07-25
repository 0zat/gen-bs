type capital 
type uncapital

type _ t = string

let to_capital (str: string) : capital t = 
  Util.escape str
  |> Str.replace_first (Str.regexp "^_") ""
  |> String.capitalize_ascii 

let to_uncapital (str: string) : uncapital t = 
  Util.escape str
  |> String.uncapitalize_ascii 
  
let to_string : _ t -> string = fun x -> x