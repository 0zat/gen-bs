let internal_use_code = "
type _baseClass
"

let get () = 
  internal_use_code ^ 
  Variadic.predefined_code ^ 
  Override.Predefined.code ^
  Optional.Predefined.code 