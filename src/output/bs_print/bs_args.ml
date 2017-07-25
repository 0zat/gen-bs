open Bs_str
open Bs_type

type arg =
  | No_label of type_ 
  | Label of uncapital Bs_str.t * type_
  | Optional of uncapital Bs_str.t * type_ 

type args = arg list
type t = args

module Print = struct

  let print_type t = Bs_type.print t

  let print_arg arg = 
    match arg with
    | No_label type_ -> print_type type_
    | Label(label_name, type_) -> 
      Printf.sprintf "%s:%s" (to_string label_name) (Bs_type.print type_)
    | Optional(label_name, type_) -> 
      Printf.sprintf "?%s:%s" (to_string label_name) (Bs_type.print type_)

  let print_args args = 
    List.map print_arg args
    |> String.concat " -> "
end

module Construct = struct

  let to_nolabel ~type_ = No_label type_
  let to_label ~label ~type_ = Label(to_uncapital label, type_)
  let to_optional ~label ~type_ = Optional(to_uncapital label, type_)

end

(* expose functions. others are hidden by mli *)
let print arg = Print.print_arg arg

include Construct