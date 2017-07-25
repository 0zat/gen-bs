open Js

open Js_type

let to_js_arg_type necessity type_ =
  match necessity with
  | `Fixed -> type_
  | `Optional -> (`Undef type_)
  | `Variadic -> (`Array type_)

let to_bs_arg (js_arg : js_arg) =
  let js_arg_type = to_js_arg_type js_arg.necessity js_arg.type_ in
  let type_ = to_arg_type js_arg_type in
  match js_arg.necessity with
  | `Fixed -> Bs_args.to_label js_arg.name type_
  | `Optional -> Bs_args.to_optional js_arg.name type_
  | `Variadic -> Bs_args.to_label js_arg.name type_

let to_bs_args args = 
  List.map to_bs_arg args

let to_owner_arg name =  
  let obj = to_bs_type `Arg (`Obj name) in
  Bs_args.to_label name obj

