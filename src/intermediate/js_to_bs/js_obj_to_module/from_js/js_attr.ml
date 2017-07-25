open Js
open Js_type
open Bs_external

let to_getter owner type_ action = 
  let return_type, return_annot = to_return type_ in
  let annot = [Get; return_annot] in
  to_external_expr action [owner] return_type action annot 

let to_setter owner type_ action = 
  let type_ = to_attr_type type_ in
  let arg = Bs_args.to_label action type_ in
  let name = "set" ^ (String.capitalize_ascii action) in
  to_external_expr name [owner; arg] `Unit action [Set]

let to_attr owner js_attr =
  let setter = to_setter owner js_attr.type_ js_attr.name in
  let getter = to_getter owner js_attr.type_ js_attr.name in
  if js_attr.is_readonly then
    [getter]
  else
    [getter; setter]