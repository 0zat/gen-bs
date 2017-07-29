open Js
open Js_type
open Js_args
open Bs_external

let to_normal_external owner (meth: js_meth) =
  let return_type, return_annot = to_return meth.return_type in
  let args = to_bs_args meth.args in
  let action = meth.name in
  to_external_expr
    meth.name 
    (owner :: args) 
    return_type 
    action 
    [Send; return_annot]

let to_call owner return_type args =
  let return_type, return_annot = to_return return_type in
  let args = to_bs_args args in
  let this = to_nolabel_arg (`As "this") in
  let args = owner :: this :: args in
  to_external_expr
    "call"
    args
    return_type
    "prototype.call"
    [Send; return_annot]

let to_accessor owner (meth: js_meth) special =
  let return_type, return_annot = to_return meth.return_type in
  let args = to_bs_args meth.args in
  let action = meth.name in
  let name, annot =
    match special with
    | `Getter | `Setter | `Deleter when meth.name <> "" -> meth.name, Send
    | `Getter -> "get", Get_index
    | `Setter -> "set", Set_index
    | `Deleter -> "delete", Get_index
  in
  to_external_expr
    meth.name 
    (owner :: args) 
    return_type 
    action 
    [annot; return_annot]

let to_external owner meth = 
  match meth.special with
  | `None -> to_normal_external owner meth
  | `Getter -> to_accessor owner meth `Getter
  | `Setter -> to_accessor owner meth `Setter
  | `Deleter -> to_accessor owner meth `Deleter
  | `Call -> to_call owner meth.return_type meth.args 