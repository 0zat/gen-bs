open Js
open Js_type
open Js_args
open Bs_external

let to_normal_external owner_name (meth: js_meth) =
  let return_type, return_annot = to_return meth.return_type in
  let owner = to_owner_arg owner_name in
  let owner_type = to_owner_type owner_name in
  let args = to_bs_args meth.args in
  let action = meth.name in
  to_external_expr
    meth.name 
    (args @ [owner]) 
    return_type 
    action 
    [Send_pipe owner_type; return_annot]

let to_call owner_name return_type args =
  let return_type, return_annot = to_return return_type in
  let owner = to_owner_arg owner_name in
  let owner_type = to_owner_type owner_name in
  let args = to_bs_args args in
  let this = to_nolabel_arg (`As "this") in
  let args = this :: args in
  to_external_expr
    "call"
    (args @ [owner]) 
    return_type
    "prototype.call"
    [Send_pipe owner_type; return_annot]

let to_accessor owner_name (meth: js_meth) special =
  let return_type, return_annot = to_return meth.return_type in
  let owner = to_owner_arg owner_name in
  let owner_type = to_owner_type owner_name in
  let args = to_bs_args meth.args in
  let action = meth.name in
  let name, annot =
    match special with
    | `Getter | `Setter | `Deleter when meth.name <> "" -> 
      meth.name, Send_pipe owner_type
    | `Getter -> "get", Get_index
    | `Setter -> "set", Set_index
    | `Deleter -> "delete", Get_index
  in
  to_external_expr
    meth.name 
    (args @ [owner]) 
    return_type 
    action 
    [annot; return_annot]

let to_external owner_name meth = 
  match meth.special with
  | `None -> to_normal_external owner_name meth
  | `Getter -> to_accessor owner_name meth `Getter
  | `Setter -> to_accessor owner_name meth `Setter
  | `Deleter -> to_accessor owner_name meth `Deleter
  | `Call -> to_call owner_name meth.return_type meth.args 