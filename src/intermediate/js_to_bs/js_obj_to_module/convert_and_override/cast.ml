open Js
open Js_obj_to_typedef
open Bs_type
open Bs_let

let make_cast func_name from_type to_type =
  let args = [to_nolabel "x"] in
  let obj_magic = to_eval_ident ~modules:["Obj"] "magic" in
  let x = to_eval_ident_with_type "x" from_type in
  let convert = [obj_magic; x] in
  let let_return = to_let_var "return" (to_eval convert) in
  let return = to_eval_ident_with_type "return" to_type in
  to_let_def func_name args [let_return] [return]

let make_downcast name =
  let from_type = to_ident (make_ident_name name) in
  let to_type = to_ident ~variables:[`Underbar] (make_like_name name) in
  make_cast "downcast" from_type to_type

let get_external_type external_types name =
  let filter (key, modules, ident) =
    let bs_type_name = make_ident_name name in
    String.lowercase_ascii bs_type_name = String.lowercase_ascii key
  in
  List.filter filter external_types 

let make_cast_external_type name external_type =
  let _, modules, ident = external_type in
  let external_name = String.concat "_" modules ^ "_" ^ ident in
  let inside_type = to_ident (make_ident_name name) in
  let external_type = to_ident ~modules ident in
  [
    make_cast ("cast_to_" ^ external_name) inside_type external_type;
    make_cast ("cast_from_" ^ external_name) external_type inside_type;
  ]

let make_cast_external_type external_types name =
  let external_types_matched = get_external_type external_types name in
  List.map (make_cast_external_type name) external_types_matched
  |> List.flatten