open Js
open Js_obj_to_typedef
open Bs_type
open Bs_let

let make_downcast name =
  let from_type = to_ident (make_ident_name name) in
  let to_type = to_ident ~variables:[`Underbar] (make_like_name name) in
  let args = [to_nolabel "x"] in
  let obj_magic = to_eval_ident ~modules:["Obj"] "magic" in
  let x = to_eval_ident_with_type "x" from_type in
  let convert = [obj_magic; x] in
  let let_return = to_let_var "return" (to_eval convert) in
  let return = to_eval_ident_with_type "return" to_type in
  to_let_def "downcast" args [let_return] [return]