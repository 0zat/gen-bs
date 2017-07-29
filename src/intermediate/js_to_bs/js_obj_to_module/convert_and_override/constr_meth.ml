open Bs_external
open Js
open Js_args
open Js_type

let attr_to_arg js_attr =
  let type_ = to_arg_type js_attr.type_ in
  if js_attr.is_required then
    to_label_arg js_attr.name type_ 
  else
    to_optional_arg js_attr.name type_  

let meth_to_arg js_meth =
  let return_type = to_arg_type js_meth.return_type in
  let args = List.map (fun (x: js_arg) -> to_arg_type x.type_) js_meth.args in
  to_label_arg js_meth.name (`Func(args @ [return_type]))

let to_constr name args =
  let args = args @ [No_label(`Unit)] in
  let return_type = Bs_type.to_ident name in
  Bs_external.(to_external_expr "make" args return_type "" [Obj])

let rec gather_inherits ans all_objs name =
  match name with
  | None -> ans
  | Some name ->
    try
      let parent = List.find (fun x -> x.name = name) all_objs in
      gather_inherits (parent :: ans) all_objs parent.inherits
    with
    | Not_found -> failwith ("inheritance of " ^ name ^ "does not exists")

let to_external all_objs js_obj = 
  let inherits = gather_inherits [js_obj] all_objs js_obj.inherits in
  let obj = Js.append_objs inherits js_obj.name None in
  let attrs = List.map attr_to_arg obj.attrs in
  let meths = List.map meth_to_arg obj.meths  in
  to_constr obj.name (attrs @ meths)