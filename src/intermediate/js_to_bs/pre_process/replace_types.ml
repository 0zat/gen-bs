open Js

let rec replace_type types type_ =
  let replace = replace_type types in
  match (type_ : Js.type_) with
  | `Obj obj when List.mem_assoc obj types ->
    List.assoc obj types
  | `Obj obj when obj = "DOMHighResTimeStamp" ->
    List.iter (fun (x, y) -> print_string x) types;
    failwith "aaa"
  | `Promise t -> `Promise (replace t)
  | `Undef t -> `Undef (replace t)
  | `Null t -> `Null (replace t)
  | `Array t -> `Array (replace t)
  | `Union types -> `Union (List.map replace types) 
  | `Callback(ret, arg) ->
    if BatList.is_empty arg then
      `Callback(ret, [`Unit])
    else
      `Callback(replace ret, List.map replace arg)
  | _ -> type_ 

let replace_arg types (arg: js_arg) =
  {arg with type_ = replace_type types arg.type_}

let replace_meth types js_meth =
  {
    js_meth with 
    args = List.map (replace_arg types) js_meth.args;
    return_type = replace_type types js_meth.return_type
  }

let replace_attr types js_attr =
  { js_attr with type_ = replace_type types js_attr.type_ }

let replace_obj types js_obj =
  {
    js_obj with
    attrs = List.map (replace_attr types) js_obj.attrs ;
    meths = List.map (replace_meth types) js_obj.meths
  }

let replace_objs types js_objs =
  List.map (replace_obj types) js_objs

let rec replace_typedef type_defs =
  let replace (x, y) = (x, replace_type type_defs y) in
  let replaced = List.map replace type_defs in
  if replaced = type_defs then
    replaced
  else
    replace_typedef replaced (* repeat until this conversion changes nothing *)
