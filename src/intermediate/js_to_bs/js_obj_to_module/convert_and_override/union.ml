open Js

let rec get_union (type_: Js.type_) =
  match type_ with
  | `Union u -> Some u
  | `Array a -> get_union a
  | `Null n -> get_union n
  | `Undef u -> get_union u
  | `Promise p -> get_union p
  | `Callback(ret, args) -> None (* union in callback is not supported now *)
  | _ -> None

let is_union type_ =
  BatOption.is_some (get_union type_)

module Let_expr = struct
  open Bs_let
  open Override

  let var_x = "x"

  let rec type_to_variant type_ =
    match (type_ : Js.type_) with 
    | `Int -> "Int"
    | `Float -> "Float"
    | `String -> "String" 
    | `Unit -> "Unit"
    | `Boolean -> "Js_boolean"
    | `Null type_ -> (type_to_variant type_) ^ "_Null"
    | `Undef type_ -> (type_to_variant type_) ^ "_Undef"
    | `Array type_ -> (type_to_variant type_) ^ "_Array"
    | `Any -> "Any"
    | `Enum enum -> "String"
    | `Promise type_ -> (type_to_variant type_) ^ "_Promise"
    | `Obj name -> name
    | #Bs_type.buffer as buffer -> Bs_type.print buffer
    | `Union _ -> "Any"
    | `Callback(type_, types) -> "Callback"

  let make_case type_ =
    let eval = 
      let obj_magic = to_eval_ident ~modules:["Obj"] "magic" in
      Bs_let.to_eval [obj_magic; to_eval_ident var_x]
    in
    let variant = type_to_variant type_ in
    to_variant_case 
      ~match_ident:(var_x, Js_type.to_bs_type `Arg type_) 
      ~variant 
      eval

  let make_conv_union_arg type_ =
    match get_union type_ with
    | None -> failwith "not union. but called"
    | Some union ->
      let cases = List.map make_case union in
      to_function cases

  let make_conv_union_return type_ =
    let conv = to_eval_ident ~modules:["Js"; "Types"] "classify" in
    to_eval [conv]

  let make_convert_code owner_name (js_meth: js_meth) =
    let is_target_arg (arg: js_arg)= is_union arg.type_ in
    let conf = 
      make_conf 
        ~make_convert_arg: make_conv_union_arg
        ~is_target_arg
        ~is_target_return: is_union
        ~make_convert_return: make_conv_union_return
    in
    make_def conf owner_name js_meth

end


let has_union args = List.exists (fun (x: js_arg) -> is_union x.type_) args 

let to_let_if_union owner_name js_meth =
  if has_union js_meth.args then
    Some(Let_expr.make_convert_code owner_name js_meth)
  else
    None