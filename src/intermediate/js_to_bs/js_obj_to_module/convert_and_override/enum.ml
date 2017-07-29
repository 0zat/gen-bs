open Js

let rec get_enum type_ =
  match (type_: Js.type_) with
  | `Enum enum -> Some enum
  | `Array a -> get_enum a
  | `Null n -> get_enum n
  | `Undef u -> get_enum u
  | `Promise p -> get_enum p
  | `Union u -> None (* enum in union is not supported now *)
  | `Callback(ret, args) -> None (* enum in callback is not supported now *)
  | _ -> None

let is_enum type_ =
  BatOption.is_some (get_enum type_)

module Let_expr = struct
  open Bs_let
  open Override

  let make_variant_to_str enum =
    let to_enum_case str =
      to_variant_case str (to_eval [to_eval_str str])
    in
    let cases = List.map to_enum_case enum in
    to_function cases 

  let make_str_to_variant enum =
    let to_enum_case str =
      to_str_case str (to_eval [to_eval_variant str])
    in
    let cases = List.map to_enum_case enum in
    to_function cases 

  let make_convert enum_convert type_ =
    match get_enum type_ with
    | None -> failwith (Printf.sprintf "not have enum") 
    | Some enum -> enum_convert enum 

  let make_convert_arg type_ =
    make_convert make_variant_to_str type_ 

  let make_convert_return type_ =
    make_convert make_str_to_variant type_

  let make_convert_code owner_name (js_meth: js_meth) =
    let is_target_arg (arg: js_arg) = is_enum arg.type_ in
    let conf = 
      make_conf 
        ~make_convert_arg
        ~is_target_arg
        ~is_target_return: is_enum
        ~make_convert_return
    in
    make_def conf owner_name js_meth

end

let has_enum args = List.exists (fun (x: js_arg) -> is_enum x.type_) args 

let to_let_if_enum owner_name js_meth =
  if has_enum js_meth.args then
    Some(Let_expr.make_convert_code owner_name js_meth)
  else
    None