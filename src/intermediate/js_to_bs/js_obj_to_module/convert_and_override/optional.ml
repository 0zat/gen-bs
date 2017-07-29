open Js

module Predefined = struct
  let code ="
let undef_to_opt x = Js_undefined.to_opt
let null_to_opt x = Js_null.to_opt
let identity x = x
"
end

let get_optional arg =
  match arg.necessity with
  | `Optional -> Some (Util.escape arg.name)
  | _ -> None

let is_optional arg =
  get_optional arg |> BatOption.is_some

module Let_expr = struct
  open Bs_let

  let make_arg_to_undef arg_name =
    let func_name = "conv_" ^ arg_name in 
    let from_opt = to_eval_ident ~modules:["Js_undefined"] "from_opt" in
    [
      to_let_var (func_name) (to_eval [from_opt]);
      let eval = List.map to_eval_ident [func_name; arg_name] in
      to_let_var arg_name (to_eval eval) 
    ]

  let make_args_to_undef args =
    BatList.filter is_optional args
    |> args_to_names
    |> List.map make_arg_to_undef
    |> List.flatten

  let to_def_arg (js_arg: js_arg) =
    match js_arg.necessity with
    | `Optional -> to_optional js_arg.name
    | `Fixed -> to_label js_arg.name
    | `Variadic -> to_label js_arg.name

  let to_def_args owner_name js_args =
    (to_label owner_name) :: (List.map to_def_arg js_args) @ [`Unit]

  let rec convert_return type_ =
    let rec convert_return_type type_ =
      match (type_: Js.type_) with
      | `Array t -> "access_array" :: (convert_return_type t)
      | `Null t ->  "null_to_opt" :: (convert_return_type t)
      | `Undef t ->  "undef_to_opt" :: (convert_return_type t)
      | `Promise t -> (convert_return_type t)
      | `Callback _ -> failwith "callback is returned"
      | _ -> []
    in
    List.rev_map (Override.override "conv_return") (convert_return_type type_)

  let rec has_null_undef type_ =
    match (type_: Js.type_) with
    | `Array t -> has_null_undef t
    | `Null t ->  true
    | `Undef t ->  true
    | `Promise t -> has_null_undef t
    | _ -> false

  let sould_return_overrride (js_meth: js_meth) =
    Variadic.has_variadic js_meth.args &&
    has_null_undef js_meth.return_type 

  let apply_conv =
    let eval =
      List.map to_eval_ident ["conv_return"; "return"]
      |> to_eval
    in
    to_let_var "return" eval

  let make_conv_return (js_meth: js_meth) lets args eval_original =
    let let_return = to_let_var "return" (to_eval eval_original) in
    let convert_return = convert_return js_meth.return_type in
    let conv_return_def = to_let_var "conv_return" (to_eval [to_eval_ident "identity"]) in
    let lets = conv_return_def :: lets @ (let_return :: convert_return) @ [apply_conv] in
    let eval = [to_eval_ident "return"] in
    to_let_def js_meth.name args lets eval

  let to_let owner_name (js_meth: js_meth) =
    let args = to_def_args owner_name js_meth.args  in
    let lets = make_args_to_undef js_meth.args in
    let eval_original = Override.to_eval_origilal owner_name js_meth in
    if sould_return_overrride js_meth then
      make_conv_return js_meth lets args eval_original
    else
      to_let_def js_meth.name args lets eval_original

end

let predefined_code = Predefined.code

let has_optional args =
  List.exists (fun x -> x.necessity = `Optional) args

let to_let_if_optional owner_name js_meth =
  if has_optional js_meth.args || Let_expr.sould_return_overrride js_meth then
    Some(Let_expr.to_let owner_name js_meth)
  else
    None