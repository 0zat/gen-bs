open Bs_args
open Js

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

  let to_let owner_name (js_meth: js_meth) =
    let args = to_def_args owner_name js_meth.args  in
    let lets = make_args_to_undef js_meth.args in
    let eval = Override.to_eval_origilal owner_name js_meth in
    to_let_def js_meth.name args lets eval

end


let has_optional args =
  List.exists (fun x -> x.necessity = `Optional) args

let to_let_if_optional owner_name js_meth =
  if has_optional js_meth.args then
    Some(Let_expr.to_let owner_name js_meth)
  else
    None