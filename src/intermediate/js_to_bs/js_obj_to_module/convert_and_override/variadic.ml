open Js

module Predefined = struct
  let code = 
    "(* functions for variadic *)
type ('own, 'return, 'args) variadic_func
type 'args variadic_args

external apply : ('own, 'return, 'args) variadic_func ->
                 'own -> 'args variadic_args -> 'return = \"apply\" [@@bs.send]

let to_arg ary =
  let to_arg : 'a array -> 'a variadic_args [@bs] = 
    [%bs.raw \"function(a){ return a}\"]
  in
  Js_unsafe.fn_run1 to_arg ary

let unsafe_add ary mem = 
  let unsafe_add : 'a variadic_args -> 'b -> ('b * 'a) variadic_args [@bs] = 
    [%bs.raw \"function(a, b){ return [b].concat(a)}\"]
  in
  Js_unsafe.fn_run2 unsafe_add ary mem"
end

module External_expr = struct
  open Bs_external

  let to_variadic_func_type owner_name return_type args = 
    let rec args_to_tuple = function
      | [] -> failwith "no argument function should not be variadic"
      | [a] -> `Tuple([a])
      | hd :: tl -> `Tuple([hd; args_to_tuple tl])
    in
    let arg_types =
      Js.args_to_types args 
      |> List.map (Js_type.to_bs_type `Arg) 
      |> args_to_tuple
    in
    let owner_type = Js_type.to_owner_type owner_name in 
    Bs_type.to_ident ~variables:[owner_type; return_type; arg_types]  "variadic_func"

  let to_external owner_name (meth: js_meth) =  
    let return_type = Js_type.to_bs_type `Return meth.return_type in
    let variadic_func_type = to_variadic_func_type owner_name return_type meth.args in
    to_external_expr
      ~name: meth.name 
      ~args: [Js_args.to_owner_arg owner_name]
      ~return_type: variadic_func_type 
      ~action: meth.name 
      ~annot: [Get]

end

module Let_expr = struct
  open Bs_let

  let func = "func"
  let args = "args"

  let make_eval owner_name = 
    List.map to_eval_ident ["apply"; func; owner_name; args]

  let make_args variadic_arg_name =
    let expr = List.map to_eval_ident ["to_arg"; variadic_arg_name] in
    to_let_var args (to_eval expr)

  let add_arg arg_name  =
    let expr = List.map to_eval_ident ["unsafe_add"; args; arg_name] in
    to_let_var args (to_eval expr)

  let make_func meth_name owner_name =
    to_let_var func (List.map to_eval_ident [meth_name; owner_name] |> to_eval)

  let make_let_lines owner_name (js_meth: js_meth) =
    match List.rev (args_to_names js_meth.args) with
    | [] -> failwith "internal error: proc variadic to no argument"
    | hd :: tl ->
      (make_func js_meth.name owner_name) ::
      (make_args hd) ::
      (List.map add_arg tl)

  let to_let owner_name (js_meth: js_meth) =
    let args = List.map to_eval_label (args_to_names js_meth.args) in
    let owner = to_nolabel owner_name in
    let meth_name = js_meth.name in
    let let_lines = make_let_lines owner_name js_meth in
    let eval_line = make_eval owner_name in
    to_let_def meth_name (args @ [owner]) let_lines eval_line

end

let predefined_code = Predefined.code

let has_variadic args =
  let aux = function
    | `Variadic -> true
    | _ -> false
  in
  List.exists (fun x -> aux x.necessity) args

let to_external_if_variadic owner_name js_meth =
  if has_variadic js_meth.args then
    Some(External_expr.to_external owner_name js_meth)
  else
    None

let to_let_if_variadic owner_name js_meth =
  if has_variadic js_meth.args then
    Some(Let_expr.to_let owner_name js_meth)
  else
    None