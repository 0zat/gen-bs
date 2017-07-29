open Js
open Bs_let

module Predefined = struct
  let code = 
    "
(* functions for override *)
let access_array f x = Array.map f x
let access_null f x = Js_null.bind x (Js_unsafe.fn_mk1 f) 
let access_undef f x = Js_undefined.bind x (Js_unsafe.fn_mk1 f) 
(*let access_promise f x = Js_promise.then_ f x*)
"
end

type conf = {
  make_convert_arg: Js.type_ -> Bs_let.expr;
  is_target_arg: Js.js_arg -> bool;
  is_target_return: Js.type_ -> bool ;
  make_convert_return: Js.type_ -> Bs_let.expr;
}

let make_conf ~make_convert_arg ~is_target_arg ~make_convert_return ~is_target_return =
  { make_convert_arg; is_target_arg; is_target_return; make_convert_return}

(* ex: let var_name = wrap var_name in result *)
let override var_name wrap =
  let idents = List.map (fun x -> to_eval_ident x) [wrap; var_name] in
  to_let_var 
    ~var_name 
    ~expr:(to_eval idents)

let rec make_accessor_list type_ =
  match (type_: Js.type_) with
  | `Array t -> "access_array" :: (make_accessor_list t)
  | `Null t ->  "access_null" :: (make_accessor_list t)
  | `Undef t ->  "access_undef" :: (make_accessor_list t)
  | `Promise t -> "access_promise" :: (make_accessor_list t)
  | `Callback _ -> failwith "callback is unsupported for override"
  | _ -> []

(* 
     ex(when 'a array Js.null):
     let var_name = access_array var_name in
     let var_name = access_null var_name in
*)
let make_access_lines var_name type_ =
  let accessors = make_accessor_list type_ in
  List.rev_map (override var_name) accessors

(* 
     ex(when 'a array Js.null):
     let var_name = conv_var_name var_name in
     let var_name = access_array var_name in
     let var_name = access_null var_name in
*)
let make_convert_code convert var_name type_ =
  let func_name = "conv_" ^ var_name in
  let access_lines = make_access_lines func_name type_ in
  let let_conv = to_let_var func_name convert in
  let do_convert = 
    let eval = List.map to_eval_ident [func_name; var_name] in
    to_let_var var_name (to_eval eval) 
  in
  let_conv :: access_lines @ [do_convert]

let to_eval_origilal owner_name (js_meth: js_meth) =
  let arg_names = args_to_names js_meth.args in
  let args = List.map to_label arg_names in
  let owner = to_label owner_name in
  let meth = to_eval_ident js_meth.name in
  (meth :: args @ [owner])

let make_let_content conf owner_name (js_meth: js_meth) =
  let arg_to_code (arg: js_arg) = 
    let type_ =
      match arg.necessity with
      | `Optional -> `Undef arg.type_
      | `Variadic -> `Array arg.type_
      | `Fixed -> arg.type_
    in
    make_convert_code (conf.make_convert_arg arg.type_) arg.name type_ 
  in
  let convert_args = 
    List.filter conf.is_target_arg  js_meth.args 
    |> List.map arg_to_code
    |> List.flatten
  in
  let return_var = "return" in
  let eval_original = to_eval_origilal owner_name js_meth in
  let let_return = to_let_var return_var (to_eval eval_original) in
  let convert_return =
    if conf.is_target_return js_meth.return_type then
      let convert_retrun = 
        let conv_ret = conf.make_convert_return js_meth.return_type in
        make_convert_code conv_ret return_var js_meth.return_type
      in
      let_return :: convert_retrun 
    else
      [let_return]
  in
  let eval = [to_eval_ident return_var] in
  (convert_args @ convert_return, eval)

let make_def conf owner_name (js_meth: js_meth) =
  let lets, eval = make_let_content conf owner_name js_meth in
  let arg_names = (args_to_names js_meth.args) @ [owner_name] in
  let args = List.map to_label arg_names in
  to_let_def js_meth.name args lets eval


