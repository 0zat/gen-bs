open Bs_type

let rec to_bs_type return_or_arg : Js.type_ -> Bs_type.type_ = function
  | `Int -> `Int
  | `Float -> `Float
  | `String -> `String 
  | `Unit -> `Unit
  | `Boolean -> `Js_boolean
  | `Null type_ -> `Null(to_bs_type return_or_arg type_)
  | `Undef type_ -> `Undef(to_bs_type return_or_arg type_)
  | `Array type_ -> `Array(to_bs_type return_or_arg type_)
  | `Any -> `Any
  | `Enum enum -> `String
  | `Promise type_ -> `Promise(to_bs_type return_or_arg type_)
  | `Obj name ->
    let open Js_obj_to_typedef in
    begin match return_or_arg with
      | `Return -> to_ident (make_ident_name name)
      | `Arg -> to_ident ~variables:[`Underbar] (make_like_name name)
    end
  | #Bs_type.buffer as buffer -> buffer
  | `Union _ -> `Any
  | `Callback(type_, types) ->  `Func((List.map (to_bs_type return_or_arg) types) @ [ to_bs_type return_or_arg type_])
  | `Variadic_callback(return_type, arg_types, variadic_type) ->
    (* the reason of `Return is object subtype contravariant*)
    let args = List.map (to_bs_type `Return) arg_types in
    let variadic = to_bs_type `Return variadic_type in
    let return = to_bs_type `Return return_type in
    let ident_name = "variadic_callback" ^ (List.length args |> string_of_int) in
    to_ident ~variables:(args @ [variadic; return; `Underbar]) ident_name

(* replace only toplevel boolean*)
let replace_boolean = function
  | `Js_boolean -> `Ocaml_bool
  | other -> other

let remove_null_undef =
  let open Bs_module in
  let open Bs_external in
  function 
  | `Undef `Null type_
  | `Null `Undef type_ -> `Option type_, (Return Null_undefined_to_opt)
  | `Null type_ -> `Option type_, (Return Null_to_opt)
  | `Undef type_ -> `Option type_, (Return Undefined_to_opt)
  | type_ -> type_, (Return Identity)

let to_return type_ = 
  let type_ = to_bs_type `Return type_ in
  let type_, annot = remove_null_undef type_ in
  replace_boolean type_, annot

let to_arg_type = to_bs_type `Arg

let to_attr_type =  to_bs_type `Return

let to_owner_type name = 
  let open Js_obj_to_typedef in
  to_ident ~variables:[to_variable "own"] (make_like_name name)