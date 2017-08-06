open Idl_type

let to_js_meth operation = 
  let open Webidl.Data in
  let return_type = to_js_type operation.return_type in
  let name = 
    match operation.ident with 
    | None -> "" 
    | Some s -> s 
  in
  let to_js_arg (_, arg) =
    let type_, name, necessity =
      match arg with
      | `Optional((_, type_), name, _) -> (to_js_type type_), name, `Optional
      | `Variadic(type_, name) -> (to_js_type type_), name, `Variadic
      | `Fixed(type_, name) -> (to_js_type type_), name, `Fixed
    in
    Js.to_arg ~name ~type_ ~necessity
  in
  let args = List.map to_js_arg operation.arguments in
  let make special = Js.to_meth ~name ~return_type ~args ~special in
  let make_specaial = function
    | `Getter -> make `Getter
    | `Setter -> make `Setter
    | `Deleter -> make `Deleter
    | `Legacycaller -> make `Call
  in
  match operation.specials with
  | [] -> [ make `None ]
  | _ -> List.map make_specaial operation.specials

let to_js_meths operations = 
  List.map to_js_meth operations
  |> List.flatten

let to_callback_typedef operation =
  let open Webidl.Data in
  let return_type = to_js_type operation.return_type in
  let name = 
    match operation.ident with 
    | None -> "" 
    | Some s -> s 
  in
  let arg_to_type (_, arg) = 
    match arg with
    | `Optional((_, type_), _, _) -> (to_js_type type_)
    | `Variadic(type_, _) -> (to_js_type type_)
    | `Fixed(type_, _) -> (to_js_type type_)
  in
  let args = List.map arg_to_type operation.arguments in
  let is_variadic (_, arg) =
    match arg with
    | `Variadic(type_, _) -> true
    | _ -> false
  in
  let has_variadic = List.exists is_variadic operation.arguments in
  if has_variadic then
    let last = BatList.last args in
    let args, _ = BatList.split_at (List.length args -1) args in
    (name, `Variadic_callback(return_type, args, last))
  else
    (name, `Callback(return_type, args))