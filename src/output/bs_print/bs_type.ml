open Bs_str
type buffer = Webidl.Syntax.Ast.buffer

type primitive = [
  | `Int
  | `Float
  | `String
  | `Unit
  | `Underbar 
  | `Object
  | `Any
  | `Ocaml_bool
  | `Js_boolean
  | `Variable of uncapital Bs_str.t (* 'a *)
  | `As of string
  | buffer
]

type 'type_ composed_aux = [
  | `Array of 'type_
  | `Ident of 'type_ list * capital Bs_str.t list * uncapital Bs_str.t 
  (*           ('a, 'b)   Module_name1.Module_name2.ident_name   *)
  | `Option of 'type_ 
  | `Null of 'type_ 
  | `Undef of 'type_ 
  | `Promise of 'type_
  | `Func of 'type_ list
  | `Tuple of 'type_ list
]

type type_ = [
  | primitive
  | type_ composed_aux
]

type composed = type_ composed_aux

type t = type_

module Print = struct

  let get_uniq_var =
    let i = ref 0 in
    let get () =
      incr i;
      "'a" ^ (string_of_int !i)
    in
    get

  let print_buffer : buffer -> string = function
    | `ArrayBuffer -> "Js_typed_array.ArrayBuffer.t"
    | `DataView -> "Js_typed_array.DataView.t"
    | `Int8Array -> "Js_typed_array.Int8Array.t"
    | `Int16Array -> "Js_typed_array.Int16Array.t"
    | `Int32Array -> "Js_typed_array.Int32Array.t"
    | `Uint8Array -> "Js_typed_array.Uint8Array.t"
    | `Uint16Array -> "Js_typed_array.Uint16Array.t"
    | `Uint32Array -> "Js_typed_array.Uint32Array.t"
    | `Uint8Clampedarray -> "Js_typed_array.Uint8ClampedArray.t"
    | `Float32Array -> "Js_typed_array.Float32Array.t"
    | `Float64Array -> "Js_typed_array.Float64Array.t"

  let print_primitive : primitive -> string = function
    | `Int -> "int"
    | `Float -> "float"
    | `String -> "string"
    | `Unit -> "unit"
    | `Underbar -> "_"
    | `Object -> "< .. > Js.t"
    | `Ocaml_bool -> "bool"
    | `Js_boolean -> "Js.boolean"
    | `Variable v -> "'" ^ (to_string v)
    | `Any -> get_uniq_var ()
    | `As str -> "_ [@bs.as \"" ^ str ^ "\"]"
    | #buffer as b -> print_buffer b

  let rec print_composed : composed -> string = function
    | `Array a -> Printf.sprintf "%s array" (print_type a)
    | `Option o -> Printf.sprintf "%s option" (print_type o)
    | `Null n -> Printf.sprintf "%s Js.null" (print_type n)
    | `Undef u -> Printf.sprintf "%s Js.undefined" (print_type u)
    | `Promise p -> Printf.sprintf "%s Js.Promise.t" (print_type p)
    | `Func types -> 
      Printf.sprintf "(%s [@bs])" (List.map print_type types |> String.concat " -> ")
    | `Tuple types -> Printf.sprintf "(%s)" (List.map print_type types |> String.concat "*")
    | `Ident(variables, modules, ident_name) -> 
      let variables = 
        match variables with
        | [] -> ""
        | _ -> List.map print_type variables 
               |> String.concat ", " 
               |> Printf.sprintf "(%s) "
      in
      let modules = List.map to_string modules in
      let ident_name = to_string ident_name in
      let ident = String.concat "." (modules @ [ident_name]) in
      Printf.sprintf "%s%s" variables ident

  and print_type : 'a. ([< type_] as 'a) -> string = function
    | #primitive as p -> print_primitive p
    | #composed as c -> print_composed c

end

module Construct = struct

  let to_ident ?(variables=[]) ?(modules=[]) str : type_ =
    let modules = List.map to_capital modules in
    `Ident (variables, modules, to_uncapital str)

  let to_variable ~variable = `Variable (to_uncapital variable)

end

(* expose functions. others are hidden by mli *)
let print type_ = Print.print_type type_
include Construct
