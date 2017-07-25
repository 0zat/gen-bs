module Idl = Webidl.Syntax.Ast

let to_array dst = `Array dst 
let to_promise dst = `Promise dst 
let to_null dst = `Null dst 

let rec to_js_union = 
  let convert_flatten union = List.map to_js_union union |> List.flatten in
  function
  | (`Union union) -> convert_flatten union 
  | (`NonAny (_, non_any)) -> to_js_type (non_any :> Idl.return_type)
  | (`Nullable (`Union union)) -> convert_flatten union |> List.map to_null

and to_js_type = 
  let conv dst = to_js_union dst in
  let convert_flatten union = List.map conv union |> List.flatten in
  function
  | (`Union union) -> to_js_union (`Union union)
  | (`Nullable (`Union union)) -> convert_flatten union |> List.map to_null
  | (`Sequence (_, `Union union)) -> convert_flatten union |> List.map to_array
  | (`Promise (`Union union)) -> convert_flatten union |> List.map to_promise
  | (`FrozenArray (_, `Union union)) -> convert_flatten union |> List.map to_array
  | (`Record (key, (_, `Union u))) -> [`Any]
  | `Boolean -> [`Boolean]
  | `Byte | `Octet | `Unsigned _ | `Short | `Long | `LongLong -> [`Int]
  | `Unrestricted _ | `Float | `Double -> [`Float]
  | `ByteString | `DOMString | `USVString -> [`String]
  | `Object -> [`Any]
  | `Ident i -> [`Obj i]
  | `Sequence (_, s) -> List.map to_array (to_js_type (s :> Idl.return_type ))
  | `Promise p -> List.map to_promise (to_js_type p)
  | `Nullable n -> List.map to_null (to_js_type (n :> Idl.return_type))
  | `Void -> [`Unit]
  | #Idl.buffer as buffer -> [buffer]
  | `Any
  | `Record (_, _)
  | `Error
  | `DomException
  | `FrozenArray _ -> [`Any]

let to_js_type dst =
  match to_js_type (dst :> Idl.return_type) with
  | [] -> failwith "internal error"
  | [non_union] -> (non_union :> Js.type_)
  | other -> `Union other 