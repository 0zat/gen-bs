(*^
  "an example the json format is the following.\n" ^
  "[\n" ^
  "  [<type name in gen-bs>, <cast type>],\n" ^
  "  [\"_ExampleType\", Js.Array.t]\n" ^
  "]"*)

open Yojson.Basic

exception Malform of string

let sep_cast_type cast_type =
  let separeted = BatString.nsplit cast_type "." in
  let last = BatList.last separeted in
  if last = "" then
    raise (Malform cast_type)
  else
    let modules, _ = BatList.split_at (List.length separeted -1) separeted in
    (modules, last)

let read_pair json =
  match json with
  | `List [`String bs_type; `String cast_type] -> 
    let modules, ident = sep_cast_type cast_type in
    (bs_type, modules, ident)
  | _ -> raise (Malform (Yojson.Basic.to_string json))

let read file =
  let json = from_file file in
  match json with
  | `List l -> List.map read_pair l
  | _ -> raise (Malform "malform external type")

