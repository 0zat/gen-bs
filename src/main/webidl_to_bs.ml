module Idl = Webidl.Data
open Idl
open Js
open Webidl.Parse

exception Web_IDL_error of string

let print_error e = 
  Webidl.Parse.show_syntax_error e 

let parse_from_file file =
  try
    Webidl.Parse.data_from_file ~strict:false file
  with
  | Webidl.Parse.Syntax_error e ->
    raise (Web_IDL_error (print_error e))

let partition parse_results =
  let open BatResult in
  let oks = BatList.filter_map (function Ok o -> Some o | _ -> None) parse_results in
  let errs  = BatList.filter_map (function Bad e -> Some e | _ -> None) parse_results in
  (List.flatten oks), errs

let parse_from_dir dir =
  let oks, errs =
    Sys.readdir dir
    |> Array.map (Filename.concat dir)
    |> Array.map (BatResult.catch parse_from_file)
    |> Array.to_list 
    |> partition
  in
  if List.length errs <> 0 then
    let msgs = List.map Printexc.to_string errs in
    raise (Web_IDL_error (String.concat "\n" msgs))
  else
    oks

let from_dir version external_types dir =
  parse_from_dir dir
  |> Webidl_to_js.to_js_input
  |> Js_to_bs.to_bs external_types
  |> Bs.print version

let from_file version external_types file =
  parse_from_file file
  |> Webidl_to_js.to_js_input 
  |> Js_to_bs.to_bs external_types
  |> Bs.print version
