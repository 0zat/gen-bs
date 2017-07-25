module Idl = Webidl.Data
open Idl
open Js

let print_error e = 
  Webidl.Parse.show_syntax_error e 
  |> print_string
  |> print_newline

let parse file =
  try
    Ok(Webidl.Parse.data_from_file ~strict:false file)
  with
  | Webidl.Parse.Syntax_error e ->
    Error e

let partition parse_results =
  let oks = BatList.filter_map (function Ok o -> Some o | _ -> None) parse_results in
  let errs  = BatList.filter_map (function Error e -> Some e | _ -> None) parse_results in
  (List.flatten oks), errs

let parse_from_dir dir =
  let oks, errs =
    Sys.readdir dir
    |> Array.map (Filename.concat dir)
    |> Array.map parse
    |> Array.to_list 
    |> partition
  in
  if List.length errs <> 0 then
    Error errs
  else
    Ok oks

let from_dir dir =
  match parse_from_dir dir with
  | Error errs -> List.iter print_error errs
  | Ok oks ->
    Webidl_to_js.to_js_input oks
    |> Js_to_bs.to_bs
    |> Bs.print
    |> print_string

