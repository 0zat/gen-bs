#!/usr/bin/env ocaml

let compile = "ocamlbuild -use-ocamlfind gen_bs.byte"
let install = "" 
let remove = ""
let clean = "ocamlbuild -clean"

let exec str =
  print_string ("setup.ml exec:" ^ str ^ "\n");
  let result = Sys.command str in
  if result <> 0 then
    exit result

let () =
  match Sys.argv.(1) with
  | "build" -> 
    exec compile
  | "install" -> exec install
  | "remove" -> exec remove
  | "clean" -> exec clean
  | _ -> (print_string "unkown option"; exit 1)



