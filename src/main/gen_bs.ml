open Description

let version = "0.1.0"

let () = 
  let is_dir = ref false in
  let action = ref `webidl in
  let external_type_file = ref None in
  let output = ref None in
  let trace = ref false in
  let speclist = [
    ("-t", (Arg.String (fun x -> external_type_file := Some x)), desc_external_type);
    ("-o", (Arg.String (fun x -> output := Some x)), desc_output);
    ("-d", (Arg.Set is_dir), desc_dir);
    ("-T", (Arg.Set trace), desc_trace);
    ("-v", Arg.Unit (fun _ -> action := `version), desc_version) ;
    ("--type", (Arg.String (fun x -> external_type_file := Some x)), desc_external_type);
    ("--output", (Arg.String (fun x -> output := Some x)), desc_output);
    ("--dir", (Arg.Set is_dir), desc_dir);
    ("--trace", (Arg.Set trace), desc_trace);
    ("--version", Arg.Unit (fun _ -> action := `version), desc_version) ;
  ]
  in
  let proc_anonymous_arg arg =
    Printexc.record_backtrace !trace;
    let external_types =
      BatOption.map_default Read_external_types.read [] !external_type_file 
    in
    let output =
      BatOption.map_default open_out stdout !output 
    in
    match !action with
    | `version -> print_endline version
    | `webidl -> 
      let open Webidl_to_bs in
      if !is_dir then
        let result = from_dir version external_types arg in
        Printf.fprintf output "%s" result
      else
        let result = from_file version external_types arg in
        Printf.fprintf output "%s" result
  in
  try
    Arg.parse speclist proc_anonymous_arg desc
  with
  | except -> 
    if !trace then Printexc.print_backtrace stderr;
    let msg = Printexc.to_string except in
    Printf.eprintf "%s\n" msg;
    exit 1