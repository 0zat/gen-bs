let print_version () = print_endline "0.0.0"

let desc_dir =
  "generate bucklescript code from Web IDL files in a directory"


let speclist = [
  ("-dir", (Arg.String Webidl_to_bs.from_dir), desc_dir);
  ("-version", Arg.Unit print_version, "show version") 
]

let desc = "available options: -dir <dir>, -version"

let proc_anonymous_arg arg = print_endline desc

let () = Arg.parse speclist proc_anonymous_arg desc