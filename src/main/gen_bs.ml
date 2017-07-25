let desc_dir =
  "generate bucklescript code from Web IDL files in a directory"


let speclist = [
  ("-dir", (Arg.String Webidl_to_bs.from_dir), desc_dir)
]

let () = Arg.parse speclist (fun _ -> ()) "options: "