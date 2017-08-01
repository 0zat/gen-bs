type type_def = Bs_type.t * Bs_type.t option

type bs = {
  predefined_code : string ;   (* fixed internal use code *)
  type_defs : type_def list;
  modules : Bs_module.t list ;
}

module Print = struct

  let print_type_def (name, type_) =
    let name = Bs_type.print name in
    match type_ with
    | None -> Printf.sprintf "%s" name
    | Some type_ -> Printf.sprintf "%s = %s" name (Bs_type.print type_)

  let print_global (ident, name, type_) =
    Printf.sprintf "external %s: %s = \"%s\" [@@bs.val]" 
      (Bs_str.to_string ident)
      (Bs_type.print type_)
      name 

  let print_bs bs =
    let print_list sep f dst =
      List.map f dst
      |> String.concat sep
    in
    let modules = List.map Bs_module.print bs.modules |> String.concat "\n" in
    let type_defs = "type " ^ (print_list "\nand " print_type_def bs.type_defs) in
    Printf.sprintf 
      "
(* internal use *)
%s

(* types of javascript objects*)
%s

(* modules *)
%s
"
      bs.predefined_code
      type_defs
      modules
end

module Construct = struct

  let to_typedef ~type_ ~type_opt =
    (type_, type_opt)

  let to_global_value ~name ~type_ =
    let ident = Bs_str.to_uncapital name in
    (ident, name, type_) 

  let to_bs ~predefined_code ~type_defs ~modules =
    {predefined_code; type_defs; modules}

end

(* expose functions. others are hidden by mli *)
let print bs = Print.print_bs bs
include Construct