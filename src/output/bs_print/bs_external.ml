open Bs_str
open Bs_type

type return_annot =
  | Null_to_opt
  | Undefined_to_opt
  | Null_undefined_to_opt
  | Identity

type annot = 
  | Obj
  | Get
  | Set
  | New
  | Get_index
  | Set_index
  | Send
  | Val
  | Module of string
  | Splice
  | Return of return_annot

type external_func = {
  name : uncapital Bs_str.t ;
  args : Bs_args.t ;
  return_type : Bs_type.t ; (* null or undef is dealed in annot *)
  action : string ;
  annot : annot list ;
}

type t = external_func

module Print = struct
  let print_return_annot = function
    | Null_to_opt -> "[@@bs.return null_to_opt]"
    | Undefined_to_opt -> "[@@bs.return undefined_to_opt]"
    | Null_undefined_to_opt -> "[@@bs.return null_undefined_to_opt]"
    | Identity -> ""

  let print_annot = function
    | Get -> "[@@bs.get]"
    | Set -> "[@@bs.set]"
    | New -> "[@@bs.new]"
    | Get_index -> "[@@bs.get_index]"
    | Set_index -> "[@@bs.set_index]"
    | Send -> "[@@bs.send]"
    | Val -> "[@@bs.val]"
    | Module m -> Printf.sprintf "[@@bs.module \"%s\"]" m
    | Splice -> "[@@bs.splice]"
    | Return annot -> print_return_annot annot 
    | Obj -> "[@@bs.obj]"

  let print_annot_list annot_list = 
    List.map print_annot annot_list
    |> String.concat " " 

  let print_func ext_func =
    Printf.sprintf "external %s : %s -> %s = \"%s\" %s"
      (to_string ext_func.name)
      (List.map Bs_args.print ext_func.args |> String.concat " -> ")
      (Bs_type.print ext_func.return_type)
      ext_func.action
      (print_annot_list ext_func.annot)

end

module Construct = struct
  let to_external_expr ~name ~args ~return_type ~action ~annot =
    let name = to_uncapital name in
    {name; args; return_type; action; annot}
end

(* expose functions. others are hidden by mli *)
let print ext_func = Print.print_func ext_func
include Construct