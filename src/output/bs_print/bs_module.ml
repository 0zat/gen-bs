open Bs_str
open Bs_type

module Bs_ext = Bs_external
module Bs_let = Bs_let

type module_ = {
  name : capital Bs_str.t ;
  includes : capital Bs_str.t option ;
  ext_defs : Bs_ext.t list ;
  let_defs : Bs_let.t list ;
}

type t = module_

module Print = struct
  open Format

  let print_module module_ =
    let pp_external ppf ext_defs =
      let print ppf s = fprintf ppf "%s" (Bs_ext.print s) in
      pp_print_list print ppf ext_defs
    in
    let pp_let ppf lets_defs = 
      pp_print_list Bs_let.print ppf lets_defs
    in
    let includes =     
      BatOption.map to_string module_.includes
      |> BatOption.map_default (fun x -> "include " ^ x ) ""
    in
    asprintf 
      "@[<v 4> module %s = struct@,%s@,@,%a@,@,%a@]@,end"
      (to_string module_.name)
      includes
      pp_external module_.ext_defs
      pp_let module_.let_defs
end

module Construct = struct

  let to_module ~name ~includes ~ext_defs ~let_defs =
    let name = to_capital name in
    let includes = BatOption.map to_capital includes in
    {name; includes; ext_defs; let_defs}
end

(* expose functions. others are hidden by mli *)
let print module_ = Print.print_module module_
include Construct