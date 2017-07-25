open Js

type js_input = 
  {
    types : (string * type_) list ;
    objs : js_obj list ;
    objs_with_constr : js_obj list ;
  }

let to_input ~types ~objs ~objs_with_constr = 
  { types; objs; objs_with_constr}

let sort_modules modules =
  let rec sort_modules_aux ans modules =
    let has_parent_in modules module_ =
      let open Bs_module in
      match module_.includes with
      | None -> true
      | Some name ->
        let module_names = List.map (fun x -> x.name) modules in
        List.mem name module_names 
    in 
    match modules with
    | [] -> List.rev ans
    | list -> 
      let sat, not_sat = List.partition (has_parent_in ans) list in
      if not (sat = [] && not_sat <> []) then
        sort_modules_aux (List.rev_append sat ans) not_sat
      else if (sat = []) && (not_sat = []) then
        List.rev ans
      else
        failwith "error fail"
  in
  sort_modules_aux [] modules

let get_replaced_objs js_inputs =
  let types = Replace_types.replace_typedef js_inputs.types in
  let objs = Replace_types.replace_objs types js_inputs.objs in
  let objs_with_constr = Replace_types.replace_objs types js_inputs.objs_with_constr in
  let objs = Remove_overload.from_objs objs in
  let objs_with_constr = Remove_overload.from_objs objs_with_constr in
  (objs, objs_with_constr)

let get_global name =
  Bs.to_global_value name (Bs_type.to_ident name)

let to_bs js_inputs = 
  let objs, objs_with_constr = get_replaced_objs js_inputs in
  let all_objs = objs @ objs_with_constr in
  let modules = Js_obj.to_modules objs in
  let modules_with_constr = Js_obj.to_modules ~make_constr:all_objs objs_with_constr in
  let sorted_modules = sort_modules (modules @ modules_with_constr) in
  let type_defs = Js_obj_to_typedef.from_objs all_objs in
  let predefined_code = Internal_code.get () in
  Bs.to_bs
    ~predefined_code
    ~type_defs
    ~modules: sorted_modules 