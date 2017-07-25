open Js

module From_meth = struct

  let to_external owner_name js_meth =
    let owner = Js_args.to_owner_arg owner_name in
    let external_candidates = [
      Variadic.to_external_if_variadic owner_name js_meth ;
      Some(Js_meth.to_external owner js_meth)
    ]
    in
    List.find BatOption.is_some external_candidates 
    |> BatOption.get

  let to_let owner_name js_meth =
    let let_candidates = [
      Variadic.to_let_if_variadic owner_name js_meth;
      Enum.to_let_if_enum owner_name js_meth;
      Union.to_let_if_union owner_name js_meth;
      Optional.to_let_if_optional owner_name js_meth
    ]
    in
    BatList.filter_map (fun x -> x) let_candidates
    
end

let to_module make_constr js_obj = 
  let owner = Js_args.to_owner_arg js_obj.name in
  let attr_externals = 
    List.map (Js_attr.to_attr owner) js_obj.attrs 
    |> List.flatten 
  in
  let meth_externals =
    List.map (From_meth.to_external js_obj.name) js_obj.meths
  in
  let meth_lets =
    List.map (From_meth.to_let js_obj.name) js_obj.meths
    |> List.flatten 
  in
  let constr = 
    match make_constr with
    | None -> []
    | Some all_obj -> [Constr_meth.to_external all_obj js_obj]
  in
  Bs_module.to_module 
    ~name: js_obj.name
    ~includes: js_obj.inherits
    ~ext_defs: (constr @ attr_externals @ meth_externals)
    ~let_defs: meth_lets

let to_modules ?make_constr js_objs = 
  List.map (to_module make_constr) js_objs
