open Webidl.Data
open Idl_type
open Js

let stringifier = 
  let name = "toString" in
  let args = [] in
  let return_type = `String in 
  let special = `None in
  Js.to_meth ~name ~args ~return_type ~special 

let to_js_const value =
  match value with
  | `Bool b -> `Boolean b
  | `Float f -> `Float f 
  | `Int i -> `Int i 
  | `Null -> `Null

let to_js_obj interface =
  let attrs = BatRefList.empty () in
  let meths = BatRefList.empty () in
  let consts = BatRefList.empty () in
  let member_to_js (extAttr, interface_member) = 
    match (interface_member : Webidl.Data.interface_member) with 
    | `Stringifier _ -> BatRefList.add meths [stringifier]
    | `Iterable _
    | `Maplike _
    | `Setlike _ -> () (*STUB*)
    | `Const (type_, name, value) -> BatRefList.add consts (name, to_js_const value)
    | `Attribute {is_readonly; type_with_ext; name; _} ->
      if not (String.contains name '-') then
        let type_ = to_js_type (snd type_with_ext) in
        let attr = Js.to_attr ~name ~is_readonly ~type_ ~is_required:true in
        BatRefList.add attrs attr
    | `Operation operation -> 
      BatRefList.add meths (Operation.to_js_meth operation)
  in
  List.iter member_to_js interface.interface_members;
  Js.to_obj 
    ~name: interface.ident 
    ~inherits: interface.inheritance
    ~attrs: (BatRefList.to_list attrs)
    ~meths: (BatRefList.to_list meths |> List.flatten)
    ~constants: (BatRefList.to_list consts)

let append_partial partials interface =
  let partials = List.filter (fun x -> x.name = interface.name) partials in
  Js.append_objs (interface :: partials) interface.name interface.inherits

let append_implements implements interfaces interface =
  let rec gather_implements ans implements names =
    match names with
    | [] -> ans
    | _ ->
      let new_names = 
        List.filter (fun (x, y) -> List.mem x names) implements
        |> List.map snd
      in
      let new_ans = List.rev_append names ans |> BatList.unique in
      gather_implements new_ans implements new_names
  in
  let implements = gather_implements [] implements [interface.name] in
  let get_interface name =
    try 
      List.find (fun x -> x.name = name) interfaces
    with
    | Not_found -> failwith (name ^ " not found")
  in
  let implements = List.map get_interface implements in
  Js.append_objs implements interface.name interface.inherits

let to_js_objs implements partials interfaces =
  let interfaces = List.map to_js_obj interfaces in
  let partials = List.map to_js_obj partials in
  let interfaces = List.map (append_partial partials) interfaces in 
  List.map (append_implements implements interfaces) interfaces 

