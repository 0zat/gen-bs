open Webidl.Data
open Idl_type
open Js

let to_js_obj (namespece: namespace) =
  let meths = BatRefList.empty () in
  let attrs = BatRefList.empty () in
  let convert_member (_, member) =
    match (member: Webidl.Data.namespace_member) with
    | `Operation operation -> BatRefList.add meths (Operation.to_js_meth operation)
    | `Attribute {is_readonly; type_with_ext; name; _} -> 
      let type_ = to_js_type (snd type_with_ext) in
      let attr = Js.to_attr ~name ~is_readonly ~type_ ~is_required:true in
      BatRefList.add attrs attr
  in
  List.iter convert_member namespece.namespace_members;
  Js.to_obj 
    ~name: namespece.ident 
    ~inherits: None
    ~attrs: (BatRefList.to_list attrs)
    ~meths: (BatRefList.to_list meths |> List.flatten)