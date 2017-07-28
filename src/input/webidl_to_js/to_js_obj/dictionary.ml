open Webidl.Data
open Js
open Operation

let member_to_js_attr (extAttr, (member : Webidl.Data.dictionary_member)) =
  let type_ = Idl_type.to_js_type (snd member.type_with_ext) in
  let is_required = member.is_required in
  let name = member.ident in
  Js.to_attr ~name ~type_ ~is_readonly:false ~is_required

let to_js_obj dictionary =
  let attrs = List.map member_to_js_attr dictionary.dictionary_members in
  Js.to_obj 
    ~name: dictionary.ident 
    ~inherits: dictionary.inheritance
    ~attrs: attrs 
    ~meths: []
    ~constants: []

let append_partial patrials dictionaries =
  let append_partial_aux patrials dictionary =
    let partials = List.filter (fun x -> x.name = dictionary.name) patrials in
    Js.append_objs (dictionary :: partials) dictionary.name dictionary.inherits
  in
  List.map (append_partial_aux patrials) dictionaries

let to_js_objs patrials dictionaries =
  let dictionaries = List.map to_js_obj dictionaries in
  let patrials = List.map to_js_obj patrials in
  append_partial patrials dictionaries
