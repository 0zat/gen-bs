module Idl = Webidl.Data


let get_dictionary_obj (idl_list : Idl.definitions) =
  let dictionaries = BatRefList.empty () in
  let partials = BatRefList.empty () in
  let () =
    let aux = function
      | _, `Partial `Dictionary dictionary -> BatRefList.add partials dictionary
      | _, `Dictionary dictionary -> BatRefList.add dictionaries dictionary
      | _, _ -> ()
    in
    List.iter aux idl_list
  in
  let partials = BatRefList.to_list partials in
  let dictionaries = BatRefList.to_list dictionaries in
  Dictionary.to_js_objs partials dictionaries
  
let get_interface_obj (idl_list : Idl.definitions) =
  let interfaces = BatRefList.empty () in
  let partials = BatRefList.empty () in
  let implements = BatRefList.empty () in
  let () =
    let aux = function
      | _, `Partial (`Interface interface) -> BatRefList.add partials interface
      | _, `Interface interface -> BatRefList.add interfaces interface
      | _, `Implements implement -> BatRefList.add implements implement
      | _, _ -> ()
    in
    List.iter aux idl_list
  in
  let partials = BatRefList.to_list partials in
  let interfaces = BatRefList.to_list interfaces in
  let implements = BatRefList.to_list implements in
  Interface.to_js_objs implements partials interfaces 

let get_namespece_obj (idl_list : Idl.definitions) =
  let namespaces = BatRefList.empty () in
  let () =
    let aux = function
      | _, `Namespace namespace -> 
        BatRefList.add namespaces namespace
      | _, _ -> ()
    in
    List.iter aux idl_list
  in
  List.map Namespace.to_js_obj (BatRefList.to_list namespaces)

let get_callback_obj (idl_list : Idl.definitions) =
  let interfaces = BatRefList.empty () in
  let () = 
    let aux = function
      | _, `Callback  (`Interface interface) -> BatRefList.add interfaces interface
      | _, _ -> () 
    in
    List.iter aux idl_list
  in
  List.map Interface.to_js_obj (BatRefList.to_list interfaces)

let get_typedef (idl_list : Idl.definitions) =
  let types = BatRefList.empty () in
  let () =
    let aux = function
      | _, `Typedef ((_, type_), name) ->
        let type_ = Idl_type.to_js_type type_ in 
        BatRefList.add types (name, type_)
      | _, `Enum (name, enum) -> BatRefList.add types (name, `Enum enum)
      | _, `Callback (`Operation operation) -> 
        let callback_type = Operation.to_callback_typedef operation in
        BatRefList.add types callback_type
      | _, _ -> () 
    in
    List.iter aux idl_list 
  in
  BatRefList.to_list types

let predefined_types = [
  ("DOMTimeStamp", `Int);
  ("ArrayBufferView", `Union [
      `DataView ;
      `Int8Array ;
      `Int16Array ;
      `Int32Array ;
      `Uint8Array ;
      `Uint16Array ;
      `Uint32Array ;
      `Uint8Clampedarray ;
      `Float32Array ;
      `Float64Array ;
    ])
]

let to_js_input (idl_list : Webidl.Data.definitions) = 
  let dicionary_objs = get_dictionary_obj idl_list in
  let namcepsce_objs = get_namespece_obj idl_list in
  let interface_objs = get_interface_obj idl_list in
  let objs = namcepsce_objs @ interface_objs in
  let callback_objs = get_callback_obj idl_list in
  let types = predefined_types @ get_typedef idl_list in
  let objs_with_constr = dicionary_objs @ callback_objs in
  Js_to_bs.to_input 
    ~types
    ~objs 
    ~objs_with_constr