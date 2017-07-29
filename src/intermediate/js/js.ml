type primitive = [
  | `Int
  | `Float
  | `String
  | `Boolean
  | `Unit
  | `Obj of string 
  | `Any
  | `Enum of string list
  | Bs_type.buffer
]

type constant = [
  | `Int of int
  | `Float of float
  | `Boolean of bool
  | `Null
]

type 'type_ composed = [
  | `Null of 'type_
  | `Undef of 'type_
  | `Array of 'type_
  | `Promise of 'type_
  | `Union of 'type_ list
  | `Callback of 'type_ * 'type_ list
]

type type_ = [
  | type_ composed
  | primitive
]

type composed_type = type_ composed

type js_arg = {
  name : string ;
  type_ : type_ ;
  necessity : [ `Optional | `Fixed | `Variadic ]
}

type js_attr = {
  name : string ;
  type_ : type_ ; 
  is_readonly : bool ;
  is_required : bool ;
}

type js_meth = {
  special : [`Getter | `Setter | `Deleter | `Call | `None ]  ;
  name : string ;
  args : js_arg list ;
  return_type : type_ ; 
}

type js_obj = {
  name : string ;
  inherits : string option ;
  constants : (string * constant) list;
  attrs : js_attr list ;
  meths : js_meth list ;
}

let to_attr ~name ~type_ ~is_readonly ~is_required =
  {name; type_; is_readonly; is_required}

let to_arg ~name ~type_ ~necessity = 
  {name; type_; necessity}

let to_meth ~name ~special ~args ~return_type =
  {name; special; args; return_type}

let to_obj ~name ~inherits ~attrs ~meths ~constants =
  {name; inherits; attrs; meths; constants} 

let map_flatten f dst = List.map f dst |> List.flatten 
let filter_attr obj = map_flatten (fun x -> x.attrs) obj
let filter_method obj = map_flatten (fun x -> x.meths) obj
let filter_const obj = map_flatten (fun x -> x.constants) obj

let args_to_types args =
  let aux (js_arg: js_arg) = 
    if js_arg.necessity = `Optional then
      `Undef js_arg.type_
    else
      js_arg.type_ 
  in  
  List.map aux args

let args_to_names args =
  let aux (js_arg: js_arg) = js_arg.name in  
  List.map aux args

let append_objs objs name inherits =
  let attrs = filter_attr objs in
  let meths = filter_method objs in
  let constants = filter_const objs in
  {name; inherits ; attrs ;  meths; constants}
