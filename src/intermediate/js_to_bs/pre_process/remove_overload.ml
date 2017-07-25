open Js

let to_union_arg args_list =
  let types_list = List.map (List.map (fun (x: js_arg) -> x.type_)) args_list in
  match types_list with
  | [] -> failwith "empty arg called"
  | hd :: tl ->
    assert(List.for_all (fun x -> List.length x = List.length hd) tl);
    let get_ith_tl i = List.map (fun x -> List.nth x i) tl in
    let to_union_or_solo types =
      match types with
      | [] -> failwith "empty type called"
      | [hd] -> hd
      | hd :: tl -> `Union types
    in
    let types =
      List.mapi (fun i x -> x :: (get_ith_tl i)) hd
      |> List.map BatList.unique
      |> List.map to_union_or_solo
    in
    let convert_arg i (arg: js_arg) = {arg with type_ = List.nth types i} in
    List.mapi convert_arg (List.hd args_list)

(* expect meths' arg lengths are the same and not empty*)
let to_union_meth meths =
  let args_list = List.map (fun x -> x.args) meths in
  let union_args = to_union_arg args_list in
  let base = List.hd meths in
  {base with args = union_args}

(* expect meths' arg lengths are the same*)
let is_one_arg_diff meths =
  let args_list = List.map (fun x -> x.args) meths in
  match args_list with
  | [] -> false
  | hd :: tl -> 
    let remove_in_hd args =
      List.filter (fun x -> not (List.mem x hd)) args
    in
    List.map remove_in_hd tl
    |> List.for_all (fun x -> List.length x = 1)

let replace_if_one_arg_diff meths =
  if is_one_arg_diff meths then
    [to_union_meth meths]
  else
    meths

let classify_by_arg_len meths =
  let to_key meth = List.length meth.args in
  Util.classify to_key meths

let classify_by_meth_name meths =
  let to_key (meth: js_meth) = meth.name in
  Util.classify to_key meths

let from_meths meths =
  let classified_by_name = classify_by_meth_name meths in
  let map same_name_meths =
    if List.length same_name_meths <= 1 then
      same_name_meths
    else if (List.hd same_name_meths : js_meth).name = "" then
      same_name_meths
    else
      let classified_by_len = classify_by_arg_len same_name_meths in
      let meths = 
        List.map replace_if_one_arg_diff classified_by_len 
        |> List.flatten
      in
      let _, max =
        let cmp x y = List.length x.args - List.length y.args in
        BatList.min_max ~cmp meths
      in
      [max]
  in
  List.map map classified_by_name
  |> List.flatten 

let from_objs objs =
  let from_obj js_obj =
    {js_obj with meths = from_meths js_obj.meths}
  in
  List.map from_obj objs