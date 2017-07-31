open Bs_str
open Bs_type

type variant

type _ pattern = 
  | Variant : string * (string * type_) option -> variant pattern (* | `A (b: c) ->*)
  | String : string -> string pattern (* | "" -> *)

type eval = [
  | `Variant of string
  | `String of string
  | `Ident of capital Bs_str.t list * uncapital Bs_str.t (* A.B.C.a *)
  | `Label of uncapital Bs_str.t
  | `Int of int
  | `Float of float
  | `Bool of bool
  | `Ident_with_type of uncapital Bs_str.t * type_
]

type eval_line = eval list
and let_line = uncapital Bs_str.t * expr (* let a = b c in *)
and 'a function_code = ('a pattern * expr) list
and expr =
  | Eval : eval_line -> expr
  | Let : let_line -> expr 
  | Function: _ function_code -> expr (* function p1 -> .. | p2 -> .. *)

type arg = [
  | `Label of uncapital Bs_str.t
  | `Optional of uncapital Bs_str.t
  | `Nolabel of uncapital Bs_str.t
  | `Unit
]

type t = {
  ident: uncapital Bs_str.t ;
  args: arg list ;
  let_lines: let_line list ;
  eval_line: eval_line
}

module Print = struct
  open Format

  let print_pattern (type p) pattern =
    match (pattern: p pattern) with
    | Variant(v, None) ->
      asprintf "`%s" v
    | Variant(v, Some (i, t)) ->
      asprintf "`%s (%s: %s)" v i (Bs_type.print t)
    | String s -> asprintf "\"%s\"" s

  let rec print_eval ppf eval =
    match (eval: eval) with
    | `Int i -> fprintf ppf "%i" i
    | `Float f -> fprintf ppf "%f" f
    | `Bool b -> fprintf ppf "%B" b
    | `Variant v -> fprintf ppf "`%s" v
    | `String s -> fprintf ppf "\"%s\"" s
    | `Label l -> fprintf ppf "~%s" (to_string l)
    | `Ident_with_type(i, t) -> fprintf ppf "(%s:%s)" (to_string i) (Bs_type.print t)
    | `Ident(modules, ident) ->
      let ident = to_string ident in
      if BatList.is_empty modules then
        fprintf ppf "%s" ident
      else
        let modules = List.map to_string modules in
        fprintf ppf "%s" ((String.concat "." modules) ^ "." ^ ident)

  let rec print_let_line ppf (ident, expr) =
    fprintf ppf "@[<v 2>let %s = %a in@]"
      (to_string ident) 
      print_expr expr 

  and print_expr ppf expr =
    let print_case ppf (pattern, expr) = 
      fprintf ppf "| %s -> %a" 
        (print_pattern pattern)
        print_expr expr
    in
    match expr with
    | Eval eval -> 
      fprintf ppf "@[<h>%a@]" (pp_print_list ~pp_sep:pp_print_space print_eval) eval
    | Let let_line -> 
      print_let_line ppf let_line
    | Function cases ->
      fprintf ppf "function@,%a@," 
        (pp_print_list print_case) cases

  let print_arg arg =
    match (arg: arg) with
    | `Label l -> asprintf "~%s" (to_string l)
    | `Unit -> "()"
    | `Optional o -> asprintf "?%s" (to_string o)
    | `Nolabel n -> to_string n

  let print_t ppf t =
    fprintf ppf "@[<v 2>let %s %s =@,%a@,@[<h>%a@]@]@,"
      (to_string t.ident)
      (List.map print_arg t.args |> String.concat " ")
      (pp_print_list print_let_line) t.let_lines
      (pp_print_list ~pp_sep:pp_print_space print_eval) t.eval_line

end

module Construct = struct

  let to_let_var ~var_name ~expr : let_line = 
    (to_uncapital var_name, expr)

  let to_variant_pattern ?(match_ident) ~variant =
    match match_ident with
    | None -> Variant(Util.escape variant, None)
    | Some(s, t) -> Variant(Util.escape variant, Some(Util.escape s, t))

  let to_str_pattern str =
    String(str)

  let to_eval_ident ?(modules = []) ident = 
    let modules = List.map to_capital modules in
    let ident = to_uncapital ident in
    `Ident(modules, ident)

  let to_eval_ident_with_type ident type_ = 
    let ident = to_uncapital ident in
    `Ident_with_type(ident, type_)

  let to_eval_label ident =
    `Label(to_uncapital ident)

  let to_eval_str str = `String str
  let to_eval_variant str = `Variant (Util.escape str)

  let to_eval evals = Eval evals

  let to_variant_case ?match_ident ~variant (expr: expr) =
    let pattern = to_variant_pattern ?match_ident ~variant in
    (pattern, expr)

  let to_str_case ~str ~(expr: expr) =
    let pattern = to_str_pattern str in
    (pattern, expr)

  let to_function ~cases =
    Function cases

  let to_label str =
    `Label(to_uncapital str)

  let to_optional str =
    `Optional(to_uncapital str)

  let to_nolabel str =
    `Nolabel(to_uncapital str)

  let to_let_def ident args let_lines eval_line : t =
    {ident = to_uncapital ident; args; let_lines; eval_line}

end

(* expose functions. others are hidden by mli *)
let print t = Print.print_t t
include Construct