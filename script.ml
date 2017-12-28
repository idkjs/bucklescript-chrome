(** Types that represent the chrome extensions API **)
type ctype =
  | CObject of
      string (* name *)
    * (string * string) list (* list of fields consisting name and type *)
  | CBool
  | CString
  | CFn of
      string (* param type *)
    * string (* return type *)
type cmethod =
  CMethod of string * (string * ctype) list (* name of method, and args with names *)
type cevent =
  CEvent of string (* name of event *)
type cmodule =
  (* a chrome module, has the implicit chrome. prefix *)
    CModule of
        string       (* name of module   *)
      * ctype list   (* declared types   *)
      * cmethod list (* declared methods *)
      * cevent list  (* declared events  *)

(** Helper constants for building output *)
let kw_bs_obj = "[@@bs.obj]"
let kw_bs_val = "[@@bs.val]"
let kw_bs = "[@bs]"
let kw_class = "class"
let kw_colon = ":"
let kw_empty_string = "\"\""
let kw_end = "end"
let kw_equals = "="
let kw_external = "external"
let kw_method = "method"
let kw_module = "module"
let kw_object = "object"
let kw_struct = "struct"
let kw_type = "type"
let kw_unit = "unit"

(* Helpers for pretty printing code *)
let space_between = String.concat " "
let line_between = String.concat "\n"
let quote s = "\"" ^ s ^ "\""

let string_of_ctype ctype = match ctype with
  | CBool -> "Js.boolean"
  | CString -> "string"
  | CFn(x, y) -> x ^ " -> " ^ y
  | CObject (ty, _) -> (String.uncapitalize_ascii ty)

let codegen_fields fields =
  let c (name, ctype) = space_between [kw_method; name; kw_colon; ctype] in
  line_between (List.map c fields)

let codegen_type ctype = match ctype with
  | CBool | CString | CFn _ -> "" (* primitive don't need to be codegen-ed *)
  | CObject (name, fields) ->
  let uncap_name = String.uncapitalize_ascii name in
  let params = List.map (fun (param, ctype) -> "?" ^ param ^ ":" ^ (ctype)) (fields) in
  let fn_ty = String.concat " -> " (params @ [kw_unit; uncap_name]) in
  line_between [
    space_between [kw_class; kw_type; "_" ^ uncap_name; kw_equals; kw_object ]
  ; codegen_fields fields
  ; space_between [kw_end; kw_bs]
  ; space_between [kw_type; uncap_name; kw_equals; "_" ^ uncap_name; "Js.t"]
  ; space_between [kw_external; "mk" ^ name; kw_colon; fn_ty; kw_equals; kw_empty_string; kw_bs_obj]
  ]

(* Given a list of args and a return type, generated a function signature.
 * All args are assumed to be optional. *)
let gen_fn_sig args ret =
  let collect_args (arg, ctype) = "?" ^ arg ^ kw_colon^ ctype
  in
  String.concat " -> " ((List.map collect_args args) @ [kw_unit; string_of_ctype ret])

(* Given a name of a function parameter and it's type, generate code if it is an object:
 * 1. class type for the new object
 * 2. Js.t wrapping the class type
 * 3. function to make instance of type
 * *)
let codegen_param mtd (name, ctype) = match ctype with
| CBool | CString | CFn _ -> ""
| CObject (ty, props) ->
  let gen_method (param, ctype) =
    space_between [kw_method; param; kw_colon; ctype]
  in
  let p = mtd ^ (String.capitalize_ascii name) in
  line_between [
    space_between [kw_class; kw_type; "_" ^ p; kw_equals; kw_object]
  ; (line_between (List.map gen_method props))
  ; space_between [kw_end; kw_bs]
  ; space_between [kw_type; p; kw_equals; "_" ^ p; "Js.t"]
  ; space_between [
      kw_external
    ; "mk" ^ (String.capitalize_ascii p); kw_colon; gen_fn_sig props (CObject (p, []))
    ; kw_equals
    ; kw_empty_string
    ; kw_bs_obj
    ]
  ]

(* for each method arg, if it's a complicated object, we generate a new type *)
let codegen_method module_name (CMethod (method_name, params)) =
  let collect_params (param, ctype) = match ctype with
    | CBool | CString -> string_of_ctype ctype
    | CFn _ -> "(" ^ string_of_ctype ctype ^ ")"
    | CObject _ -> method_name ^ (String.capitalize_ascii param)
  in
  let method_sig = String.concat " -> " ((List.map collect_params params) @ ["unit"]) in
  let params_type = line_between (List.map (codegen_param method_name) params) in
  line_between [
    params_type
  ; space_between [
      kw_external
    ; method_name
    ; kw_colon
    ; method_sig
    ; kw_equals
    ; quote method_name
    ; "[@@bs.scope " ^ quote "chrome" ^ ", " ^ quote module_name ^ "]"
    ; kw_bs_val
    ]
  ]

(* generates code for a module *)
let codegen_module (CModule (name, types, methods, events)) =
  let module_name = String.capitalize_ascii name in
  let type_code = List.map codegen_type types in
  let method_code = List.map (codegen_method name) methods in
  line_between [
    space_between [ kw_module; module_name; kw_equals; kw_struct; ]
  ; line_between (type_code @ [""] @ method_code)
  ; kw_end
  ]

let accountinfo = CObject("AccountInfo", [("id", "string")])
let _ =
  let m = CModule(
      "identity"
    , [accountinfo]
    , [CMethod(
        "getAuthToken"
      , [("details"
         , CObject(
             "getAuthTokenOptions", [
               ("interactive", "Js.boolean")
             ; ("account", "accountInfo")
             ; ("scopes", "string list")
             ]))
        ; ("callback", CFn("string", "'a"))
        ])]
    , []) in
  let gen = codegen_module m in
  print_endline gen
