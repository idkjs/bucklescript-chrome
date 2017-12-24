(** Types that represent the chrome extensions API **)
type ctype =
  | CObject of string * (string * string) list (* a type exported by chrome extensions *)
  | CBool
  | CString
  | CFn of string * string
(* a type has a name, and fields *)
(* fields are names and types *)
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

let module_kw = "module"
let struct_kw = "struct"
let end_kw = "end"
let method_kw = "method"
let sig_sep_kw = ":"
let equal_kw = "="
let class_kw = "class"
let class_type_kw = "class type"
let object_kw = "object"
let external_kw = "external"
let type_kw = "type"

let string_of_ctype ctype = match ctype with
  | CBool -> "Js.boolean"
  | CString -> "string"
  | CFn(x, y) -> x ^ " -> " ^ y
  | CObject (ty, _) -> (String.uncapitalize_ascii ty)

let codegen_properties props =
  let c (name, ctype) = "method " ^ name ^ " : " ^ ctype
    (* match ctype with *)
    (* | CObject (ty, _) -> "method " ^ name ^ " : " ^ ty *)
    (* | CBool -> "method " ^ name ^ " : bool" *)
    (* | CString -> "method " ^ name ^ " : string" *)
  in
  (* don't recurse *)
  String.concat "\n" (List.map c props)

let codegen_type ctype = match ctype with
  | CBool | CString | CFn _ -> "" (* primitive don't need to be codegen-ed *)
  | CObject (name, properties) ->
  let uncap_name = String.uncapitalize_ascii name in
  let params = List.map (fun (param, ctype) -> "?" ^ param ^ ":" ^ (ctype)) (properties) in
  let fn_ty = params @ ["unit"; uncap_name] in
  "class type _" ^ uncap_name
  ^ " = object\n"
  ^ (codegen_properties properties)
  ^ "\nend [@bs]\n"
  ^ "type " ^ uncap_name ^ " = _" ^ uncap_name ^ " Js.t\n"
  ^ "external mk" ^ name ^ " : " ^ (String.concat " -> " fn_ty) ^ " = \"\" [@@bs.obj]\n"

(* Given a list of args and a return type, generated a function signature.
 * All args are assumed to be optional. *)
let gen_fn_sig args ret =
  let collect_args (arg, ctype) = "?" ^ arg ^ ":" ^ ctype
  in
  String.concat " -> " ((List.map collect_args args) @ ["unit"; string_of_ctype ret])

(* Given a name of a function parameter and it's type, generate code if it is an object:
 * 1. class type for the new object
 * 2. Js.t wrapping the class type
 * 3. function to make instance of type
 * *)
let codegen_param mtd name ctype = match ctype with
| CBool | CString | CFn _ -> ""
| CObject (ty, props) ->
  let gen_method (param, ctype) =
    "method " ^ param ^ " : " ^ (ctype)
  in
  let p = mtd ^ (String.capitalize_ascii name) in
  "class type _" ^ p ^ " = object\n"
  ^ (String.concat "\n" (List.map gen_method props))
  ^ "\nend [@bs]\n"
  ^ "type " ^ p ^ " = _" ^ p ^ " Js.t\n"
  ^ "external " ^ "mk" ^ (String.capitalize_ascii p) ^ " : "
  ^ (gen_fn_sig props (CObject (p, [])))
  ^ " = \"\" [@@bs.obj]"

let uncurry fn = function (a, b) -> fn a b

let codegen_method module_name (CMethod (method_name, params)) =
  let collect_params (param, ctype) = match ctype with
    | CBool | CString -> string_of_ctype ctype
    | CFn _ -> "(" ^ string_of_ctype ctype ^ ")"
    | CObject _ -> method_name ^ (String.capitalize_ascii param)
  in
  let method_sig = String.concat " -> " ((List.map collect_params params) @ ["unit"]) in
  let params_type = String.concat "\n" (List.map (uncurry (codegen_param method_name)) params) in
  params_type ^ "\n"
  ^ "external " ^ method_name ^ " : " ^ method_sig ^ " = \"" ^ method_name ^ "\""
  ^ " [@@bs.scope \"chrome\", \"" ^ module_name ^ "\"] [@@bs.val]\n"
  (* for each method arg, if it's a complicated object, we generate a new type *)

let space_between = String.concat " "
let line_between = String.concat "\n"
let with_module_struct module_name body =
  line_between [
    space_between [ module_kw; module_name; equal_kw; struct_kw; ]
  ; body
  ; end_kw
  ]

(* generates code for a module *)
let codegen_module (CModule (name, types, methods, events)) =
  let module_name = String.capitalize_ascii name in
  let type_code = List.map codegen_type types in
  let method_code = List.map (codegen_method name) methods in
  with_module_struct module_name (line_between (type_code @ method_code))

let accountinfo = CObject("AccountInfo", [("id", "string")])
let _ =
  let m = CModule("identity",
                  [accountinfo],
                  [CMethod("getAuthToken",
                           [("details", CObject(
                                "getAuthTokenOptions", [
                                  ("interactive", "Js.boolean");
                                  ("account", "accountInfo");
                                  ("scopes", "string list")
                                ]));
                            ("callback", CFn("string", "'a"
                              ))
                           ])], []) in
  let gen = codegen_module m in
  print_endline gen

(*
chrome.identity
     AccountInfo
       id:string
     getAuthToken
       details:{interactive:boolean?, account:AccountInfo?, scopes:string[]?}
       callback:token:string? -> unit

Uppercase first letter => Type
lowercase first letter => method

CModule(
   "chrome.identity"
 , [CType(
      "AccountInfo",
      [("id", "string")])
   ]
 , [CMethod(
      "getAuthToken",
      [("details", Object([("interactive", "boolean?"); ("account", "AccountInfo?"); ("scopes", "string[]?"])
       ("callback", Fun("string?", "unit"))
      ,])
   ]
*)
