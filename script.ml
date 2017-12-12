(** Types that represent the chrome extensions API **)
type ctype =
  CType of string * (string * ctype) list (* a type exported by chrome extensions *)
| CBool
| CString
(* a type has a name, and fields *)
(* fields are names and types *)
type cmethod =
  CMethod of string * (string * ctype) list (* name of method, and args with names *)
type cevent =
  CEvent of string (* name of event *)
type cmodule =
  (* a chrome module, has the implicit chrome. prefix *)
  CModule of string * ctype list * cmethod list * cevent list


(* types of exported chrome extension bindings *)
(* type cexported = *)
(*     ECModule of string (1* a chrome module, has the implicit chrome. prefix *1) *)
(*   | ECType of string (1* a type exported by chrome extensions *1) *)
(*   | ECMethod of string * (string * ctype) list (1* name of method, and args with names *1) *)
(*   | ECEvent of string (1* name of event *1) *)

let chrome_alarms = CModule ("alarms", [], [], [])

let string_of_ctype ctype = match ctype with
  | CBool -> "Js.boolean"
  | CString -> "string"
  | CType (ty, _) -> (String.uncapitalize_ascii ty)
let codegen_properties props =
  let c (name, ctype) =
    match ctype with
    | CType (ty, _) -> "method " ^ name ^ " : " ^ ty
    | CBool -> "method " ^ name ^ " : bool"
    | CString -> "method " ^ name ^ " : string"
  in
  (* don't recurse *)
  String.concat "\n" (List.map c props)
let codegen_type ctype = match ctype with
  | CBool | CString -> "" (* primitive don't need to be codegen-ed *)
  | CType (name, properties) ->
  let uncap_name = String.uncapitalize_ascii name in
  let params = List.map (fun (param, ctype) -> "?" ^ param ^ ":" ^ (string_of_ctype ctype)) (properties) in
  let fn_ty = params @ ["unit"; uncap_name] in
  "class type _" ^ uncap_name
  ^ " = object\n"
  ^ (codegen_properties properties)
  ^ "\nend [@bs]\n"
  ^ "type " ^ uncap_name ^ " = _" ^ uncap_name ^ " Js.t\n"
  ^ "external mk" ^ name ^ " : " ^ (String.concat " -> " fn_ty) ^ " = \"\" [@@bs.obj]\n"

let codegen_method (CMethod (method_name, params)) =
  let codegen_param_method (param, ctype) =
    "method " ^ param ^ " : " ^ (string_of_ctype ctype)
  in
  let codegen_param_type (param, ctype) = match ctype with
    | CBool | CString -> string_of_ctype ctype
    | CType (ty, props) ->
      let p = (String.capitalize_ascii param) in
      "class type _" ^ method_name ^ p ^ " = object\n"
      ^ (String.concat "\n" (List.map codegen_param_method props))
      ^ "\nend [@bs]\n"
      ^ "type " ^ method_name ^ p ^ " = _" ^ method_name ^ p ^ " Js.t\n"
      ^ "external " ^ "mk" ^ (String.capitalize_ascii method_name) ^ " : "
  in
  (* let codegen_param_method (param, ctype) = match ctype with *)
  (*   | CBool -> "method " ^ param ^ " : " ^ (string_of_ctype ctype) *)
  (*   | CType (ty, _) -> "method " ^ param ^ " : " ^ (string_of_ctype ctype) *)
  let collect_params (param, ctype) = match ctype with
    | CBool | CString -> string_of_ctype ctype
    | CType _ -> method_name ^ (String.capitalize_ascii param)
  in
  let method_sig = String.concat " -> " ((List.map collect_params params) @ ["unit"]) in
  let params_type = String.concat "\n" (List.map codegen_param_type params) in
  params_type ^ "\n"
  ^ "external " ^ method_name ^ " : " ^ method_sig ^ " = \"" ^ method_name ^ "\"" ^ " [@@bs.scope \"chrome\"] [@@bs.val]\n"
  (* for each method arg, if it's a complicated object, we generate a new type *)
  (*
  class type _getAuthTokenOptions = object
    method interactive : Js.boolean
    method scopes : string list
    method account : accountInfo
  end [@bs]

  type getAuthTokenOptions = _getAuthTokenOptions Js.t
  external mkAuthOptions : ?interactive:Js.boolean -> ?scopes:string list -> ?account:accountInfo -> unit -> getAuthTokenOptions = "" [@@bs.obj]
  *)

let codegen_event (anevent : cevent) =
  ""
(* generates code for a module *)
let codegen_module (CModule (name, types, methods, events)) =
  "module " ^ (String.capitalize_ascii name) ^ " = struct\n" ^
  (String.concat "\n" (List.map codegen_type types))
  ^
  (String.concat "\n" (List.map codegen_method methods))
  ^ "end"

let carraystring = CType ("string list", [])
let accountinfo = CType("AccountInfo", [("id", CString)])
let _ =
  let m = CModule("identity", [accountinfo],
                  [CMethod("getAuthToken", [("details", CType("getAuthTokenOptions", [
                       ("interactive", CBool);
                       ("account", accountinfo);
                       ("scopes", carraystring)
                     ]))])], []) in
  let gen = codegen_module m in
  print_endline gen

(*
module Identity = struct
  (* chrome.identity.getAuthToken *)

  class type _accountInfo = object
    method id : string
  end [@bs]

  type accountInfo = _accountInfo Js.t
  external mkAccountInfo : ?id:string -> unit -> accountInfo = "" [@@bs.obj]

  class type _getAuthTokenOptions = object
    method interactive : Js.boolean
    method scopes : string list
    method account : accountInfo
  end [@bs]

  type getAuthTokenOptions = _getAuthTokenOptions Js.t
  external mkAuthOptions : ?interactive:Js.boolean -> ?scopes:string list -> ?account:accountInfo -> unit -> getAuthTokenOptions = "" [@@bs.obj]

  external getAuthToken : getAuthTokenOptions -> (string -> 'a) -> unit = "getAuthToken" [@@bs.scope "chrome", "identity"] [@@bs.val]

  (* chrome.identity.getProfileUserInfo *)

  class type _profileUserInfo = object
    method id : string
    method email : string
  end [@bs]

  type profileUserInfo = _profileUserInfo Js.t

  external getProfileUserInfo : (profileUserInfo -> 'a) -> unit = "getProfileUserInfo" [@@bs.scope "chrome", "identity"] [@@bs.val]

  (* chrome.identity.removeCachedAuthToken *)

  class type _rmCachedToken = object
    method token : string
  end [@bs]

  type rmCachedTokenOptions = _rmCachedToken Js.t
  external mkRmCachedTokenOptions : token:string -> unit -> rmCachedTokenOptions = "" [@@bs.obj]

  external removeCachedAuthToken : rmCachedTokenOptions -> (unit -> 'a) -> unit = "removeCachedAuthToken" [@@bs.scope "chrome", "identity"] [@@bs.val]

  (* chrome.identity.launchWebAuthFlow *)

  class type _webAuthFlowOptions = object
    method url : string
    method interactive : Js.boolean
  end [@bs]

  type webAuthFlowOptions = _webAuthFlowOptions Js.t
  external mkWebFlowOptions : url:string -> ?interactive:Js.boolean -> unit -> webAuthFlowOptions = "" [@@bs.obj]

  external launchWebAuthFlow : webAuthFlowOptions -> (string Js.null -> unit -> 'a) -> unit = "launchWebAuthFlow" [@@bs.scope "chrome", "identity"] [@@bs.val]

  (* chrome.identity.getRedirectURL *)

  external getRedirectURL : string Js.null -> string = "getRedirectURL" [@@bs.scope "chrome", "identity"] [@@bs.val]

  (* chrome.identity.onSignInChanged.addListener *)

  module OnSignInChanged = struct
    external addListener : (accountInfo -> Js.boolean -> 'a) -> unit = "addListener" [@@bs.scope "chrome", "identity", "onSignInChanged"] [@@bs.val]
  end
end
*)
