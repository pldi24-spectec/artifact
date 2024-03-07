open Ast

val string_of_atom : atom -> string
val string_of_unop : unop -> string
val string_of_binop : binop -> string
val string_of_cmpop : cmpop -> string
val string_of_mixop : mixop -> string
val string_of_iter : iter -> string
val string_of_typ : typ -> string
val string_of_exp : exp -> string
val string_of_path : path -> string
val string_of_prem : premise -> string
val string_of_def : def -> string
val string_of_deftyp : deftyp -> string
val string_of_script : script -> string

val structured_string_of_atom : atom -> string
val structured_string_of_unop : unop -> string
val structured_string_of_binop : binop -> string
val structured_string_of_cmpop : cmpop -> string
val structured_string_of_mixop : mixop -> string
val structured_string_of_iter : iter -> string
val structured_string_of_typ : typ -> string
val structured_string_of_exp : exp -> string
val structured_string_of_path : path -> string
val structured_string_of_binds : binds -> string
val structured_string_of_premise : premise -> string
val structured_string_of_def : def -> string
val structured_string_of_deftyp : deftyp -> string
val structured_string_of_script : script -> string
