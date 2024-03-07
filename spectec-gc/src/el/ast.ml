open Util.Source


(* Lists *)

type 'a nl_elem =
  | Nl
  | Elem of 'a

type 'a nl_list = 'a nl_elem list


(* Terminals *)

type nat = int
type text = string
type id = string phrase

type atom =
  | Atom of string               (* atomid *)
  | Infinity                     (* infinity *)
  | Bot                          (* `_|_` *)
  | Dot                          (* `.` *)
  | Dot2                         (* `..` *)
  | Dot3                         (* `...` *)
  | Semicolon                    (* `;` *)
  | Backslash                    (* `\` *)
  | In                           (* `in` *)
  | Arrow                        (* `->` *)
  | Colon                        (* `:` *)
  | Sub                          (* `<:` *)
  | Assign                       (* `:=` *)
  | Approx                       (* `~~` *)
  | SqArrow                      (* `~>` *)
  | SqArrowStar                  (* `~>*` *)
  | Prec                         (* `<<` *)
  | Succ                         (* `>>` *)
  | Turnstile                    (* `|-` *)
  | Tilesturn                    (* `-|` *)
  | Quest                        (* `?` *)
  | Star                         (* `*` *)

type brack =
  | Paren                        (* ``(` ... `)` *)
  | Brack                        (* ``[` ... `]` *)
  | Brace                        (* ``{` ... `}` *)

type dots = Dots | NoDots


(* Iteration *)

type iter =
  | Opt                          (* `?` *)
  | List                         (* `*` *)
  | List1                        (* `+` *)
  | ListN of exp * id option     (* `^` exp *)


(* Types *)

and numtyp =
  | NatT                         (* `nat` *)
  | IntT                         (* `int` *)
  | RatT                         (* `rat` *)
  | RealT                        (* `real` *)

and typ = typ' phrase
and typ' =
  | VarT of id                   (* varid *)
  | BoolT                        (* `bool` *)
  | NumT of numtyp               (* numtyp *)
  | TextT                        (* `text` *)
  | ParenT of typ                (* `(` typ `)` *)
  | TupT of typ list             (* `(` list2(typ, `,`) `)` *)
  | IterT of typ * iter          (* typ iter *)
  (* The forms below are only allowed in type definitions *)
  | StrT of typfield nl_list     (* `{` list(typfield,`,`') `}` *)
  | CaseT of dots * id nl_list * typcase nl_list * dots (* `|` list(`...`|varid|typcase, `|`) *)
  | RangeT of typenum nl_list    (* exp `|` `...` `|` exp *)
  | AtomT of atom                (* atom *)
  | SeqT of typ list             (* `epsilon` / typ typ *)
  | InfixT of typ * atom * typ   (* typ atom typ *)
  | BrackT of brack * typ        (* ``` ([{ typ }]) *)

and typfield = atom * (typ * premise nl_list) * hint list     (* atom typ prem* hint* *)
and typcase = atom * (typ list * premise nl_list) * hint list (* atom typ* prem* hint* *)
and typenum = exp * exp option                  (* exp (`|` exp (`|` `...` `|` exp)?)* *)


(* Expressions *)

and unop =
  | NotOp   (* `~` *)
  | PlusOp  (* `+` *)
  | MinusOp (* `-` *)

and binop =
  | AndOp  (* `/\` *)
  | OrOp   (* `\/` *)
  | ImplOp (* `=>` *)
  | EquivOp (* `<=>` *)
  | AddOp  (* `+` *)
  | SubOp  (* `-` *)
  | MulOp  (* `*` *)
  | DivOp  (* `/` *)
  | ExpOp  (* `^` *)

and cmpop =
  | EqOp (* `=` *)
  | NeOp (* `=/=` *)
  | LtOp (* `<` *)
  | GtOp (* `>` *)
  | LeOp (* `<=` *)
  | GeOp (* `>=` *)

and exp = exp' phrase
and exp' =
  | VarE of id                   (* varid *)
  | AtomE of atom                (* atom *)
  | BoolE of bool                (* bool *)
  | NatE of nat                  (* nat *)
  | HexE of nat                  (* 0xhex *)
  | CharE of nat                 (* 0uhex *)
  | TextE of text                (* text *)
  | UnE of unop * exp            (* unop exp *)
  | BinE of exp * binop * exp    (* exp binop exp *)
  | CmpE of exp * cmpop * exp    (* exp cmpop exp *)
  | EpsE                         (* `epsilon` *)
  | SeqE of exp list             (* exp exp *)
  | IdxE of exp * exp            (* exp `[` exp `]` *)
  | SliceE of exp * exp * exp    (* exp `[` exp `:` exp `]` *)
  | UpdE of exp * path * exp     (* exp `[` path `=` exp `]` *)
  | ExtE of exp * path * exp     (* exp `[` path `=..` exp `]` *)
  | StrE of expfield nl_list     (* `{` list(expfield, `,`) `}` *)
  | DotE of exp * atom           (* exp `.` atom *)
  | CommaE of exp * exp          (* exp `,` exp *)
  | CompE of exp * exp           (* exp `++` exp *)
  | LenE of exp                  (* `|` exp `|` *)
  | SizeE of id                  (* `||` exp `||` *)
  | ParenE of exp * bool         (* `(` exp `)` *)
  | TupE of exp list             (* `(` list2(exp, `,`) `)` *)
  | InfixE of exp * atom * exp   (* exp atom exp *)
  | BrackE of brack * exp        (* ``` ([{ exp }]) *)
  | CallE of id * exp            (* `$` defid exp? *)
  | IterE of exp * iter          (* exp iter *)
  | HoleE of bool                (* `%` or `%%` *)
  | FuseE of exp * exp           (* exp `#` exp *)

and expfield = atom * exp        (* atom exp *)

and path = path' phrase
and path' =
  | RootP                        (*  *)
  | IdxP of path * exp           (* path `[` exp `]` *)
  | SliceP of path * exp * exp   (* path `[` exp `:` exp `]` *)
  | DotP of path * atom          (* path `.` atom *)


(* Grammars *)

and sym = sym' phrase
and sym' =
  | VarG of id * sym list                    (* gramid (`(` sym,* `)`)? *)
  | NatG of int                              (* nat *)
  | HexG of int                              (* 0xhex *)
  | CharG of int                             (* U+hex *)
  | TextG of string                          (* `"`text`"` *)
  | EpsG                                     (* `epsilon` *)
  | SeqG of sym nl_list                      (* sym sym *)
  | AltG of sym nl_list                      (* sym `|` sym *)
  | RangeG of sym * sym                      (* sym `|` `...` `|` sym *)
  | ParenG of sym                            (* `(` sym `)` *)
  | TupG of sym list                         (* `(` sym ',' ... ',' sym `)` *)
  | IterG of sym * iter                      (* sym iter *)
  | ArithG of exp                            (* `$(` exp `)` *)
  | AttrG of sym * exp                       (* exp `:` sym *)

and prod = prod' phrase
and prod' = sym * exp * premise nl_list      (* `|` sym `=>` exp (`--` premise)* *)

and gram = gram' phrase
and gram' = dots * prod nl_list * dots       (* `|` list(`...`|prod, `|`) *)


(* Definitions *)

and param = param' phrase
and param' =
  | VarP of id                                (* varid *)
  | GramP of id * id * iter list              (* `grammar` gramid `:` varid iter* *)

and def = def' phrase
and def' =
  | SynD of id * id * typ * hint list         (* `syntax` synid hint* `=` typ *)
  | GramD of id * id * param list * typ * gram * hint list (* `grammar` gramid hint* `:` type `=` gram *)
  | RelD of id * typ * hint list              (* `relation` relid `:` typ hint* *)
  | RuleD of id * id * exp * premise nl_list  (* `rule` relid ruleid? `:` exp (`--` premise)* *)
  | VarD of id * typ * hint list              (* `var` varid `:` typ *)
  | DecD of id * exp * typ * hint list        (* `def` `$` defid exp? `:` typ hint* *)
  | DefD of id * exp * exp * premise nl_list  (* `def` `$` defid exp? `=` exp (`--` premise)* *)
  | SepD                                      (* separator *)
  | HintD of hintdef

and premise = premise' phrase
and premise' =
  | RulePr of id * exp                       (* ruleid `:` exp *)
  | IfPr of exp                              (* `if` exp *)
  | ElsePr                                   (* `otherwise` *)
  | IterPr of premise * iter                 (* premise iter *)

and hintdef = hintdef' phrase
and hintdef' =
  | AtomH of id * hint list
  | SynH of id * id * hint list
  | GramH of id * id * hint list
  | RelH of id * hint list
  | VarH of id * hint list
  | DecH of id * hint list

and hint = {hintid : id; hintexp : exp}      (* `(` `hint` hintid exp `)` *)


(* Scripts *)

type script = def list
