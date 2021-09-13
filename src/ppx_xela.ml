open Ppxlib

let name ~loc x =
  let (module A) = Ast_builder.make loc in
  A.pvar @@ "all_" ^ x

let variants ~loc xs =
  let (module A) = Ast_builder.make loc in
  let expr cd =
    let name = cd.pcd_name in
    let decl =
      A.constructor_declaration ~name ~args:(Pcstr_tuple []) ~res:None
    in
    A.econstruct decl None
  in
  List.map expr xs

let xela ~loc x =
  let (module A) = Ast_builder.make loc in

  let lis =
    match x.ptype_kind with
    | Ptype_variant xs -> Option.some @@ variants ~loc xs
    | _ -> None
  in
  let lis = Option.get lis in

  let name = name ~loc x.ptype_name.txt in

  [%stri let [%p name] = [%e A.elist lis]]

let derive ~loc = function (_, [ x ]) -> [ xela ~loc x ] | _ -> failwith "foo"

let expander ~ctxt decls =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  derive ~loc decls

let xela_expander = Deriving.Generator.V2.make_noarg expander

let _ = Deriving.add "xela" ~str_type_decl:xela_expander
