open Ppxlib
open Asttypes
open Parsetree
open Ast_helper
open Ppx_deriving.Ast_convenience

exception TODO

let deriver = "cmd"
let raise_errorf = Ppx_deriving.raise_errorf

let ct_attr_parser =
  Attribute.declare "deriving.cmd.parser" Attribute.Context.core_type
    Ast_pattern.(single_expr_payload __)
    (fun e -> e)

let ct_attr_default =
  Attribute.declare "deriving.cmd.default" Attribute.Context.core_type
    Ast_pattern.(single_expr_payload __)
    (fun e -> e)

let ct_attr_arg =
  Attribute.declare_flag "deriving.cmd.arg" Attribute.Context.core_type

let ct_attr_placeholder =
  Attribute.declare "deriving.cmd.placeholder" Attribute.Context.core_type
    Ast_pattern.(single_expr_payload (estring __))
    (fun e -> e)

let ct_attr_short =
  Attribute.declare "deriving.cmd.short" Attribute.Context.core_type
    Ast_pattern.(single_expr_payload (echar __))
    (fun e -> e)

let ct_attr_long =
  Attribute.declare "deriving.cmd.long" Attribute.Context.core_type
    Ast_pattern.(single_expr_payload (estring __))
    (fun e -> e)

let try_with_core_type_of_decl type_decl =
  let loc = !Ast_helper.default_loc in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  [%type:
    Ppx_deriving_runtime.string Ppx_deriving_runtime.list ->
    ([%t typ], Ppx_deriving_runtime.string) Result.t]

let with_core_type_of_decl type_decl =
  let loc = !Ast_helper.default_loc in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  [%type: Ppx_deriving_runtime.string Ppx_deriving_runtime.list -> [%t typ]]

let core_type_of_decl type_decl =
  let loc = !Ast_helper.default_loc in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  [%type: Ppx_deriving_runtime.unit -> [%t typ]]

let sig_of_type type_decl =
  [
    Sig.value
      (Val.mk
         (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix "parse") type_decl))
         (core_type_of_decl type_decl));
  ]

let rec parser_of_typ loc = function
  | [%type: bool] ->
      [%expr
        fun (s : string) ->
          match Stdlib.bool_of_string_opt s with
          | Stdlib.Option.Some i -> Stdlib.Result.Ok i
          | None ->
              Stdlib.Result.Error
                ("could not parse \"" ^ Stdlib.String.escaped s ^ "\" as bool")]
  | [%type: int] ->
      [%expr
        fun (s : string) ->
          match Stdlib.int_of_string_opt s with
          | Stdlib.Option.Some i -> Stdlib.Result.Ok i
          | None ->
              Stdlib.Result.Error
                ("could not parse \"" ^ Stdlib.String.escaped s ^ "\" as int")]
  | [%type: int32] | [%type: Int32.t] ->
      [%expr
        fun (s : string) ->
          match Stdlib.Int32.of_string_opt s with
          | Stdlib.Option.Some i -> Stdlib.Result.Ok i
          | None ->
              Stdlib.Result.Error
                ("could not parse \"" ^ Stdlib.String.escaped s ^ "\" as int32")]
  | [%type: int64] | [%type: Int64.t] ->
      [%expr
        fun (s : string) ->
          match Stdlib.Int64.of_string_opt s with
          | Stdlib.Option.Some i -> Stdlib.Result.Ok i
          | None ->
              Stdlib.Result.Error
                ("could not parse \"" ^ Stdlib.String.escaped s ^ "\" as int64")]
  | [%type: nativeint] | [%type: Nativeint.t] ->
      [%expr
        fun (s : string) ->
          match Stdlib.Nativeint.of_string_opt s with
          | Stdlib.Option.Some i -> Stdlib.Result.Ok i
          | None ->
              Stdlib.Result.Error
                ("could not parse \"" ^ Stdlib.String.escaped s
               ^ "\" as nativeint")]
  | [%type: float] ->
      [%expr
        fun (s : string) ->
          match Stdlib.float_of_string_opt s with
          | Stdlib.Option.Some i -> Stdlib.Result.Ok i
          | None ->
              Stdlib.Result.Error
                ("could not parse \"" ^ Stdlib.String.escaped s ^ "\" as float")]
  | [%type: char] ->
      [%expr
        fun (s : string) ->
          if Stdlib.String.length s = 1 then Stdlib.Result.Ok s.[0]
          else Stdlib.Result.Error "incorrect length for char"]
  | [%type: string] -> [%expr fun (s : string) -> Result.Ok s]
  | [%type: [%t? typ] ref] ->
      [%expr
        fun (s : string) -> Stdlib.Result.map ref ([%e parser_of_typ loc typ] s)]
  | [%type: [%t? typ] list] ->
      [%expr
        fun (s : string) ->
          let rec try_map f = function
            | x :: xs ->
                Stdlib.Result.bind (f x) @@ fun y ->
                Stdlib.Result.bind (try_map f xs) @@ fun ys ->
                Stdlib.Result.Ok (y :: ys)
            | [] -> Stdlib.Result.Ok []
          in
          try_map [%e parser_of_typ loc typ] (Stdlib.String.split_on_char ',' s)]
  | [%type: [%t? typ] array] ->
      [%expr
        fun (s : string) ->
          Stdlib.Result.map Stdlib.Array.of_list
            ([%e parser_of_typ loc [%type: [%t typ] list]] s)]
  | [%type: [%t? typ] option] ->
      [%expr
        fun (s : string) ->
          Stdlib.Result.map Stdlib.Option.some ([%e parser_of_typ loc typ] s)]
  | [%type: [%t? typ] lazy_t] | [%type: [%t? typ] Lazy.t] ->
      [%expr
        fun (s : string) ->
          Stdlib.Result.map Stdlib.Lazy.from_val ([%e parser_of_typ loc typ] s)]
  | { ptyp_desc = Ptyp_variant (_, _, _); _ } -> raise TODO
  | { ptyp_loc; _ } as typ ->
      raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s" deriver
        (Ppx_deriving.string_of_core_type typ)

type flag = {
  name : string;
  long : string;
  short : char option;
  parser : expression option;
  default : expression option;
}

let flag_of_label_decl quoter name loc typ =
  let long = Option.value (Attribute.get ct_attr_long typ) ~default:name in
  let short = Attribute.get ct_attr_short typ in
  let parser =
    match Attribute.get ct_attr_parser typ with
    | Some fn -> Some (Ppx_deriving.quote ~quoter fn)
    | None -> begin
        match typ with
        | [%type: bool] -> None
        | _ -> Some (parser_of_typ loc typ)
      end
  in
  let default =
    match (Attribute.get ct_attr_default typ, typ) with
    | None, [%type: [%t? _] option] -> Some [%expr Stdlib.Option.None]
    | default, _ -> default
  in
  { name; long; short; parser; default }

type arg = {
  name : string;
  placeholder : string;
  parser : expression;
  default : expression option;
}

let arg_of_label_decl quoter name loc typ =
  let placeholder =
    Option.value
      (Attribute.get ct_attr_placeholder typ)
      ~default:(String.uppercase_ascii name)
  in
  let parser =
    match Attribute.get ct_attr_parser typ with
    | Some fn -> Ppx_deriving.quote ~quoter fn
    | None -> parser_of_typ loc typ
  in
  let default =
    match (Attribute.get ct_attr_default typ, typ) with
    | None, [%type: [%t? _] option] -> Some [%expr Stdlib.Option.None]
    | default, _ -> default
  in
  { name; placeholder; parser; default }

let input_of_label_decl quoter
    { pld_name = { txt = name; _ }; pld_type; pld_attributes; _ } =
  let loc = !Ast_helper.default_loc in
  let attrs = pld_type.ptyp_attributes @ pld_attributes in
  let typ =
    Ppx_deriving.remove_pervasives ~deriver
      { pld_type with ptyp_attributes = attrs }
  in
  let arg = Attribute.get ct_attr_arg typ in
  if Option.is_some arg then
    Either.Right (arg_of_label_decl quoter name loc typ)
  else Either.Left (flag_of_label_decl quoter name loc typ)

let str_of_type ({ ptype_loc = loc; _ } as type_decl) =
  let quoter = Ppx_deriving.create_quoter () in
  let try_parser_with =
    match (type_decl.ptype_kind, type_decl.ptype_manifest) with
    | Ptype_abstract, Some _manifest -> raise TODO
    | Ptype_variant _constrs, _ ->
        raise_errorf ~loc "%s does not yet support variant subcommands" deriver
    | Ptype_record labels, _ ->
        let flags, args =
          List.partition_map (input_of_label_decl quoter) labels
        in
        let () =
          if
            List.filter (fun { default; _ } -> Option.is_some default) args
            |> List.length > 1
          then
            raise_errorf ~loc
              "%s cannot be derived when multiple positional arguments can \
               appear a variable number of times because it introduces \
               ambiguity"
              deriver
          else ()
        in
        let arg_prefix = "arg_" in
        let flag_prefix = "flag_" in
        let rhs_case name = function
          | Some parser ->
              [%expr
                match ss with
                | [] ->
                    Stdlib.Result.Error
                      ("expected argument for flag \"" ^ Stdlib.String.escaped s
                     ^ "\"")
                | v :: ss -> begin
                    match [%e parser] v with
                    | Stdlib.Result.Ok x ->
                        [%e evar (flag_prefix ^ name)] := Stdlib.Option.Some x;
                        inner ss
                    | Error e ->
                        Stdlib.Result.Error
                          ("error parsing value for flag \""
                         ^ Stdlib.String.escaped s ^ "\": " ^ e)
                  end]
          | None ->
              [%expr
                [%e evar (flag_prefix ^ name)] := true;
                inner ss]
        in
        let long_case { name; long; parser; _ } =
          {
            pc_lhs = Pat.constant (Const.string long);
            pc_guard = None;
            pc_rhs = rhs_case name parser;
          }
        in
        let short_case { name; short; parser; _ } =
          Option.map
            (fun short ->
              {
                pc_lhs = Pat.constant (Const.char short);
                pc_guard = None;
                pc_rhs = rhs_case name parser;
              })
            short
        in
        let unrecognized_case =
          {
            pc_lhs = pvar "_";
            pc_guard = None;
            pc_rhs =
              [%expr
                Stdlib.Result.Error
                  ("unrecognized flag \"" ^ Stdlib.String.escaped s ^ "\"")];
          }
        in
        let handle_long =
          List.map long_case flags
          |> (Fun.flip List.append) [ unrecognized_case ]
          |> Exp.match_ (evar "long")
        in
        let handle_short =
          List.filter_map short_case flags
          |> (Fun.flip List.append) [ unrecognized_case ]
          |> Exp.match_ (evar "short")
        in
        let end_flags =
          List.fold_left
            (fun end_flags { name; long; parser; _ } ->
              if Option.is_some parser then
                [%expr
                  match ![%e evar (flag_prefix ^ name)] with
                  | Stdlib.Option.Some [%p pvar name] -> [%e end_flags]
                  | None ->
                      Stdlib.Result.Error
                        [%e
                          str
                            ("no value provided for flag \""
                            ^ Stdlib.String.escaped ("--" ^ long)
                            ^ "\"")]]
              else end_flags)
            [%expr
              Stdlib.Result.Ok
                [%e
                  List.map
                    (fun { name; _ } -> (name, evar (arg_prefix ^ name)))
                    args
                  |> List.append
                       (List.map
                          (fun ({ name; parser; _ } : flag) ->
                            ( name,
                              if Option.is_some parser then evar name
                              else [%expr ![%e evar (flag_prefix ^ name)]] ))
                          flags)
                  |> record]]
            flags
        in
        let _, end_args =
          List.fold_left
            (fun (i, end_flags) { name; placeholder; parser; default } ->
              match default with
              | Some default ->
                  ( i - 1,
                    [%expr
                      match args with
                      | v :: args when List.length args >= [%e int i] -> begin
                          match [%e parser] v with
                          | Stdlib.Result.Ok [%p pvar (arg_prefix ^ name)] ->
                              [%e end_flags]
                          | Error e ->
                              Stdlib.Result.Error
                                ([%e
                                   str
                                     ("error parsing value for [<" ^ placeholder
                                    ^ ">]: ")]
                                ^ e)
                        end
                      | _ ->
                          let [%p pvar (arg_prefix ^ name)] = [%e default] in
                          [%e end_flags]] )
              | None ->
                  ( i - 1,
                    [%expr
                      match args with
                      | v :: args -> begin
                          match [%e parser] v with
                          | Stdlib.Result.Ok [%p pvar (arg_prefix ^ name)] ->
                              [%e end_flags]
                          | Error e ->
                              Stdlib.Result.Error
                                ([%e
                                   str
                                     ("error parsing value for <" ^ placeholder
                                    ^ ">: ")]
                                ^ e)
                        end
                      | [] ->
                          Stdlib.Result.Error
                            [%e
                              str
                                ("expected positional argument <" ^ placeholder
                               ^ ">")]] ))
            ( List.length args,
              [%expr
                match args with
                | [] -> [%e end_flags]
                | s :: _ ->
                    Stdlib.Result.Error
                      ("unexpected positional argument \""
                     ^ Stdlib.String.escaped s ^ "\"")] )
            (List.rev args)
        in
        let body =
          [%expr
            let rec inner = function
              | s :: ss when s = "--" ->
                  args := !args @ ss;
                  Ok ()
              | s :: ss when Stdlib.String.starts_with ~prefix:"--" s -> begin
                  let s, ss =
                    match Stdlib.String.split_on_char '=' s with
                    | s :: (_ :: _ as ss') ->
                        (s, Stdlib.String.concat "=" ss' :: ss)
                    | _ -> (s, ss)
                  in
                  let long =
                    Stdlib.String.sub s 2 (Stdlib.String.length s - 2)
                  in
                  [%e handle_long]
                end
              | s :: ss when Stdlib.String.starts_with ~prefix:"-" s -> begin
                  let s, ss =
                    match Stdlib.String.split_on_char '=' s with
                    | s :: (_ :: _ as ss') ->
                        (s, Stdlib.String.concat "=" ss' :: ss)
                    | _ -> (s, ss)
                  in
                  let length = Stdlib.String.length s in
                  let short, ss =
                    if length > 2 then
                      (s.[1], ("-" ^ Stdlib.String.sub s 2 (length - 2)) :: ss)
                    else (s.[1], ss)
                  in
                  [%e handle_short]
                end
              | s :: ss ->
                  args := !args @ [ s ];
                  inner ss
              | [] -> Ok ()
            in
            Stdlib.Result.bind (inner arguments) @@ fun () ->
            let args = !args in
            [%e end_args]]
        in
        let full_body =
          List.fold_left
            (fun body ({ name; parser; default; _ } : flag) ->
              Exp.let_ Nonrecursive
                [
                  {
                    pvb_pat = pvar (flag_prefix ^ name);
                    pvb_expr =
                      (if Option.is_some parser then
                         match default with
                         | Some default ->
                             [%expr ref (Stdlib.Option.Some [%e default])]
                         | None -> [%expr ref Stdlib.Option.None]
                       else [%expr ref false]);
                    pvb_attributes = [];
                    pvb_loc = !Ast_helper.default_loc;
                  };
                ]
                body)
            [%expr
              let args = ref [] in
              [%e body]]
            flags
        in
        [%expr fun arguments -> [%e full_body]]
    | Ptype_abstract, None ->
        raise_errorf ~loc "%s cannot be derived for fully abstract types"
          deriver
    | Ptype_open, _ ->
        raise_errorf ~loc "%s cannot be derived for open types" deriver
  in
  let parser_with =
    [%expr
      fun arguments ->
        match [%e try_parser_with] arguments with
        | Result.Ok x -> x
        | Error e ->
            Printf.eprintf "error: %s\n" e;
            exit 1]
  in
  let parser =
    [%expr fun () -> [%e parser_with] (Sys.argv |> Array.to_list |> List.tl)]
  in
  let eta_expand expr =
    (* Ensure expr is statically constructive by eta-expanding non-funs.
       See https://github.com/ocaml-ppx/ppx_deriving/pull/252. *)
    match expr with
    | { pexp_desc = Pexp_fun _; _ } -> expr
    | _ -> [%expr fun x -> [%e expr] x]
  in
  let try_with_out_type =
    Ppx_deriving.strong_type_of_type @@ try_with_core_type_of_decl type_decl
  in
  let with_out_type =
    Ppx_deriving.strong_type_of_type @@ with_core_type_of_decl type_decl
  in
  let out_type =
    Ppx_deriving.strong_type_of_type @@ core_type_of_decl type_decl
  in
  let try_parse_with_var =
    pvar
      (Ppx_deriving.mangle_type_decl
         (`PrefixSuffix ("try_parse", "with"))
         type_decl)
  in
  let parse_with_var =
    pvar
      (Ppx_deriving.mangle_type_decl
         (`PrefixSuffix ("parse", "with"))
         type_decl)
  in
  let parse_var =
    pvar (Ppx_deriving.mangle_type_decl (`Prefix "parse") type_decl)
  in
  [
    Vb.mk
      (Pat.constraint_ try_parse_with_var try_with_out_type)
      (Ppx_deriving.sanitize ~quoter (eta_expand try_parser_with));
    Vb.mk
      (Pat.constraint_ parse_with_var with_out_type)
      (Ppx_deriving.sanitize ~quoter (eta_expand parser_with));
    Vb.mk
      (Pat.constraint_ parse_var out_type)
      (Ppx_deriving.sanitize ~quoter (eta_expand parser));
  ]

let impl_generator =
  Deriving.Generator.V2.make_noarg (fun ~ctxt:_ (_, type_decls) ->
      [ Str.value Nonrecursive (List.concat (List.map str_of_type type_decls)) ])

let intf_generator =
  Deriving.Generator.V2.make_noarg (fun ~ctxt:_ (_, type_decls) ->
      List.concat (List.map sig_of_type type_decls))

let deriving : Deriving.t =
  Deriving.add deriver ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
