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
    Fun.id

let ct_attr_default =
  Attribute.declare "deriving.cmd.default" Attribute.Context.core_type
    Ast_pattern.(single_expr_payload __)
    Fun.id

let ct_attr_nonempty =
  Attribute.declare_flag "deriving.cmd.nonempty" Attribute.Context.core_type

let ct_attr_arg =
  Attribute.declare_flag "deriving.cmd.arg" Attribute.Context.core_type

let ct_attr_placeholder =
  Attribute.declare "deriving.cmd.placeholder" Attribute.Context.core_type
    Ast_pattern.(single_expr_payload (estring __))
    Fun.id

let ct_attr_short =
  Attribute.declare "deriving.cmd.short" Attribute.Context.core_type
    Ast_pattern.(single_expr_payload (echar __))
    Fun.id

let ct_attr_long =
  Attribute.declare "deriving.cmd.long" Attribute.Context.core_type
    Ast_pattern.(single_expr_payload (estring __))
    Fun.id

let ct_attr_description =
  Attribute.declare "deriving.cmd.description" Attribute.Context.core_type
    Ast_pattern.(single_expr_payload (estring __))
    Fun.id

let ct_attr_env =
  Attribute.declare "deriving.cmd.env" Attribute.Context.core_type
    Ast_pattern.(single_expr_payload (esequence (estring __)))
    Fun.id

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

let rec take_while f = function
  | [] -> ([], [])
  | y :: xs when f y ->
      let ys, zs = take_while f xs in
      (y :: ys, zs)
  | zs -> ([], zs)

let is_uppercase_ascii c = 'A' <= c && c <= 'Z'
let is_lowercase_ascii c = 'a' <= c && c <= 'z'

let rec unsnoc = function
  | [ x; last ] -> Some ([ x ], last)
  | x :: xs -> Option.map (fun (xs, last) -> (x :: xs, last)) (unsnoc xs)
  | _ -> None

let pascal_case_to_snake s =
  let cs = String.to_seq s |> List.of_seq in
  let rec split_words = function
    | [] -> []
    | cs -> begin
        let uppers, rest = take_while is_uppercase_ascii cs in
        assert (uppers != []);
        match unsnoc uppers with
        | Some (uppers, r) ->
            assert (uppers != []);
            let rest_words = split_words (r :: rest) in
            uppers :: rest_words
        | None ->
            assert (uppers != []);
            let lowers, rest = take_while is_lowercase_ascii rest in
            let rest_words = split_words rest in
            (uppers @ lowers) :: rest_words
      end
  in
  split_words cs
  |> List.map (fun cs ->
         List.to_seq cs |> String.of_seq |> String.lowercase_ascii)
  |> String.concat "_"

let rec parser_of_typ loc = function
  | [%type: bool] ->
      [%expr
        fun (s : string) ->
          match bool_of_string_opt s with
          | Some i -> Ok i
          | None ->
              Error ("could not parse \"" ^ String.escaped s ^ "\" as bool")]
  | [%type: int] ->
      [%expr
        fun (s : string) ->
          match int_of_string_opt s with
          | Some i -> Ok i
          | None -> Error ("could not parse \"" ^ String.escaped s ^ "\" as int")]
  | [%type: int32] | [%type: Int32.t] ->
      [%expr
        fun (s : string) ->
          match Int32.of_string_opt s with
          | Some i -> Ok i
          | None ->
              Error ("could not parse \"" ^ String.escaped s ^ "\" as int32")]
  | [%type: int64] | [%type: Int64.t] ->
      [%expr
        fun (s : string) ->
          match Int64.of_string_opt s with
          | Some i -> Ok i
          | None ->
              Error ("could not parse \"" ^ String.escaped s ^ "\" as int64")]
  | [%type: nativeint] | [%type: Nativeint.t] ->
      [%expr
        fun (s : string) ->
          match Nativeint.of_string_opt s with
          | Some i -> Ok i
          | None ->
              Error ("could not parse \"" ^ String.escaped s ^ "\" as nativeint")]
  | [%type: float] ->
      [%expr
        fun (s : string) ->
          match float_of_string_opt s with
          | Some i -> Ok i
          | None ->
              Error ("could not parse \"" ^ String.escaped s ^ "\" as float")]
  | [%type: char] ->
      [%expr
        fun (s : string) ->
          if String.length s = 1 then Ok s.[0]
          else Error "incorrect length for char"]
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
                Stdlib.Result.bind (try_map f xs) @@ fun ys -> Ok (y :: ys)
            | [] -> Ok []
          in
          try_map [%e parser_of_typ loc typ] (String.split_on_char ',' s)]
  | [%type: [%t? typ] array] ->
      [%expr
        fun (s : string) ->
          Stdlib.Result.map Array.of_list
            ([%e parser_of_typ loc [%type: [%t typ] list]] s)]
  | [%type: [%t? typ] option] ->
      [%expr
        fun (s : string) ->
          Stdlib.Result.map Option.some ([%e parser_of_typ loc typ] s)]
  | [%type: [%t? typ] lazy_t] | [%type: [%t? typ] Lazy.t] ->
      [%expr
        fun (s : string) ->
          Stdlib.Result.map Lazy.from_val ([%e parser_of_typ loc typ] s)]
  | { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, args); _ } as _typ ->
      app
        (Exp.ident
           (mknoloc
              (Ppx_deriving.mangle_lid (`PrefixSuffix ("parse", "value")) lid)))
        (List.map (parser_of_typ loc) args)
  | { ptyp_loc; ptyp_desc = Ptyp_variant (fields, Closed, None); _ } as typ ->
      let string_and_case = function
        | { prf_desc = Rtag ({ txt = name; _ }, _, []); _ } ->
            let snake = pascal_case_to_snake name in
            ( "\"" ^ String.escaped snake ^ "\"",
              {
                pc_lhs = Pat.constant (Const.string snake);
                pc_guard = None;
                pc_rhs = [%expr Ok [%e Exp.variant name None]];
              } )
        | _ ->
            raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s" deriver
              (Ppx_deriving.string_of_core_type typ)
      in
      let strings, cases = List.split (List.map string_and_case fields) in
      let invalid_case =
        {
          pc_lhs = pvar "s";
          pc_guard = None;
          pc_rhs =
            [%expr
              Error
                ("invalid value \"" ^ String.escaped s
                ^ [%e str ("\", expected one of " ^ String.concat ", " strings)]
                )];
        }
      in
      List.append cases [ invalid_case ] |> Exp.function_
  | { ptyp_loc; _ } as typ ->
      raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s" deriver
        (Ppx_deriving.string_of_core_type typ)

type flag = {
  name : string;
  long : string;
  short : char option;
  parser : expression option;
  default : expression option;
  description : string option;
  env : string list;
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
    | None, [%type: [%t? _] option] -> Some [%expr None]
    | default, _ -> default
  in
  let description = Attribute.get ct_attr_description typ in
  let env = Option.value (Attribute.get ct_attr_env typ) ~default:[] in
  { name; long; short; parser; default; description; env }

type arg_details = None | Default of expression | NonemptyList | List

type arg = {
  name : string;
  placeholder : string;
  parser : expression;
  details : arg_details;
  description : string option;
}

let arg_of_label_decl quoter name loc typ =
  let placeholder =
    Option.value
      (Attribute.get ct_attr_placeholder typ)
      ~default:(String.uppercase_ascii name)
  in
  let parser =
    match (Attribute.get ct_attr_parser typ, typ) with
    | Some fn, _ -> Ppx_deriving.quote ~quoter fn
    | None, [%type: [%t? typ] list] -> parser_of_typ loc typ
    | None, _ -> parser_of_typ loc typ
  in
  let details =
    match (Attribute.get ct_attr_default typ, typ) with
    | Some default, _ -> Default default
    | None, [%type: [%t? _] option] -> Default [%expr None]
    | None, [%type: [%t? _] list] ->
        if Attribute.get ct_attr_nonempty typ |> Option.is_some then
          NonemptyList
        else List
    | None, _ -> None
  in
  let description = Attribute.get ct_attr_description typ in
  { name; placeholder; parser; details; description }

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
            List.filter (fun { details; _ } -> details != None) args
            |> List.length > 1
          then
            raise_errorf ~loc
              "%s cannot be derived when multiple positional arguments can \
               appear a variable number of times because it introduces \
               ambiguity"
              deriver
          else ()
        in
        let help_string =
          let flag_info =
            ("-h, --help", Some "Show this message.")
            :: List.map
                 (fun { long; short; description; _ } ->
                   ( String.concat ", "
                       (Option.to_list
                          (Option.map (fun c -> "-" ^ String.make 1 c) short)
                       @ [ "--" ^ long ]),
                     description ))
                 flags
          in
          let max_start_length =
            List.fold_left max min_int
              (List.map (fun (start, _) -> String.length start) flag_info)
          in
          let flag_info_lines =
            List.map
              (fun (start, description) ->
                match description with
                | Some description ->
                    "    " ^ start
                    ^ String.make
                        (max_start_length - String.length start + 2)
                        ' '
                    ^ description
                | None -> "    " ^ start)
              flag_info
          in
          let arg_info =
            List.map
              (fun { placeholder; description; details; _ } ->
                let placeholder =
                  match details with
                  | None -> "<" ^ placeholder ^ ">"
                  | Default _ -> "[<" ^ placeholder ^ ">]"
                  | NonemptyList -> "<" ^ placeholder ^ ">..."
                  | List -> "[<" ^ placeholder ^ ">...]"
                in
                (placeholder, description))
              args
          in
          let max_placeholder_length =
            List.fold_left max min_int
              (List.map
                 (fun (placeholder, _) -> String.length placeholder)
                 arg_info)
          in
          let arg_info_lines =
            List.map
              (fun (placeholder, description) ->
                match description with
                | Some description ->
                    "    " ^ placeholder
                    ^ String.make
                        (max_placeholder_length - String.length placeholder + 2)
                        ' '
                    ^ description
                | None -> "    " ^ placeholder)
              arg_info
          in
          [%expr
            "Usage: " ^ Sys.argv.(0)
            ^ [%e
                str
                  ({|

Options:
|}
                  ^ String.concat "\n" flag_info_lines
                  ^
                  if List.is_empty args then ""
                  else {|

Arguments:
|} ^ String.concat "\n" arg_info_lines)]]
        in
        let arg_prefix = "arg_" in
        let flag_prefix = "flag_" in
        let rhs_case name = function
          | Some parser ->
              [%expr
                match ss with
                | [] ->
                    Error
                      ("expected argument for flag \"" ^ String.escaped s ^ "\"")
                | v :: ss -> begin
                    match [%e parser] v with
                    | Ok x ->
                        [%e evar (flag_prefix ^ name)] := Some x;
                        inner ss
                    | Error e ->
                        Error
                          ("error parsing value for flag \"" ^ String.escaped s
                         ^ "\": " ^ e)
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
        let rhs_help =
          [%expr
            print_endline [%e help_string];
            exit 0]
        in
        let long_help_case =
          {
            pc_lhs = Pat.constant (Const.string "help");
            pc_guard = None;
            pc_rhs = rhs_help;
          }
        in
        let short_help_case =
          {
            pc_lhs = Pat.constant (Const.char 'h');
            pc_guard = None;
            pc_rhs = rhs_help;
          }
        in
        let unrecognized_case =
          {
            pc_lhs = pvar "_";
            pc_guard = None;
            pc_rhs =
              [%expr Error ("unrecognized flag \"" ^ String.escaped s ^ "\"")];
          }
        in
        let handle_long =
          List.map long_case flags
          |> (Fun.flip List.append) [ long_help_case; unrecognized_case ]
          |> Exp.match_ (evar "long")
        in
        let handle_short =
          List.filter_map short_case flags
          |> (Fun.flip List.append) [ short_help_case; unrecognized_case ]
          |> Exp.match_ (evar "short")
        in
        let end_flags =
          List.fold_left
            (fun end_flags { name; long; parser; _ } ->
              if Option.is_some parser then
                [%expr
                  match ![%e evar (flag_prefix ^ name)] with
                  | Some [%p pvar name] -> [%e end_flags]
                  | None ->
                      Error
                        [%e
                          str
                            ("no value provided for flag \""
                            ^ String.escaped ("--" ^ long)
                            ^ "\"")]]
              else end_flags)
            [%expr
              Ok
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
            (fun (following_args, end_flags)
                 { name; placeholder; parser; details; _ } ->
              match details with
              | None ->
                  ( following_args + 1,
                    [%expr
                      match args with
                      | v :: args -> begin
                          match [%e parser] v with
                          | Ok [%p pvar (arg_prefix ^ name)] -> [%e end_flags]
                          | Error e ->
                              Error
                                ([%e
                                   str
                                     ("error parsing value for <" ^ placeholder
                                    ^ ">: ")]
                                ^ e)
                        end
                      | [] ->
                          Error
                            [%e
                              str
                                ("expected positional argument <" ^ placeholder
                               ^ ">")]] )
              | Default default ->
                  ( 0,
                    [%expr
                      match args with
                      | v :: args
                        when List.length args >= [%e int following_args] ->
                        begin
                          match [%e parser] v with
                          | Ok [%p pvar (arg_prefix ^ name)] -> [%e end_flags]
                          | Error e ->
                              Error
                                ([%e
                                   str
                                     ("error parsing value for [<" ^ placeholder
                                    ^ ">]: ")]
                                ^ e)
                        end
                      | _ ->
                          let [%p pvar (arg_prefix ^ name)] = [%e default] in
                          [%e end_flags]] )
              | NonemptyList ->
                  ( 0,
                    [%expr
                      if List.length args >= [%e int following_args] then
                        let rec try_map f = function
                          | x :: xs ->
                              Stdlib.Result.bind (f x) @@ fun y ->
                              Stdlib.Result.bind (try_map f xs) @@ fun ys ->
                              Ok (y :: ys)
                          | [] -> Ok []
                        in
                        let rec split_at n xs =
                          match (n, xs) with
                          | 0, zs -> ([], zs)
                          | n, y :: xs ->
                              let ys, zs = split_at (n - 1) xs in
                              (y :: ys, zs)
                        in
                        let vs, args =
                          split_at
                            (max 1 (List.length args - [%e int following_args]))
                            args
                        in
                        match try_map [%e parser] vs with
                        | Ok [%p pvar (arg_prefix ^ name)] -> [%e end_flags]
                        | Error e ->
                            Error
                              ([%e
                                 str
                                   ("error parsing value for <" ^ placeholder
                                  ^ ">...: ")]
                              ^ e)
                      else
                        Error
                          [%e
                            str
                              ("expected positional argument <" ^ placeholder
                             ^ ">...")]] )
              | List ->
                  ( 0,
                    [%expr
                      let length = List.length args in
                      if length >= [%e int following_args] then
                        let rec try_map f = function
                          | x :: xs ->
                              Stdlib.Result.bind (f x) @@ fun y ->
                              Stdlib.Result.bind (try_map f xs) @@ fun ys ->
                              Ok (y :: ys)
                          | [] -> Ok []
                        in
                        let rec split_at n xs =
                          match (n, xs) with
                          | 0, zs -> ([], zs)
                          | n, y :: xs ->
                              let ys, zs = split_at (n - 1) xs in
                              (y :: ys, zs)
                        in
                        let vs, args =
                          split_at (length - [%e int following_args]) args
                        in
                        match try_map [%e parser] vs with
                        | Ok [%p pvar (arg_prefix ^ name)] -> [%e end_flags]
                        | Error e ->
                            Error
                              ([%e
                                 str
                                   ("error parsing value for [<" ^ placeholder
                                  ^ ">...]: ")]
                              ^ e)
                      else
                        let [%p pvar (arg_prefix ^ name)] = [] in
                        [%e end_flags]] ))
            ( 0,
              [%expr
                match args with
                | [] -> [%e end_flags]
                | s :: _ ->
                    Error
                      ("unexpected positional argument \"" ^ String.escaped s
                     ^ "\"")] )
            (List.rev args)
        in
        let body =
          [%expr
            let rec inner = function
              | s :: ss when s = "--" ->
                  args := !args @ ss;
                  Ok ()
              | s :: ss when String.starts_with ~prefix:"--" s -> begin
                  let s, ss =
                    match String.split_on_char '=' s with
                    | s :: (_ :: _ as ss') -> (s, String.concat "=" ss' :: ss)
                    | _ -> (s, ss)
                  in
                  let long = String.sub s 2 (String.length s - 2) in
                  [%e handle_long]
                end
              | s :: ss when String.starts_with ~prefix:"-" s -> begin
                  let s, ss =
                    match String.split_on_char '=' s with
                    | s :: (_ :: _ as ss') -> (s, String.concat "=" ss' :: ss)
                    | _ -> (s, ss)
                  in
                  let length = String.length s in
                  let short, ss =
                    if length > 2 then
                      (s.[1], ("-" ^ String.sub s 2 (length - 2)) :: ss)
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
            (fun body ({ name; parser; default; env; _ } : flag) ->
              match parser with
              | Some parser -> begin
                  let default =
                    [%expr
                      Ok
                        [%e
                          match default with
                          | Some default -> [%expr ref (Some [%e default])]
                          | None -> [%expr ref None]]]
                  in
                  let env_result =
                    List.fold_right
                      (fun env body ->
                        [%expr
                          match Sys.getenv_opt [%e str env] with
                          | Some v -> begin
                              match [%e parser] v with
                              | Result.Ok x -> Ok (ref (Some x))
                              | Error e ->
                                  Error
                                    ("error parsing value for environment \
                                      variable \"" ^ String.escaped env ^ "\": "
                                   ^ e)
                            end
                          | None -> [%e body]])
                      env default
                  in
                  [%expr
                    match [%e env_result] with
                    | Result.Ok [%p pvar (flag_prefix ^ name)] -> [%e body]
                    | Error e -> Error e]
                end
              | None ->
                  let default = [%expr Ok (ref false)] in
                  let env_result =
                    List.fold_right
                      (fun env body ->
                        [%expr
                          match Sys.getenv_opt [%e str env] with
                          | Some v -> begin
                              match
                                [%e
                                  parser_of_typ !Ast_helper.default_loc
                                    [%type: bool]]
                                  v
                              with
                              | Result.Ok x -> Ok (ref x)
                              | Error e ->
                                  Error
                                    ("error parsing value for environment \
                                      variable \"" ^ String.escaped env ^ "\": "
                                   ^ e)
                            end
                          | None -> [%e body]])
                      env default
                  in
                  [%expr
                    match [%e env_result] with
                    | Result.Ok [%p pvar (flag_prefix ^ name)] -> [%e body]
                    | Error e -> Error e])
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

let deriver = "cmd_value"

let value_core_type_of_decl type_decl =
  let loc = !Ast_helper.default_loc in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  [%type:
    Ppx_deriving_runtime.string ->
    ([%t typ], Ppx_deriving_runtime.string) Ppx_deriving_runtime.Result.t]

let value_sig_of_type type_decl =
  [
    Sig.value
      (Val.mk
         (mknoloc
            (Ppx_deriving.mangle_type_decl
               (`PrefixSuffix ("parse", "value"))
               type_decl))
         (value_core_type_of_decl type_decl));
  ]

let parser_of_label_decl { pld_type; pld_attributes; _ } =
  let loc = !Ast_helper.default_loc in
  let attrs = pld_type.ptyp_attributes @ pld_attributes in
  let typ =
    Ppx_deriving.remove_pervasives ~deriver
      { pld_type with ptyp_attributes = attrs }
  in
  parser_of_typ loc typ

let value_str_of_type ({ ptype_loc = loc; _ } as type_decl) =
  let quoter = Ppx_deriving.create_quoter () in
  let value_parser =
    match (type_decl.ptype_kind, type_decl.ptype_manifest) with
    | Ptype_abstract, Some manifest -> parser_of_typ loc manifest
    | Ptype_variant constrs, _ ->
        let string_and_case = function
          | {
              pcd_name = { txt = name; _ };
              pcd_vars = [];
              pcd_args = Pcstr_tuple [];
              pcd_res = None;
              _;
            } ->
              let snake = pascal_case_to_snake name in
              ( "\"" ^ String.escaped snake ^ "\"",
                {
                  pc_lhs = Pat.constant (Const.string snake);
                  pc_guard = None;
                  pc_rhs =
                    [%expr Ok [%e Exp.construct (mknoloc (Lident name)) None]];
                } )
          | _ ->
              raise_errorf ~loc
                "%s cannot be derived for variants with vars, args, or a res"
                deriver
        in

        let strings, cases = List.split (List.map string_and_case constrs) in
        let invalid_case =
          {
            pc_lhs = pvar "s";
            pc_guard = None;
            pc_rhs =
              [%expr
                Error
                  ("invalid value \"" ^ String.escaped s
                  ^ [%e
                      str ("\", expected one of " ^ String.concat ", " strings)]
                  )];
          }
        in
        List.append cases [ invalid_case ] |> Exp.function_
    | Ptype_record _, _ ->
        raise_errorf ~loc "%s cannot be derived for record types" deriver
    | Ptype_abstract, None ->
        raise_errorf ~loc "%s cannot be derived for fully abstract types"
          deriver
    | Ptype_open, _ ->
        raise_errorf ~loc "%s cannot be derived for open types" deriver
  in
  let eta_expand expr =
    (* Ensure expr is statically constructive by eta-expanding non-funs.
       See https://github.com/ocaml-ppx/ppx_deriving/pull/252. *)
    match expr with
    | { pexp_desc = Pexp_fun _; _ } -> expr
    | _ -> [%expr fun x -> [%e expr] x]
  in
  let value_out_type =
    Ppx_deriving.strong_type_of_type @@ value_core_type_of_decl type_decl
  in
  let parse_value_var =
    pvar
      (Ppx_deriving.mangle_type_decl
         (`PrefixSuffix ("parse", "value"))
         type_decl)
  in
  [
    Vb.mk
      (Pat.constraint_ parse_value_var value_out_type)
      (Ppx_deriving.sanitize ~quoter (eta_expand value_parser));
  ]

let value_impl_generator =
  Deriving.Generator.V2.make_noarg (fun ~ctxt:_ (_, type_decls) ->
      [
        Str.value Nonrecursive
          (List.concat (List.map value_str_of_type type_decls));
      ])

let value_intf_generator =
  Deriving.Generator.V2.make_noarg (fun ~ctxt:_ (_, type_decls) ->
      List.concat (List.map value_sig_of_type type_decls))

let deriving_value : Deriving.t =
  Deriving.add deriver ~str_type_decl:value_impl_generator
    ~sig_type_decl:value_intf_generator
