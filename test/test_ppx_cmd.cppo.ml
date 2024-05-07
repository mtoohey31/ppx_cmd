open OUnit2

let show_cmd_result show_cmd = function
  | Ok cmd -> "Ok (" ^ show_cmd cmd ^ ")"
  | Error e -> "Error (" ^ e ^ ")"

type basic = { id : int; name : string; [@short 'n'] quiet : bool }
[@@deriving cmd, show]

let test_basic _ =
  let assert_equal = assert_equal ~printer:(show_cmd_result show_basic) in
  assert_equal
    (Ok { id = 7; name = "joe"; quiet = false })
    (try_parse_basic_with [ "--id"; "7"; "--name"; "joe" ]);
  assert_equal
    (Ok { id = -518; name = "bob"; quiet = false })
    (try_parse_basic_with [ "--id"; "-518"; "-n=bob" ]);
  assert_equal (Error {|no value provided for flag "--name"|})
    (try_parse_basic_with [ "--id"; "518" ]);
  assert_equal (Error {|unexpected positional argument ""|})
    (try_parse_basic_with [ "--id"; "7"; "--name"; "joe"; "--quiet=" ])

type types = {
  a : int;
  b : int32;
  c : Int32.t;
  d : int64;
  e : Int64.t;
  f : nativeint;
  g : Nativeint.t;
  h : float;
  i : char;
  j : int ref;
  k : string list;
  l : string array;
  m : char option;
  n : int64 Lazy.t;
  o : bool;
}
[@@deriving cmd, show]

let test_types _ =
  let assert_equal = assert_equal ~printer:(show_cmd_result show_types) in
  assert_equal
    (Ok
       {
         a = 5;
         b = 95l;
         c = 6l;
         d = 31L;
         e = 1351835L;
         f = -168541n;
         g = -88135125n;
         h = 4318.51685;
         i = 'q';
         j = ref 63515;
         k = [ "abc"; "xyz" ];
         l = [| "foo"; "bar" |];
         m = Some 'i';
         n = lazy 81631L;
         o = true;
       })
    (try_parse_types_with
       [
         "--a=5";
         "--b";
         "95";
         "--c";
         "6";
         "--d";
         "31";
         "--e";
         "1351835";
         "--f=-168541";
         "--g";
         "-88135125";
         "--h";
         "4318.51685";
         "--i";
         "q";
         "--j";
         "63515";
         "--k=abc,xyz";
         "--l";
         "foo,bar";
         "--m";
         "i";
         "--n";
         "81631";
         "--o";
       ])

type many_short = {
  foo : bool; [@short 'f']
  bar : string; [@short 'b']
  quux : bool; [@short 'q']
}
[@@deriving cmd, show]

let test_many_short _ =
  let assert_equal = assert_equal ~printer:(show_cmd_result show_many_short) in
  assert_equal
    (Ok { foo = true; bar = "baz"; quux = true })
    (try_parse_many_short_with [ "-fqb=baz" ]);
  assert_equal
    (Ok { foo = false; bar = "baz"; quux = false })
    (try_parse_many_short_with [ "-b"; "baz" ])

type defaults = {
  password : string option;
  port : int; [@default 22]
  key_file : string option; [@arg]
  host : string; [@arg]
}
[@@deriving cmd, show]

let test_defaults _ =
  let assert_equal = assert_equal ~printer:(show_cmd_result show_defaults) in
  assert_equal
    (Ok
       {
         password = Some "foo";
         port = 22;
         key_file = None;
         host = "example.com";
       })
    (try_parse_defaults_with [ "--password=foo"; "example.com" ]);
  assert_equal
    (Ok
       {
         password = None;
         port = 5681;
         key_file = Some "a-key-file";
         host = "example.com";
       })
    (try_parse_defaults_with [ "--port"; "5681"; "a-key-file"; "example.com" ])

type mv = { src : string; [@arg] dst : string [@arg] } [@@deriving cmd, show]

let test_mv _ =
  let assert_equal = assert_equal ~printer:(show_cmd_result show_mv) in
  assert_equal
    (Ok { src = "foo"; dst = "bar" })
    (try_parse_mv_with [ "foo"; "bar" ]);
  assert_equal (Error "expected positional argument <SRC>")
    (try_parse_mv_with []);
  assert_equal (Error "expected positional argument <DST>")
    (try_parse_mv_with [ "foo" ]);
  assert_equal (Error {|unexpected positional argument "baz"|})
    (try_parse_mv_with [ "foo"; "bar"; "baz" ]);
  assert_equal
    (Ok { src = "--foo"; dst = "--bar" })
    (try_parse_mv_with [ "--"; "--foo"; "--bar" ]);
  assert_equal
    (Ok { src = "foo"; dst = "bar" })
    (try_parse_mv_with [ "foo"; "bar"; "--" ])

let suite =
  "Test deriving(cmd)"
  >::: [
         "test_basic" >:: test_basic;
         "test_types" >:: test_types;
         "test_many_short" >:: test_many_short;
         "test_mv" >:: test_mv;
         "test_defaults" >:: test_defaults;
       ]

let _ = run_test_tt_main suite
