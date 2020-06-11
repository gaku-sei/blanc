open Jest
open Expect
open! Operators
open Decoder
open! SafeDecoders

let () =
  describe "SafeDecoders" (fun () ->
      describe "int" (fun () ->
          testAll "successfully parse int"
            [
              ("1.0", Ok 1);
              ("1", Ok 1);
              ("10", Ok 10);
              ("0", Ok 0);
              ("-10000", Ok (-10000));
              ("1e1", Ok 10);
            ] (fun (input, expected) -> expect (input --> int) = expected);

          testAll "fails to parse non int"
            [ "1.1"; {|"foo"|}; "true"; "false"; "1x"; "1." ] (fun input ->
              match input --> int with
              | Ok _ -> fail "got an int"
              | Error _ -> pass));

      describe "float" (fun () ->
          testAll "successfully parse float"
            [
              ("1", Ok 1.0);
              ("1.0", Ok 1.0);
              ("1.1", Ok 1.1);
              ("10.0", Ok 10.0);
              ("0", Ok 0.0);
              ("0.0", Ok 0.0);
              ("-10000.0", Ok (-10000.0));
              ("1e1", Ok 10.0);
            ] (fun (input, expected) -> expect (input --> float) = expected);

          testAll "fails to parse non float"
            [ {|"foo"|}; "true"; "false"; "1x"; "1." ] (fun input ->
              match input --> float with
              | Ok _ -> fail "got a float"
              | Error _ -> pass));

      describe "bool" (fun () ->
          testAll "successfully parse bool"
            [ ("true", Ok true); ("false", Ok false) ] (fun (input, expected) ->
              expect (input --> bool) = expected);

          testAll "fails to parse non bool" [ {|"foo"|}; "null"; "1x"; "1." ]
            (fun input ->
              match input --> bool with
              | Ok _ -> fail "got a bool"
              | Error _ -> pass));

      describe "string" (fun () ->
          testAll "successfully parse string"
            [ ({|"foo"|}, Ok "foo"); ({|""|}, Ok "") ] (fun (input, expected) ->
              expect (input --> string) = expected);

          testAll "fails to parse non string" [ "null"; "1x"; "1." ]
            (fun input ->
              match input --> string with
              | Ok _ -> fail "got a string"
              | Error _ -> pass));

      describe "null" (fun () ->
          testAll "successfully parse null" [ ("null", "foo", Ok "foo") ]
            (fun (input, fallback, expected) ->
              expect (input --> null fallback) = expected);

          testAll "fails to parse non null" [ {|"foo"|}; "false"; "1x"; "1." ]
            (fun input ->
              match input --> null "whatever" with
              | Ok _ -> fail "got a null"
              | Error _ -> pass));

      describe "nullable" (fun () ->
          testAll "successfully parse nullable"
            [ ("null", Ok None); ({|"foo"|}, Ok (Some "foo")) ]
            (fun (input, expected) ->
              expect (input --> nullable string) = expected);

          testAll "fails to parse non nullable" [ "false"; "1x"; "1." ]
            (fun input ->
              match input --> nullable string with
              | Ok _ -> fail "got a nullable"
              | Error _ -> pass));

      describe "array" (fun () ->
          testAll "successfully parse array of int"
            [ ("[1,2,3]", Ok [| 1; 2; 3 |]); ("[]", Ok [||]) ]
            (fun (input, expected) -> expect (input --> array int) = expected);

          testAll "successfully parse array of string"
            [ ({|["foo","bar"]|}, Ok [| "foo"; "bar" |]) ]
            (fun (input, expected) ->
              expect (input --> array string) = expected);

          testAll "fails to parse non array of string"
            [ "false"; "1x"; "1."; "[1,2,3]" ] (fun input ->
              match input --> array string with
              | Ok _ -> fail "got a array"
              | Error _ -> pass));

      describe "list" (fun () ->
          testAll "successfully parse list of int"
            [ ("[1,2,3]", Ok [ 1; 2; 3 ]); ("[]", Ok []) ]
            (fun (input, expected) -> expect (input --> list int) = expected);

          testAll "successfully parse list of string"
            [ ({|["foo","bar"]|}, Ok [ "foo"; "bar" ]) ]
            (fun (input, expected) -> expect (input --> list string) = expected);

          testAll "fails to parse non list of string"
            [ "false"; "1x"; "1."; "[1,2,3]" ] (fun input ->
              match input --> list string with
              | Ok _ -> fail "got a list"
              | Error _ -> pass));

      describe "dict" (fun () ->
          testAll "successfully parse dict of int"
            [
              ( {|{"foo":1,"bar":2,"baz":3}|},
                Ok (Js.Dict.fromList [ ("foo", 1); ("bar", 2); ("baz", 3) ]) );
              ("{}", Ok (Js.Dict.empty ()));
            ]
            (fun (input, expected) -> expect (input --> dict int) = expected);

          testAll "successfully parse dict of string"
            [
              ( {|{"foo":"bar","bar":"baz","baz":"foo"}|},
                Ok
                  (Js.Dict.fromList
                     [ ("foo", "bar"); ("bar", "baz"); ("baz", "foo") ]) );
            ]
            (fun (input, expected) -> expect (input --> dict string) = expected);

          testAll "fails to parse non dict of string"
            [
              "false";
              "1x";
              "1.";
              "[1,2,3]";
              {|{"foo":"bar","bar":"baz","baz":1}|};
            ] (fun input ->
              match input --> dict string with
              | Ok _ -> fail "got a dict"
              | Error _ -> pass));

      describe "keyValuePairs" (fun () ->
          testAll "successfully parse keyValuePairs of int"
            [
              ( {|{"foo":1,"bar":2,"baz":3}|},
                Ok [| ("foo", 1); ("bar", 2); ("baz", 3) |] );
              ("{}", Ok [||]);
            ]
            (fun (input, expected) ->
              expect (input --> keyValuePairs int) = expected);

          testAll "successfully parse keyValuePairs of string"
            [
              ( {|{"foo":"bar","bar":"baz","baz":"foo"}|},
                Ok [| ("foo", "bar"); ("bar", "baz"); ("baz", "foo") |] );
            ]
            (fun (input, expected) ->
              expect (input --> keyValuePairs string) = expected);

          testAll "fails to parse non keyValuePairs of string"
            [
              "false";
              "1x";
              "1.";
              "[1,2,3]";
              {|{"foo":"bar","bar":"baz","baz":1}|};
            ] (fun input ->
              match input --> keyValuePairs string with
              | Ok _ -> fail "got a keyValuePairs"
              | Error _ -> pass));

      describe "field" (fun () ->
          testAll "successfully parse field int"
            [
              ({|{"foo":1,"bar":2,"baz":3}|}, "foo", Ok 1);
              ({|{"foo":1,"bar":2,"baz":3}|}, "bar", Ok 2);
              ({|{"foo":1,"bar":2,"baz":3}|}, "baz", Ok 3);
            ] (fun (input, key, expected) ->
              expect (input --> field key int) = expected);

          testAll "fails parse field int"
            [
              ({|{"foo":1,"bar":2,"baz":3}|}, "foo'");
              ({|{"foo":1,"bar":2,"baz":3}|}, "");
              ("", "foo");
              ("1", "foo");
            ] (fun (input, key) ->
              match input --> field key int with
              | Ok _ -> fail "got a valid value for field"
              | Error _ -> pass));

      describe "at" (fun () ->
          testAll "successfully parse at int"
            [
              ({|{"foo":{"bar":{"baz":1}}}|}, [ "foo"; "bar"; "baz" ], Ok 1);
              ({|{"foo":1,"bar":2,"baz":3}|}, [ "bar" ], Ok 2);
              ({|{"baz":{"foo":3}}|}, [ "baz"; "foo" ], Ok 3);
            ]
            (fun (input, keys, expected) ->
              expect (input --> at keys int) = expected);

          testAll "fails parse at int"
            [
              ({|{"foo":{"bar":{"baz":1}}}|}, [ "foo"; "bar"; "ba" ]);
              ({|{"foo":1,"bar":2,"baz":3}|}, [ "foo'" ]);
              ({|{"foo":1,"bar":2,"baz":3}|}, [ "" ]);
              ("", [ "foo" ]);
            ]
            (fun (input, keys) ->
              match input --> at keys int with
              | Ok _ -> fail "got a valid value for at"
              | Error _ -> pass));

      describe "index" (fun () ->
          testAll "successfully parse index int"
            [ ("[1,2]", 0, Ok 1); ("[1,2]", 1, Ok 2) ]
            (fun (input, i, expected) ->
              expect (input --> index i int) = expected);

          testAll "fails parse index int"
            [
              ("[]", 0);
              ("[]", -1);
              ("[]", 1);
              ("[1]", 2);
              ("[1]", -1);
              ("", 0);
              ("1", 0);
            ] (fun (input, i) ->
              match input --> index i int with
              | Ok _ -> fail "got a valid value for index"
              | Error _ -> pass));

      describe "maybe" (fun () ->
          testAll "successfully parse maybe int"
            [ ("null", Ok None); ("1", Ok (Some 1)) ] (fun (input, expected) ->
              expect (input --> maybe int) = expected);

          testAll "successfully parse maybe field int"
            [ ("{}", "foo", Ok None); ({|{"foo": 1}|}, "foo", Ok (Some 1)) ]
            (fun (input, key, expected) ->
              expect (input --> maybe (field key int)) = expected));

      describe "complex examples" (fun () ->
          let make name hobbies age additionalInformation =
            [%bs.obj { name; hobbies; age; additionalInformation }]
          in

          let nameDecoder =
            let open! Pervasives in
            string >>= function
            | x when String.length x > 0 -> pure x
            | _ -> throwError "Not a valid name"
          in

          let ageDecoder =
            let open! Pervasives in
            int >>= function
            | x when x > 0 && x <= 130 -> pure x
            | _ -> throwError "Not a valid age"
          in

          let decoder =
            make <$ field "id" int <*> field "name" nameDecoder <@> [ "books" ]
            <*> field "age" ageDecoder
            <*> maybe (field "additionalInformation" string)
          in

          testAll "successfully parse complex objects"
            [
              ( {|{"id": 1, "name": "foobar", "age": 35, "additionalInformation": "likes food too"}|},
                Ok (make "foobar" [ "books" ] 35 @@ Some "likes food too") );
              ( {|{"id": 1, "name": "foobar", "age": 35}|},
                Ok (make "foobar" [ "books" ] 35 None) );
            ]
            (fun (input, expected) -> expect (input --> decoder) = expected);

          testAll "fails to parse invalid complex objects"
            [
              {|{"id": 1, "name": "foobar", "age": 0, "additionalInformation": "likes food too"}|};
              {|{"id": 1, "name": "", "age": 35, "additionalInformation": "likes food too"}|};
              {|{"name": "foobar", "age": 35}|};
              "";
            ] (fun input ->
              match input --> int with
              | Ok _ -> fail "Got a valid result"
              | Error _ -> pass);

          testAll "successfully parse complex objects"
            [
              ( {|{"id": 1, "name": "foobar", "age": 35, "additionalInformation": "likes food too"}|},
                Ok (make "foobar" [ "books" ] 35 @@ Some "likes food too") );
              ( {|{"id": 1, "name": "foobar", "age": 35}|},
                Ok (make "foobar" [ "books" ] 35 None) );
            ]
            (fun (input, expected) -> expect (decoder <-- input) = expected);

          testAll "fails to parse invalid complex objects"
            [
              {|{"id": 1, "name": "foobar", "age": 0, "additionalInformation": "likes food too"}|};
              {|{"id": 1, "name": "", "age": 35, "additionalInformation": "likes food too"}|};
              {|{"name": "foobar", "age": 35}|};
              "";
            ] (fun input ->
              match decoder <-- input with
              | Ok _ -> fail "Got a valid result"
              | Error _ -> pass)))
