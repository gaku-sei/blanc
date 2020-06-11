open Jest
open Expect
open! Operators
open Decoder
open SafeDecoders

type ('a, 'b) either = Right of 'a | Left of 'b

let () =
  describe "Decoder" (fun () ->
      describe "Functor" (fun () ->
          describe "<$" (fun () ->
              let make = "done" in

              testAll "successfully parse and void an int" [ "1" ] (fun input ->
                  match input --> (make <$ int) with
                  | Ok "done" -> pass
                  | _ -> fail "expected an int");

              testAll "fails to validate" [ "1.2"; {|"foo"|}; "true" ]
                (fun input ->
                  match input --> (make <$ int) with
                  | Ok _ -> fail "received an int"
                  | Error _ -> pass));

          describe "$>" (fun () ->
              let make = "done" in

              testAll "successfully parse and void an int" [ "1" ] (fun input ->
                  match input --> (int $> make) with
                  | Ok "done" -> pass
                  | _ -> fail "expected an int");

              testAll "fails to validate" [ "1.2"; {|"foo"|}; "true" ]
                (fun input ->
                  match input --> (int $> make) with
                  | Ok _ -> fail "received an int"
                  | Error _ -> pass));

          describe "<@>" (fun () ->
              let make x = x in

              testAll "successfully add an hardcoded value to the decoded value"
                [ "1" ] (fun input ->
                  match input --> (pure make <@> "foo") with
                  | Ok "foo" -> pass
                  | _ -> fail "expected \"foo\"")));

      describe "Apply" (fun () ->
          describe "<*>" (fun () ->
              let make _ = "done" in

              testAll "successfully apply a decoder of function to an int"
                [ "1" ] (fun input ->
                  match input --> (pure make <*> int) with
                  | Ok "done" -> pass
                  | _ -> fail "expected an int");

              testAll "fails to validate" [ "1.2"; {|"foo"|}; "true" ]
                (fun input ->
                  match input --> (pure make <*> int) with
                  | Ok _ -> fail "received an int"
                  | Error _ -> pass));

          describe "<*" (fun () ->
              let make = "done" in

              testAll "successfully parse and void an int" [ "1" ] (fun input ->
                  match input --> (pure make <* int) with
                  | Ok "done" -> pass
                  | _ -> fail "expected an int");

              testAll "fails to validate " [ "1.2"; {|"foo"|}; "true" ]
                (fun input ->
                  match input --> (pure make <* int) with
                  | Ok _ -> fail "received an int"
                  | Error _ -> pass));

          describe "*>" (fun () ->
              let make = "done" in

              testAll "successfully parse and void an int" [ "1" ] (fun input ->
                  match input --> (int *> pure make) with
                  | Ok "done" -> pass
                  | _ -> fail "expected an int");

              testAll "fails to validate" [ "1.2"; {|"foo"|}; "true" ]
                (fun input ->
                  match input --> (int *> pure make) with
                  | Ok _ -> fail "received an int"
                  | Error _ -> pass)));

      describe "Monad" (fun () ->
          describe ">>=" (fun () ->
              let decoder x = pure @@ (x + 1) in

              testAll "successfully parse and void an int"
                [ ("1", 2); ("-10", -9) ] (fun (input, expected) ->
                  expect (input --> (int >>= decoder)) = Ok expected);

              testAll "fails to validate" [ "1.2"; {|"foo"|}; "true" ]
                (fun input ->
                  match input --> (int >>= decoder) with
                  | Ok _ -> fail "received an int"
                  | Error _ -> pass));

          describe "=<<" (fun () ->
              let decoder x = pure @@ (x + 1) in

              testAll "successfully parse and void an int"
                [ ("1", 2); ("-10", -9) ] (fun (input, expected) ->
                  expect (input --> (decoder =<< int)) = Ok expected);

              testAll "fails to validate" [ "1.2"; {|"foo"|}; "true" ]
                (fun input ->
                  match input --> (decoder =<< int) with
                  | Ok _ -> fail "received an int"
                  | Error _ -> pass));

          describe ">=>" (fun () ->
              let decoder1 x = pure @@ (x + 1) in
              let decoder2 x = pure @@ string_of_int x in

              testAll "successfully parse and void an int"
                [ ("1", "2"); ("-10", "-9") ] (fun (input, expected) ->
                  expect (input --> (int >>= (decoder1 >=> decoder2)))
                  = Ok expected);

              testAll "fails to validate" [ "1.2"; {|"foo"|}; "true" ]
                (fun input ->
                  match input --> (int >>= (decoder1 >=> decoder2)) with
                  | Ok _ -> fail "received an int"
                  | Error _ -> pass));

          describe "<=<" (fun () ->
              let decoder1 x = pure @@ (x + 1) in
              let decoder2 x = pure @@ string_of_int x in

              testAll "successfully parse and void an int"
                [ ("1", "2"); ("-10", "-9") ] (fun (input, expected) ->
                  expect (input --> (int >>= (decoder2 <=< decoder1)))
                  = Ok expected);

              testAll "fails to validate" [ "1.2"; {|"foo"|}; "true" ]
                (fun input ->
                  match input --> (int >>= (decoder2 <=< decoder1)) with
                  | Ok _ -> fail "received an int"
                  | Error _ -> pass));

          describe "join" (fun () ->
              let positiveDecoder =
                let open! Pervasives in
                int >>= function
                | x when x >= 0 -> pure x
                | _ -> throwError "Not a positive integer"
              in

              let negativeDecoder =
                let open! Pervasives in
                int >>= function
                | x when x < 0 -> pure x
                | _ -> throwError "Not a negative integer"
              in

              let decoder bool =
                if bool then pure positiveDecoder else pure negativeDecoder
              in

              testAll "successfully parse and void an int"
                [ ("1", true, 1); ("-1", false, -1) ]
                (fun (input, positive, expected) ->
                  expect (input --> (join @@ decoder positive)) = Ok expected);

              testAll "fails to validate" [ ("1", false); ("-1", true) ]
                (fun (input, positive) ->
                  match input --> (join @@ decoder positive) with
                  | Ok _ -> fail "received an int"
                  | Error _ -> pass)));

      describe "MonadError" (fun () ->
          describe "catchError" (fun () ->
              let decoder = throwError "Not a valid whatever!" in

              testAll "successfully catch thrown error"
                [ ("1", Ok "All good now!") ] (fun (input, expected) ->
                  expect
                    ( input
                    --> catchError decoder (fun _ -> pure "All good now!") )
                  = expected);

              testAll "doesn't catch anything when there is nothing to catch"
                [ ("1", Ok 1); ("-5", Ok (-5)) ] (fun (input, expected) ->
                  expect (input --> catchError int (fun _ -> pure 0)) = expected)));

      describe "Extend" (fun () ->
          describe "=>>" (fun () ->
              let decoder input =
                int =>> fun intDecoder ->
                match input --> intDecoder with
                | Error _ -> "Oops"
                | Ok _ -> "Yeah!"
              in

              testAll
                "successfully transform a simple value into a decoder with an \
                 int"
                [
                  ("1", "0", Ok "Yeah!");
                  ("1", "true", Ok "Oops");
                  ("true", "1", Ok "Yeah!");
                ] (fun (input1, input2, expected) ->
                  expect (input1 --> decoder input2) = expected));

          describe "<<=" (fun () ->
              let decoder input =
                (fun intDecoder ->
                  match input --> intDecoder with
                  | Error _ -> "Oops"
                  | Ok _ -> "Yeah!")
                <<= int
              in

              testAll
                "successfully transform a simple value into a decoder with an \
                 int"
                [
                  ("1", "0", Ok "Yeah!");
                  ("1", "true", Ok "Oops");
                  ("true", "1", Ok "Yeah!");
                ] (fun (input1, input2, expected) ->
                  expect (input1 --> decoder input2) = expected));

          describe "duplicate" (fun () ->
              testAll "successfully duplicate and join any decoder"
                [ ("1", int, Ok 1) ] (fun (input, decoder, expected) ->
                  expect (input --> (join @@ duplicate decoder)) = expected)));

      describe "Alt" (fun () ->
          describe "<|>" (fun () ->
              let right = (fun value -> Right value) <$> string in

              let left = (fun value -> Left value) <$> bool in

              testAll
                "successfully parse using one of the provided decoders, string \
                 or bool"
                [ ({|"foo"|}, Ok (Right "foo")); ({|""|}, Ok (Right "")) ]
                (fun (input, expected) ->
                  expect (input --> (right <|> left)) = expected);

              testAll
                "successfully parse using one of the provided decoders, string \
                 or bool"
                [ ("true", Ok (Left true)); ("false", Ok (Left false)) ]
                (fun (input, expected) ->
                  expect (input --> (right <|> left)) = expected);

              testAll "fails to parse non bool/string"
                [
                  "null";
                  "2";
                  "1x";
                  "1.";
                  "[1,2,3]";
                  {|{"foo":"bar","bar":"baz","baz":1}|};
                ] (fun input ->
                  match input --> (right <|> left) with
                  | Ok _ -> fail "got a oneOf"
                  | Error _ -> pass))))
