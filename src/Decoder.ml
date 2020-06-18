type 'a t = Js.Json.t -> ('a, string) result

(* Functor *)

let map f decoder json = Belt.Result.map (decoder json) f

let ( <$> ) = map

let voidRight : 'a 'b. 'a -> 'b t -> 'a t = fun x -> map (fun _ -> x)

let ( <$ ) = voidRight

let voidLeft decoder x = map (fun _ -> x) decoder

let ( $> ) = voidLeft

let flap decoder x = map (fun f -> f x) decoder

let ( <@> ) = flap

(* Apply *)

let apply decoder1 decoder2 json =
  Belt.Result.flatMap (decoder1 json) @@ Belt.Result.map (decoder2 json)

let ( <*> ) = apply

let applyFirst a b = (fun x _ -> x) <$> a <*> b

let ( <* ) = applyFirst

let applySecond a b = (fun _ x -> x) <$> a <*> b

let ( *> ) = applySecond

(* Applicative *)

let pure value _ = Ok value

(* Monad *)

let flatMap decoder1 decoder2 json =
  Belt.Result.flatMap (decoder1 json) (fun value -> decoder2 value json)

let ( >>= ) = flatMap

let flatMapFlipped decoder2 decoder1 json =
  Belt.Result.flatMap (decoder1 json) (fun value -> decoder2 value json)

let ( =<< ) = flatMapFlipped

let composeKleisli decoder1 decoder2 value = decoder1 value >>= decoder2

let ( >=> ) = composeKleisli

let composeKleisliFlipped decoder1 decoder2 value = decoder1 =<< decoder2 value

let ( <=< ) = composeKleisliFlipped

let join decoder = decoder >>= fun value -> value

(* MonadThrow *)

let throwError error _ = Error error

(* MonadError *)

let catchError decoder f json =
  match decoder json with Error error -> f error json | Ok _ as ok -> ok

(* Extend *)

let extend f decoder = pure @@ f decoder

let ( <<= ) = extend

let extendFlipped decoder f = pure @@ f decoder

let ( =>> ) = extendFlipped

let duplicate decoder = extend (fun x -> x) decoder

(* Alt *)

let alt decoder1 decoder2 json =
  match decoder1 json with Ok _ as ok -> ok | Error _ -> decoder2 json

let ( <|> ) = alt

(* Helpers *)

let fromOption ?(error = "invalid value") = function
  | Some value -> pure value
  | None -> throwError error

let fromResult = function
  | Ok value -> pure value
  | Error error -> throwError error

(* Decode string helpers *)

let decodeString string decoder =
  match Js.Json.parseExn string with
  | exception _ -> Error "Invalid JSON"
  | json -> decoder json

let ( --> ) = decodeString

let decodeStringFlipped decoder string =
  match Js.Json.parseExn string with
  | exception _ -> Error "Invalid JSON"
  | json -> decoder json

let ( <-- ) = decodeStringFlipped
