(* Type safe decoders. The runtime value and the type will always match and be 1:1 *)

open Decoder

let int json =
  match Js.Json.decodeNumber json with
  | None -> Error "Not a number"
  | Some value
    when not
           ( Js.Float.isFinite value
           && float_of_int (Js.Math.floor value) == value ) ->
      Error "Not an integer"
  | Some int -> Ok (Obj.magic int : int)

let float json =
  match Js.Json.decodeNumber json with
  | None -> Error "Not a float"
  | Some float -> Ok float

let bool json =
  match Js.Json.decodeBoolean json with
  | None -> Error "Not a bool"
  | Some bool -> Ok bool

let string json =
  match Js.Json.decodeString json with
  | None -> Error "Not a string"
  | Some string -> Ok string

let null value json =
  match Js.Json.decodeNull json with
  | None -> Error "Not null"
  | Some _ -> Ok value

let nullable decoder = null None <|> map (fun value -> Some value) decoder

(* FIXME: There is probably a type safe way to achieve the same efficient code *)
let array (type a) (decoder : a t) json =
  match Js.Json.decodeArray json with
  | None -> Error "Not an array"
  | Some xs ->
      Belt.Array.reduce xs (Ok (Obj.magic xs : a array)) @@ fun acc x ->
      Belt.Result.flatMap (decoder x) (fun _ -> acc)

(* FIXME: There is probably a type safe way to achieve the same efficient code *)
let list (type a) (decoder : a t) json =
  match Js.Json.decodeArray json with
  | None -> Error "Not a list"
  | Some xs ->
      let xs = Belt.List.fromArray xs in
      Belt.List.reduce xs (Ok (Obj.magic xs : a list)) @@ fun acc x ->
      Belt.Result.flatMap (decoder x) (fun _ -> acc)

(* FIXME: There is probably a type safe way to achieve the same efficient code *)
let dict (type a) (decoder : a t) json =
  match Js.Json.decodeObject json with
  | None -> Error "Not a dict"
  | Some dict ->
      Belt.Array.reduce (Js.Dict.entries dict)
        (Ok (Obj.magic dict : a Js.Dict.t))
      @@ fun acc (_, x) -> Belt.Result.flatMap (decoder x) (fun _ -> acc)

(* FIXME: There is probably a type safe way to achieve the same efficient code *)
let keyValuePairs (type a) (decoder : a t) json =
  match Js.Json.decodeObject json with
  | None -> Error "Not a dict"
  | Some dict ->
      let entries = Js.Dict.entries dict in
      Belt.Array.reduce entries (Ok (Obj.magic entries : (string * a) array))
      @@ fun acc (_, x) -> Belt.Result.flatMap (decoder x) (fun _ -> acc)

let field key decoder value =
  match Js.Json.decodeObject value with
  | None -> Error "Not an object"
  | Some dict -> (
      match Js.Dict.get dict key with
      | None -> Error ("Couldn't find the key " ^ key)
      | Some value -> decoder value )

let at keys decoder =
  Belt.List.reduceReverse keys decoder @@ fun acc key -> field key acc

let index index decoder json =
  match Js.Json.decodeArray json with
  | None -> Error "Not an array"
  | Some xs -> (
      match Belt.Array.get xs index with
      | None -> Error (string_of_int index ^ ": index out of bound")
      | Some value -> decoder value )

let maybe decoder = (fun value -> Some value) <$> decoder <|> pure None
