open! Core

module Name = struct
  type t = string

  let decoder =
    string >>= function
    | x when String.length x > 0 -> pure x
    | _ -> throwError "Not a valid name"
end

module Age = struct
  type t = int

  let decoder =
    int >>= function
    | x when x > 0 && x <= 130 -> pure x
    | _ -> throwError "Not a valid age"
end

module Hobby = struct
  type t = { name : Name.t } [@@bs.deriving accessors]

  let make name = { name }

  let decoder = make <$> field "name" string
end

module User = struct
  type t = {
    age : Age.t;
    name : Name.t;
    hobbies : Hobby.t list;
    additionalInformation : string option;
  }

  let make name hobbies age additionalInformation =
    { age; name; hobbies; additionalInformation }

  (* A user also contains an id, which we need to validate,
     but we don't want it to be in the resulting record *)
  let decoder =
    make
    <$ field "id" int
    <*> field "name" Name.decoder
    <@> [ Hobby.make "books" ]
    <*> field "age" Age.decoder
    <*> maybe (field "additionalInformation" string)
    [@@ocamlformat "break-infix=fit-or-vertical"]
end

let () =
  match "undefined" --> User.decoder with
  | Error error -> Js.log error
  | Ok _ -> ()

let () =
  match {|{"id": 1, "age": 33, "name": "Foobar"}|} --> User.decoder with
  | Error _ -> ()
  | Ok { age; name; hobbies; additionalInformation } -> (
      Js.log @@ name ^ " is " ^ string_of_int age ^ " years old, and likes "
      ^ ( Js.Array.joinWith ", " @@ Belt.List.toArray
        @@ Belt.List.map hobbies Hobby.name )
      ^
      match additionalInformation with
      | None -> ""
      | Some additionalInformation -> ", oh!, and " ^ additionalInformation )

let () =
  match
    {|{"id": 1, "age": 33, "name": "Foobar", "additionalInformation": \
     "they love games as well"}|}
    --> User.decoder
  with
  | Error _ -> ()
  | Ok { age; name; hobbies; additionalInformation } -> (
      Js.log @@ name ^ " is " ^ string_of_int age ^ " years old, and likes "
      ^ ( Js.Array.joinWith ", " @@ Belt.List.toArray
        @@ Belt.List.map hobbies Hobby.name )
      ^
      match additionalInformation with
      | None -> ""
      | Some additionalInformation -> ", oh, and " ^ additionalInformation )

(* Using the monadic features to find positive integers *)

let positiveDecoder =
  int >>= fun x -> if x >= 0 then pure "Positive" else throwError "Not positive"

let () =
  match "32" --> positiveDecoder with Error _ -> () | Ok value -> Js.log value

let () =
  match "-32" --> positiveDecoder with
  | Error error -> Js.log error
  | Ok _ -> ()

(* We can use Kleisli composition *)

let uselessDecoder =
  int
  >>= ( (fun x -> if x >= 0 then pure "All Good" else pure "Not good")
      >=> fun s -> if s = "All Good" then pure 1 else pure 0 )

let () =
  match "12" --> uselessDecoder with Error _ -> () | Ok value -> Js.log value

(* Extend can be useful to bring "normal" values into the decoder monad context *)

let ignoreDecoder = int =>> fun _ -> "foo"

let () =
  match "\"fooo\"" --> ignoreDecoder with
  | Error _ -> ()
  | Ok value -> Js.log value

(* A decoder is a simple function, so it can be applied with json value *)

let () =
  match float @@ Js.Json.number 32. with
  | Error _ -> ()
  | Ok value -> Js.log value
