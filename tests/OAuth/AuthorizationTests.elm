module OAuth.AuthorizationTests exposing (all)

import Test exposing (..)
import Expect

import OAuth.Authorization exposing
    ( parseFragment )

all : Test
all = describe "Zong's parser"
  [ test "read a token when exactly a token is there" <| \() ->
      Expect.equal
        (Just "abc")
        (parseFragment "#access_token=abc")
  , test "read nothing when only one irrelevant field is there" <| \() ->
      Expect.equal
        Nothing
        (parseFragment "#something_else=blah")
  , test "read nothing when only several irrelevant fields are there" <| \() ->
      Expect.equal
        Nothing
        (parseFragment "#too=many&arguments=here")
  , test "read a token among other irrelevant fields" <| \() ->
      Expect.equal
        (Just "not-nothing")
        (parseFragment "#even=if&some-extra-args=are-here&we-are=ok&access_token=not-nothing")
  ]
