module Tests exposing (..)

import Test exposing (..)

import DateTimeTests
import GoogleTasks.EncoderTests as GEnc
import GoogleTasks.DecoderTests as GDec
import OAuth.AuthorizationTests as OAuthAuth

all : Test
all = describe "All tests to be run"
  [ DateTimeTests.all
  , GEnc.all
  , GDec.all
  , OAuthAuth.all
  ]
