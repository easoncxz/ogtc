module Tests exposing (..)

import Test exposing (..)

import UrlParserStudyTests
import JsonCoding
import MarshallersTest
import DateTimeTests

all : Test
all = describe "All tests to be run"
  [ UrlParserStudyTests.all
  , JsonCoding.all
  , MarshallersTest.all
  , DateTimeTests.all
  ]
