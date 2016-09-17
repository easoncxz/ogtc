module MyTests exposing (..)

import Test exposing (..)
import Expect
import String

import Palindromes as P

all : Test
all =
    describe "A Test Suite"
        [ test "Addition" <|
            \() ->
                Expect.equal (3 + 7) 10
        , test "String.left" <|
            \() ->
                Expect.equal "a" (String.left 1 "abcdefg")
        -- , test "This test should fail" <|
        --     \() ->
        --         Expect.fail "failed as expected!"
        , test "My palindrome SUT" <|
            \() ->
                Expect.equal False (P.isPalindrome "123")
        ]
