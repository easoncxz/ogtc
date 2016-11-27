
module UrlParserStudyTests exposing (all)

import Test exposing (..)
import Expect

import UrlParser as U
import UrlParser exposing ((</>), (<?>))
import Navigation exposing (Location)

import ZongGoogleOAuthParsers exposing
    ( parseFragment
    , maybeToPair
    , checkKeyIs
    )

importantSample =
    { href = "http://localhost:8000/src/Main.elm#access_token=abc"
    , host = "localhost:8000"
    , hostname = "localhost"
    , protocol = "http:"
    , origin = "http://localhost:8000"
    , port_ = "8000"
    , pathname = "/src/Main.elm"
    , search = ""
    , hash = "#access_token=abc"
    , username = ""
    , password = ""
    }

-- No fragment
qsOnly =
    --{ href = "http://localhost:8000/src/Main.elm?query-param-name=qavalue&another-param=value-again"
    { href = "http://localhost:8000/src/Main.elm?key=qavalue"
    , host = "localhost:8000"
    , hostname = "localhost"
    , protocol = "http:"
    , origin = "http://localhost:8000"
    , port_ = "8000"
    , pathname = "/src/Main.elm"
    , search = "?key=qavalue"
    , hash = ""
    , username = ""
    , password = ""
    }

pathOnly =
    { href = "http://localhost:8000/src/Main.elm"
    , host = "localhost:8000"
    , hostname = "localhost"
    , protocol = "http:"
    , origin = "http://localhost:8000"
    , port_ = "8000"
    , pathname = "/src/Main.elm"
    , search = ""
    , hash = ""
    , username = ""
    , password = ""
    }

comprehensiveSample =
    --{ href = "http://localhost:8000/src/Main.elm?query-param-name=qavalue&another-param=value-again#access_token=abc&other_hash=33"
    { href = "http://localhost:8000/src/Main.elm?query-param-name=qavalue&another-param=value-again#access_token=abc"
    , host = "localhost:8000"
    , hostname = "localhost"
    , protocol = "http:"
    , origin = "http://localhost:8000"
    , port_ = "8000"
    , pathname = "/src/Main.elm"
    , search = "?query-param-name=qavalue&another-param=value-again"
    --, hash = "#access_token=abc&other_hash=33"
    , hash = "#access_token=abc"
    , username = ""
    , password = ""
    }

all : Test
all = describe "many parsers"
    [ describe "UrlParser"
        [ test "it can test" <| always <|
            Expect.equal (toString 3) "3"
        , test "it can do exact matches on paths" <| always <|
            Expect.equal
                (U.parsePath (U.s "src" </> U.string) pathOnly)
                (Just "Main.elm")
        ]
    , describe "Zong's parser"
        [ describe "utils"
            [ describe "maybeToPair"
                [ test "rejects []" <| always <|
                    Expect.equal Nothing (maybeToPair [])
                , test "rejects [1]" <| always <|
                    Expect.equal Nothing (maybeToPair [1])
                , test "accepts [1, 2]" <| always <|
                    Expect.equal (Just (1, 2)) (maybeToPair [1, 2])
                , test "rejects [1, 2, 3]" <| always <|
                    Expect.equal Nothing (maybeToPair [1, 2, 3])
                ]
            , describe "checkKeyIs"
                [ test "accepts good key" <| always <|
                    Expect.equal
                        (Just (1, 2))
                        (checkKeyIs 1 (1, 2))
                , test "rejects bad key" <| always <|
                    Expect.equal
                        Nothing
                        (checkKeyIs 100 (1, 2))
                ]
            ]
        , test "it just works" <| always <|
            Expect.all
                [ always <| Expect.equal
                    (Just "abc")
                    (parseFragment importantSample.hash)
                , always <| Expect.equal
                    Nothing
                    (parseFragment "#something_else=blah")
                , always <| Expect.equal
                    Nothing
                    (parseFragment "#too=many&arguments=here")
                , always <| Expect.equal
                    Nothing
                    (parseFragment "#even=if&some=are&access_token=not-nothing")
                ]
                ()
        ]
    ]
