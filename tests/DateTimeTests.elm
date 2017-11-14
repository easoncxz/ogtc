module DateTimeTests exposing (..)

import Test exposing (..)
import Expect
import Time.DateTime as DT

all : Test
all = describe "elm-community/elm-time"
  [ test "Read out UTC without conversion" <| \() ->
      case DT.fromISO8601 "2017-11-14T10:05:08.440Z" of
        Ok dt ->
          Expect.equal 10 (DT.hour dt)
        Err e ->
          Expect.fail e
  , test "Interpret non-UTC WITH conversion into UTC" <| \() ->
      case DT.fromISO8601 "2017-11-14T23:05:08.440+13:00" of
        Ok dt ->
          -- It's the same point in time as the above case
          Expect.equal 10 (DT.hour dt)
        Err e ->
          Expect.fail e
  , test "Formatting as string results in UTC" <| \() ->
      case DT.fromISO8601 "2017-11-14T23:05:08.440+13:00" of
        Ok dt ->
          Expect.true "string contains the ISO-8691 'Z'"
            (String.contains "Z" (DT.toISO8601 dt))
        Err e ->
          Expect.fail e
  ]
