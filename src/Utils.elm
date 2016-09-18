
module Utils exposing (..)

splitAt : Int -> List a -> (List a, List a)
splitAt n xs = (List.take n xs, List.drop n xs)
