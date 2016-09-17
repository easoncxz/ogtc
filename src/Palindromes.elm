
module Palindromes exposing (isPalindrome)

import String

isPalindrome : String -> Bool
isPalindrome s = String.reverse s == s
