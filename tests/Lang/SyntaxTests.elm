module Lang.SyntaxTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Lang.Syntax as Syntax exposing (Expr(..))
import Test exposing (..)


suite : Test
suite =
    describe "Syntax"
        [ test "parse empty results in error" <| \_ -> Expect.err (Syntax.parse "")
        , test "parse 5" <| \_ -> Expect.equal (Ok (Int 5)) (Syntax.parse "5")
        , test "parse 5.5" <| \_ -> Expect.equal (Ok (Float 5.5)) (Syntax.parse "5.5")
        , test "parse list" <| \_ -> Expect.equal (Ok (List [ Symbol "+", Int 5, Int 6 ])) (Syntax.parse "(+ 5 6)")
        ]
