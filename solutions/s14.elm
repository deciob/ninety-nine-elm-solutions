import Html exposing (text)
import List



t : a -> List a -> List a
t el acc =
  el :: acc |> (::) el


duplicate : List a -> List a
duplicate list =
    List.foldr t [] list


main =
    text
        (if (test) then
            "Your implementation passed all tests."
         else
            --toString (duplicate [1, 2, 3, 5, 8, 8])
            "Your implementation failed at least one test."
        )


test : Bool
test =
    List.all (\(result, expect) -> result == expect)
        [ ( duplicate [1, 2, 3, 5, 8, 8], [1, 1, 2, 2, 3, 3, 5, 5, 8, 8, 8, 8])
        , ( duplicate [], [])
        , ( duplicate [1], [1, 1])
        ]
        && List.all (\(result, expect) -> result == expect)
            [ ( duplicate ["1", "2", "5"], 
                ["1", "1", "2", "2", "5", "5"] )
            ]
