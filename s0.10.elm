import Html exposing (text)
import List
import Debug



removeNothings : List (Int, Maybe a) -> List (Int, a)
removeNothings xs =
  case xs of
    [] -> []
    (c, Nothing) :: ys -> removeNothings ys
    (c, Just y) :: ys -> (c, y) :: removeNothings ys

      


runLengths : List (List a) -> List (Int, a)
runLengths xss =
  List.map (\xs -> (List.length xs, List.head xs)) xss |> removeNothings


main =
    text
        (if (test) then
            "Your implementation passed all tests."
        else
            toString (runLengths [[1, 1, 1, 1], [2], [5, 5], [2], [1]])
        )

test : Bool
test =
    List.all ((==) True)
        [ runLengths
            [[1, 1, 1, 1], [2], [5, 5], [2], [1]] ==
                [(4, 1), (1, 2), (2, 5), (1,2), (1,1)]
        , runLengths
            [[2], [5, 5], [2], [1]] ==
                [(1, 2), (2, 5), (1,2), (1,1)]
        , runLengths
            [[1, 1, 1, 1], [2], [5, 5]] ==
                [(4, 1), (1, 2), (2, 5)]
        , runLengths
            [[1, 1, 1, 1]] == [(4, 1)]
        , runLengths
            [["a", "a", "a", "a"], ["b"], ["c", "c"], ["b"], ["a"]] ==
                [(4, "a"), (1, "b"), (2, "c"), (1,"b"), (1,"a")]
        , runLengths [[]] == []
        , runLengths [] == []
        ]
