import Html exposing (text)
import List 



fPack : a -> List (List a) -> List (List a)
fPack x acc =
    case acc of
        [] -> 
            [[x]]

        -- [x]::[]
        y::ys ->
            case List.head y of
                Nothing ->
                    [[x]]
                Just h ->
                    if h == x then
                        (x :: y) :: ys
                    else
                        [x] :: acc
                    
            
        


pack : List a -> List (List a)
pack xs =
    -- foldr : (a -> b -> b) -> b -> List a -> b
    List.foldr fPack [] xs


main =
    text
        (if (test) then
            "Your implementation passed all tests."
         else
            "Your implementation failed at least one test."
        )


test : Bool
test =
    List.all ((==) True)
        [ pack [1, 1, 1, 1, 2, 5, 5, 2, 1] == [[1, 1, 1, 1], [2], [5, 5], [2], [1]]
        , pack [2, 1, 1, 1] == [[2], [1, 1, 1]]
        , pack [2, 2, 2, 1, 1, 1] == [[2, 2, 2], [1, 1, 1]]
        , pack [1] == [[1]]
        , pack [] == []
        , pack [ "aa", "aa", "aa" ] == [ ["aa", "aa", "aa"] ]
        , pack [ "aab", "b", "b", "aa" ] == [ ["aab"], ["b", "b"], ["aa"] ]
        ]
