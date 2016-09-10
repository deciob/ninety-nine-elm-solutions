module Main exposing (..)

import Html exposing (text)
import List
import Debug


type RleCode a
    = Run Int a
    | Single a
    | Empty


pack : a -> List (List a) -> List (List a)
pack x acc =
    case acc of
        [] ->
            [ [ x ] ]

        y :: ys ->
            case List.head y of
                Nothing ->
                    [ [ x ] ]

                Just z ->
                    if x == z then
                        (x :: y) :: ys
                    else
                        --[z] :: (y::ys)
                        [ x ] :: acc


transform : List a -> RleCode a
transform block =
    let
        _ =
            Debug.log "block" block
    in
        case List.head block of
            Nothing ->
                Empty

            Just something ->
                if List.length block == 1 then
                    Single something
                else
                    Run (List.length block) something


rleEncode : List a -> List (RleCode a)
rleEncode list =
    let
        _ =
            Debug.log "pack" (List.foldr pack [] list)
    in
        List.foldr pack [] list
            |> List.map transform


main =
    text
        (if (test) then
            "Your implementation passed all tests."
         else
            --toString (rleEncode [1, 1, 1, 1, 2, 5, 5, 2, 1])
            "Your implementation failed at least one test."
        )


test : Bool
test =
    List.all ((==) True)
        [ rleEncode [ 1, 1, 1, 1, 2, 5, 5, 2, 1 ]
            == [ Run 4 1, Single 2, Run 2 5, Single 2, Single 1 ]
        , rleEncode [ 2, 1, 1, 1 ] == [ Single 2, Run 3 1 ]
        , rleEncode [ 2, 2, 2, 1, 1, 1 ] == [ Run 3 2, Run 3 1 ]
        , rleEncode [ 1 ] == [ Single 1 ]
        , rleEncode [] == []
        , rleEncode [ "aa", "aa", "aa" ] == [ Run 3 "aa" ]
        , rleEncode [ "aab", "b", "b", "aa" ]
            == [ Single "aab", Run 2 "b", Single "aa" ]
        ]
