port module Day01 exposing (..)

import Expect
import Json.Encode as Encode exposing (Value)
import Platform exposing (Program)
import Test exposing (..)


type Input
    = Input (List Int)


window : Int -> List a -> List (List a)
window size xs =
    windowHelper [ [] ] size xs


windowHelper : List (List a) -> Int -> List a -> List (List a)
windowHelper acc size xs =
    if List.length (List.take size xs) == size then
        windowHelper (List.take size xs :: acc) size (List.drop 1 xs)

    else
        List.drop 1 <| List.reverse acc


rest : List a -> List a
rest =
    List.tail >> Maybe.withDefault []


solve : Input -> Int
solve (Input input) =
    List.map2 Tuple.pair input (rest input)
        |> List.map
            (\( prev, current ) ->
                if current > prev then
                    1

                else
                    0
            )
        |> List.sum


solve2 : Input -> Int
solve2 (Input input) =
    window 3 input
        |> List.map List.sum
        |> (\sums -> List.map2 Tuple.pair sums (rest sums))
        |> List.map
            (\( prev, current ) ->
                if current > prev then
                    1

                else
                    0
            )
        |> List.sum


decode : String -> Input
decode input =
    input
        |> String.split "\n"
        |> List.filterMap String.toInt
        |> Input


encode : { part1 : Int, part2 : Int } -> Value
encode answer =
    Encode.object
        [ ( "part1", Encode.int answer.part1 )
        , ( "part2", Encode.int answer.part2 )
        ]



-- Tests


part1 : Test
part1 =
    test "Part 1"
        (\_ ->
            Input
                [ 199
                , 200
                , 208
                , 210
                , 200
                , 207
                , 240
                , 269
                , 260
                , 263
                ]
                |> solve
                |> Expect.equal 7
        )


part2 : Test
part2 =
    test "Part 2"
        (\_ ->
            Input
                [ 199
                , 200
                , 208
                , 210
                , 200
                , 207
                , 240
                , 269
                , 260
                , 263
                ]
                |> solve2
                |> Expect.equal 5
        )


windowTest : Test
windowTest =
    test "window"
        (\_ ->
            [ 199
            , 200
            , 208
            , 210
            , 200
            , 207
            , 240
            , 269
            , 260
            , 263
            ]
                |> window 3
                |> Expect.equal
                    [ [ 199, 200, 208 ]
                    , [ 200, 208, 210 ]
                    , [ 208, 210, 200 ]
                    , [ 210, 200, 207 ]
                    , [ 200, 207, 240 ]
                    , [ 207, 240, 269 ]
                    , [ 240, 269, 260 ]
                    , [ 269, 260, 263 ]
                    ]
        )



-- Boilerplate stuff


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    ()


type Msg
    = GotInput String


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotInput inputString ->
            let
                input =
                    decode inputString
            in
            ( model
            , output
                (encode
                    { part1 = solve input
                    , part2 = solve2 input
                    }
                )
            )


port receiveInput : (String -> msg) -> Sub msg


port output : Value -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveInput GotInput
