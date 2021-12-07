port module Day07 exposing (..)

import Expect
import Html exposing (input)
import Json.Encode as Encode exposing (Value)
import Platform exposing (Program)
import Test exposing (..)
import Utils.List as List


type Input
    = Input (List Int)


calculateFuel : Int -> List Int -> Int
calculateFuel position crabs =
    List.foldl (\crab acc -> abs (position - crab) + acc) 0 crabs


calculateFuel2 : Int -> List Int -> Int
calculateFuel2 position crabs =
    List.foldl (\crab acc -> (+) acc <| List.sum <| List.range 1 (abs (position - crab))) 0 crabs


solve : Input -> Int
solve (Input input) =
    let
        range =
            List.range (List.minimum input |> Maybe.withDefault 0) (List.maximum input |> Maybe.withDefault 1000)
    in
    range
        |> List.foldl (\position acc -> min (calculateFuel position input) acc) 1000000


solve2 : Input -> Int
solve2 (Input input) =
    let
        average =
            List.average input
    in
    List.range (round average - 1) (round average)
        |> List.foldl (\position acc -> min (calculateFuel2 position input) acc) 10000000000


decode : String -> Input
decode input =
    input
        |> String.lines
        |> List.concatMap (String.split ",")
        |> List.filterMap String.toInt
        |> Input


encode : { part1 : Int, part2 : Int } -> Value
encode answer =
    Encode.object
        [ ( "part1", Encode.int answer.part1 )
        , ( "part2", Encode.int answer.part2 )
        ]



-- Tests


testInput : String
testInput =
    "16,1,2,0,4,2,7,1,2,14"


part1 : Test
part1 =
    test "Part 1"
        (\_ ->
            testInput
                |> decode
                |> solve
                |> Expect.equal 37
        )


part2 : Test
part2 =
    test "Part 2"
        (\_ ->
            testInput
                |> decode
                |> solve2
                |> Expect.equal 168
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
