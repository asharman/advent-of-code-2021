port module Day03 exposing (..)

import Expect
import Html exposing (input)
import Json.Encode as Encode exposing (Value)
import Platform exposing (Program)
import Test exposing (..)
import Utils.List as List


type alias Binary =
    List Bool


type Input
    = Input (List Binary)


invertBinary : Binary -> Binary
invertBinary =
    List.map not


bitToInt : Bool -> Int
bitToInt bit =
    if bit then
        1

    else
        0


toBase10 : Binary -> Int
toBase10 binary =
    binary
        |> List.reverse
        |> List.indexedMap Tuple.pair
        |> List.foldl (\( position, bit ) acc -> acc + (2 ^ position * bitToInt bit)) 0


filterByPosition : Int -> Bool -> List Binary -> List Binary
filterByPosition position bit binary =
    binary
        |> List.map (List.indexedMap Tuple.pair)
        |> List.filter (List.member ( position, bit ))
        |> List.map (List.map Tuple.second)


solve : Input -> Int
solve (Input input) =
    let
        invertAndMultiply binary =
            toBase10 binary * (invertBinary >> toBase10) binary
    in
    input
        |> significantBits (>)
        |> invertAndMultiply


significantBits : (Int -> Int -> Bool) -> List Binary -> Binary
significantBits f binary =
    binary
        |> List.transpose
        |> List.map (List.partition identity)
        |> List.map
            (\( trues, falses ) ->
                if f (List.length trues) (List.length falses) then
                    True

                else
                    False
            )


solve2 : Input -> Int
solve2 (Input input) =
    let
        findRating f position binary =
            let
                bitToCheck =
                    binary
                        |> significantBits f
                        |> (List.drop position >> List.head >> Maybe.withDefault True)
            in
            if List.length binary == 1 then
                (List.head >> Maybe.withDefault []) binary

            else
                binary
                    |> filterByPosition position bitToCheck
                    |> findRating f (position + 1)

        oxygenRating =
            findRating (>=) 0

        co2Rating =
            findRating (<) 0
    in
    (oxygenRating >> toBase10) input * (co2Rating >> toBase10) input


decode : String -> Input
decode input =
    input
        |> String.split "\n"
        |> List.map parseBinary
        |> List.filter (not << List.isEmpty)
        |> Input


parseBinary : String -> Binary
parseBinary input =
    input
        |> String.toList
        |> List.filterMap parseBoolean


parseBoolean : Char -> Maybe Bool
parseBoolean bit =
    case bit of
        '0' ->
            Just False

        '1' ->
            Just True

        _ ->
            Nothing


encode : { part1 : Int, part2 : Int } -> Value
encode answer =
    Encode.object
        [ ( "part1", Encode.int answer.part1 )
        , ( "part2", Encode.int answer.part2 )
        ]



-- Tests


parse : Test
parse =
    describe "Parsing"
        [ test
            "Parse input"
            (\_ ->
                "00100"
                    |> decode
                    |> Expect.equal (Input [ [ False, False, True, False, False ] ])
            )
        , test
            "Parse empty string"
            (\_ ->
                ""
                    |> decode
                    |> Expect.equal (Input [])
            )
        ]


significantBitsTest : Test
significantBitsTest =
    test "Find the bits that come up most often in each position in a list of binary"
        (\_ ->
            [ [ False, False, True, False, False ], [ True, False, True, True, False ], [ True, False, True, False, False ] ]
                |> significantBits (>)
                |> Expect.equal [ True, False, True, False, False ]
        )


convertBinaryToDecimal : Test
convertBinaryToDecimal =
    test "Convert Binary to Decimal"
        (\_ ->
            -- 10110
            [ True, False, True, True, False ]
                |> toBase10
                |> Expect.equal 22
        )


part1 : Test
part1 =
    test "Part 1"
        (\_ ->
            "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"
                |> decode
                |> solve
                |> Expect.equal 198
        )


filterBinaryByPosition : Test
filterBinaryByPosition =
    test "Filters List of Binary by position"
        (\_ ->
            [ [ False, False, True, False, False ], [ True, False, True, True, False ] ]
                |> filterByPosition 0 True
                |> Expect.equal [ [ True, False, True, True, False ] ]
        )


part2 : Test
part2 =
    test "Part 2"
        (\_ ->
            "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"
                |> decode
                |> solve2
                |> Expect.equal 230
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
