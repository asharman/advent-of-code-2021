port module Day08 exposing (..)

import Expect
import Html exposing (input)
import Json.Encode as Encode exposing (Value)
import Parser exposing ((|.), (|=), Parser)
import Platform exposing (Program)
import Set exposing (Set)
import Test exposing (..)



-- DIGIT


type alias Digit =
    Set String


digitFromString : String -> Set String
digitFromString string =
    String.split "" string
        |> Set.fromList



-- INPUT & SOLVE


type alias InputRecord =
    { inputValues : List Digit
    , outputValues : List Digit
    }


type Input
    = Input (List InputRecord)


solve : Input -> Int
solve (Input input) =
    let
        uniqueSizes =
            Set.fromList [ 2, 3, 4, 7 ]
    in
    input
        |> List.concatMap .outputValues
        |> List.filter (\set -> Set.member (Set.size set) uniqueSizes)
        |> List.length


solve2 : Input -> Int
solve2 (Input input) =
    input
        |> List.map solveDigits
        |> List.sum


solveDigits : InputRecord -> Int
solveDigits { inputValues, outputValues } =
    let
        findUnique size =
            case List.filter (\set -> Set.size set == size) inputValues of
                [ theSet ] ->
                    theSet

                _ ->
                    Set.empty

        hasSize size set =
            Set.size set == size

        one =
            findUnique 2

        seven =
            findUnique 3

        four =
            findUnique 4

        eight =
            findUnique 7

        three =
            inputValues
                |> List.filter (hasSize 5)
                |> List.filter (\set -> Set.intersect one set == one)
                |> (List.head >> Maybe.withDefault Set.empty)

        nine =
            inputValues
                |> List.filter (hasSize 6)
                |> List.filter (\set -> Set.intersect three set == three)
                |> (List.head >> Maybe.withDefault Set.empty)

        zero =
            inputValues
                |> List.filter (hasSize 6)
                |> List.filter (\set -> set /= nine)
                |> List.filter (\set -> Set.intersect seven set == seven)
                |> (List.head >> Maybe.withDefault Set.empty)

        six =
            inputValues
                |> List.filter (hasSize 6)
                |> List.filter (\set -> set /= nine)
                |> List.filter (\set -> set /= zero)
                |> (List.head >> Maybe.withDefault Set.empty)

        five =
            inputValues
                |> List.filter (hasSize 5)
                |> List.filter (\set -> Set.intersect six set == set)
                |> (List.head >> Maybe.withDefault Set.empty)

        two =
            inputValues
                |> List.filter (hasSize 5)
                |> List.filter (\set -> set /= three)
                |> List.filter (\set -> set /= five)
                |> (List.head >> Maybe.withDefault Set.empty)

        numbers =
            [ zero
            , one
            , two
            , three
            , four
            , five
            , six
            , seven
            , eight
            , nine
            ]
                |> List.indexedMap Tuple.pair

        translate set =
            numbers
                |> List.filter (\( n, nSet ) -> set == nSet)
                |> List.map Tuple.first
                |> List.head
                |> Maybe.withDefault -1
    in
    outputValues
        |> List.map translate
        |> List.map String.fromInt
        |> String.join ""
        |> String.toInt
        |> Maybe.withDefault 0



-- DECODE


decode : String -> Input
decode input =
    input
        |> String.lines
        |> List.map parseLine
        |> Input


parseLine : String -> InputRecord
parseLine input =
    Parser.run lineParser input
        |> Result.toMaybe
        |> Maybe.withDefault { inputValues = [], outputValues = [] }


lineParser : Parser InputRecord
lineParser =
    Parser.succeed (\a b -> ( a, b ))
        |= digitsParser
        |. Parser.spaces
        |. Parser.symbol "| "
        |= digitsParser
        |> Parser.map
            (\( inputValues, outputValues ) ->
                { inputValues = inputValues
                , outputValues = outputValues
                }
            )


digitsParser : Parser (List Digit)
digitsParser =
    Parser.succeed (\a b -> ( a, b ))
        |= Parser.chompUntilEndOr " |"
        |> Parser.getChompedString
        |> Parser.map (String.split " " >> List.map digitFromString)


encode : { part1 : Int, part2 : Int } -> Value
encode answer =
    Encode.object
        [ ( "part1", Encode.int answer.part1 )
        , ( "part2", Encode.int answer.part2 )
        ]



-- Tests


testInput : String
testInput =
    """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
"""


singleLineInput : String
singleLineInput =
    "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"


part1 : Test
part1 =
    test "Part 1"
        (\_ ->
            testInput
                |> decode
                |> solve
                |> Expect.equal 26
        )


part2 : Test
part2 =
    test "Part 2"
        (\_ ->
            testInput
                |> decode
                |> solve2
                |> Expect.equal 61229
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
