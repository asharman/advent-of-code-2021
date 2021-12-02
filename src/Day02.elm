port module Day02 exposing (..)

import Expect
import Json.Encode as Encode exposing (Value)
import Parser exposing ((|.), (|=), Parser)
import Platform exposing (Program)
import Test exposing (..)


type Command
    = Up Int
    | Down Int
    | Forward Int


type alias Horizontal =
    Int


type alias Vertical =
    Int


type alias Aim =
    Int


type alias Position =
    ( Horizontal, Vertical, Aim )


parseCommand : String -> Result (List Parser.DeadEnd) Command
parseCommand =
    Parser.run commandParser


commandParser : Parser Command
commandParser =
    Parser.oneOf
        [ Parser.map (\_ -> Up) (Parser.keyword "up")
        , Parser.map (\_ -> Down) (Parser.keyword "down")
        , Parser.map (\_ -> Forward) (Parser.keyword "forward")
        ]
        |. Parser.spaces
        |= Parser.int


type Input
    = Input (List Command)


solve : Input -> Int
solve (Input input) =
    List.foldl updatePosition ( 0, 0, 0 ) input
        |> multiplyPosition


updatePosition : Command -> Position -> Position
updatePosition command ( h, v, a ) =
    case command of
        Up amount ->
            ( h, v - amount, a )

        Down amount ->
            ( h, v + amount, a )

        Forward amount ->
            ( h + amount, v, a )


solve2 : Input -> Int
solve2 (Input input) =
    List.foldl updatePositionWithAim ( 0, 0, 0 ) input
        |> multiplyPosition


updatePositionWithAim : Command -> Position -> Position
updatePositionWithAim command ( h, v, a ) =
    case command of
        Up amount ->
            ( h, v, a - amount )

        Down amount ->
            ( h, v, a + amount )

        Forward amount ->
            ( h + amount, v + (amount * a), a )


multiplyPosition : Position -> Int
multiplyPosition ( h, v, _ ) =
    h * v


decode : String -> Input
decode input =
    input
        |> String.split "\n"
        |> List.filterMap (parseCommand >> Result.toMaybe)
        |> Input


encode : { part1 : Int, part2 : Int } -> Value
encode answer =
    Encode.object
        [ ( "part1", Encode.int answer.part1 )
        , ( "part2", Encode.int answer.part2 )
        ]



-- Tests


parsing : Test
parsing =
    test "Parsing"
        (\_ ->
            "forward 5\ndown 5"
                |> decode
                |> Expect.equal (Input [ Forward 5, Down 5 ])
        )


part1 : Test
part1 =
    test "Part 1"
        (\_ ->
            "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2\n"
                |> decode
                |> solve
                |> Expect.equal 150
        )


part2 : Test
part2 =
    test "Part 2"
        (\_ ->
            "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2\n"
                |> decode
                |> solve2
                |> Expect.equal 900
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
