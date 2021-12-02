
import Expect
import Json.Encode as Encode exposing (Value)
import Platform exposing (Program)
import Test exposing (..)


type Input
    = Input (List String)


solve : Input -> Int
solve (Input input) =
    0

solve2 : Input -> Int
solve2 (Input input) =
    0


decode : String -> Input
decode input =
    input
        |> String.split "\n"
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
            Expect.true "" True
        )


part2 : Test
part2 =
    test "Part 2"
        (\_ ->
            Expect.true "" True
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
            ( model, output 
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
