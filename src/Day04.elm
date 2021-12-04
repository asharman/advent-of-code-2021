port module Day04 exposing (..)

import Debug
import Expect
import Html exposing (input)
import Json.Encode as Encode exposing (Value)
import Maybe exposing (withDefault)
import Parser exposing ((|.), (|=), Parser)
import Platform exposing (Program)
import Test exposing (..)
import Utils.List as List


type alias Moves =
    List Int


type alias Board =
    List (List Int)


type Input
    = Input ( Moves, List Board )


checkBoard : Moves -> Board -> Bool
checkBoard moves board =
    List.any (checkRow moves) board || List.any (checkRow moves) (List.transpose board)


checkRow : Moves -> List Int -> Bool
checkRow moves row =
    row
        |> List.map (\i -> List.member i moves)
        |> List.all identity


findFirstWinningBoard : Moves -> List Board -> ( Moves, Board )
findFirstWinningBoard moves boards =
    let
        iter movesToConsider futureMoves =
            case List.filter (checkBoard movesToConsider) boards of
                [] ->
                    iter (List.take 1 futureMoves ++ movesToConsider) (List.drop 1 futureMoves)

                board :: _ ->
                    ( movesToConsider, board )
    in
    iter (List.take 1 moves) (List.drop 1 moves)


findLastWinningBoard : Moves -> List Board -> ( Moves, Board )
findLastWinningBoard moves boards =
    let
        iter movesToConsider futureMoves boardsToKeepChecking =
            case List.filter (checkBoard movesToConsider) boardsToKeepChecking of
                [] ->
                    iter (List.take 1 futureMoves ++ movesToConsider) (List.drop 1 futureMoves) boardsToKeepChecking

                board :: _ ->
                    if List.length boardsToKeepChecking == 1 then
                        ( movesToConsider, board )

                    else
                        iter (List.take 1 futureMoves ++ movesToConsider) (List.drop 1 futureMoves) (List.filter (checkBoard movesToConsider >> not) boardsToKeepChecking)
    in
    iter (List.take 1 moves) (List.drop 1 moves) boards


solve : Input -> Int
solve (Input ( moves, boards )) =
    findFirstWinningBoard moves boards
        |> (\( winningMoves, winningBoard ) ->
                winningBoard
                    |> List.concatMap identity
                    |> List.partition (\a -> List.member a winningMoves)
                    |> Tuple.second
                    |> List.sum
                    |> (List.head >> Maybe.withDefault 1 >> (*)) winningMoves
           )


solve2 : Input -> Int
solve2 (Input ( moves, boards )) =
    findLastWinningBoard moves boards
        |> (\( winningMoves, winningBoard ) ->
                winningBoard
                    |> List.concatMap identity
                    |> List.partition (\a -> List.member a winningMoves)
                    |> Tuple.second
                    |> List.sum
                    |> (List.head >> Maybe.withDefault 1 >> (*)) winningMoves
           )


decode : String -> Input
decode input =
    input
        |> Parser.run inputParser
        |> Result.toMaybe
        |> Maybe.withDefault (Input ( [], [] ))


inputParser : Parser Input
inputParser =
    Parser.succeed
        Tuple.pair
        |. Parser.spaces
        |= moveParser
        |. Parser.spaces
        |= boardsParser
        |> Parser.map Input


moveParser : Parser Moves
moveParser =
    Parser.sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = Parser.spaces
        , item = Parser.int
        , trailing = Parser.Forbidden
        }


boardsParser : Parser (List Board)
boardsParser =
    Parser.loop []
        (\boards ->
            Parser.oneOf
                [ Parser.succeed (Parser.Done (List.reverse boards))
                    |. Parser.end
                , Parser.succeed (\board -> Parser.Loop (board :: boards))
                    |. Parser.spaces
                    |= boardParser
                ]
        )


boardParser : Parser Board
boardParser =
    let
        rowParser =
            Parser.loop []
                (\row ->
                    Parser.succeed (\int step -> step int)
                        |. Parser.chompWhile (\c -> c == ' ')
                        |= Parser.int
                        |= Parser.oneOf
                            [ Parser.succeed (\int -> Parser.Done (List.reverse (int :: row)))
                                |. Parser.symbol "\n"
                            , Parser.succeed (\int -> Parser.Loop (int :: row))
                                |. Parser.chompWhile (\c -> c == ' ')
                            ]
                )
    in
    Parser.loop []
        (\rows ->
            Parser.oneOf
                [ Parser.succeed (\row -> Parser.Loop (row :: rows))
                    |= rowParser
                , Parser.succeed (Parser.Done (List.reverse rows))
                ]
        )


encode : { part1 : Int, part2 : Int } -> Value
encode answer =
    Encode.object
        [ ( "part1", Encode.int answer.part1 )
        , ( "part2", Encode.int answer.part2 )
        ]



-- Tests


testInput : String
testInput =
    """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
"""


parseMoves : Test
parseMoves =
    test "Parse a list of moves"
        (\_ ->
            "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n"
                |> Parser.run moveParser
                |> Expect.equal (Ok [ 7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1 ])
        )


parseBoards : Test
parseBoards =
    test "Parse a list of boards"
        (\_ ->
            """22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
"""
                |> Parser.run boardsParser
                |> Expect.equal
                    (Ok
                        [ [ [ 22, 13, 17, 11, 0 ]
                          , [ 8, 2, 23, 4, 24 ]
                          , [ 21, 9, 14, 16, 7 ]
                          , [ 6, 10, 3, 18, 5 ]
                          , [ 1, 12, 20, 15, 19 ]
                          ]
                        , [ [ 3, 15, 0, 2, 22 ]
                          , [ 9, 18, 13, 17, 5 ]
                          , [ 19, 8, 7, 25, 23 ]
                          , [ 20, 11, 10, 24, 4 ]
                          , [ 14, 21, 16, 12, 6 ]
                          ]
                        , [ [ 14, 21, 17, 24, 4 ]
                          , [ 10, 16, 15, 9, 19 ]
                          , [ 18, 8, 23, 26, 20 ]
                          , [ 22, 11, 13, 6, 5 ]
                          , [ 2, 0, 12, 3, 7 ]
                          ]
                        ]
                    )
        )


parseBoard : Test
parseBoard =
    test "Parse a single board"
        (\_ ->
            """14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
"""
                |> Parser.run boardParser
                |> Expect.equal
                    (Ok
                        [ [ 14, 21, 17, 24, 4 ]
                        , [ 10, 16, 15, 9, 19 ]
                        , [ 18, 8, 23, 26, 20 ]
                        , [ 22, 11, 13, 6, 5 ]
                        , [ 2, 0, 12, 3, 7 ]
                        ]
                    )
        )


parse : Test
parse =
    test "Parsing"
        (\_ ->
            testInput
                |> decode
                |> Expect.equal
                    (Input
                        ( [ 7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1 ]
                        , [ [ [ 22, 13, 17, 11, 0 ]
                            , [ 8, 2, 23, 4, 24 ]
                            , [ 21, 9, 14, 16, 7 ]
                            , [ 6, 10, 3, 18, 5 ]
                            , [ 1, 12, 20, 15, 19 ]
                            ]
                          , [ [ 3, 15, 0, 2, 22 ]
                            , [ 9, 18, 13, 17, 5 ]
                            , [ 19, 8, 7, 25, 23 ]
                            , [ 20, 11, 10, 24, 4 ]
                            , [ 14, 21, 16, 12, 6 ]
                            ]
                          , [ [ 14, 21, 17, 24, 4 ]
                            , [ 10, 16, 15, 9, 19 ]
                            , [ 18, 8, 23, 26, 20 ]
                            , [ 22, 11, 13, 6, 5 ]
                            , [ 2, 0, 12, 3, 7 ]
                            ]
                          ]
                        )
                    )
        )


checkRowTest : Test
checkRowTest =
    describe "check if a row is a winner"
        [ test "Returns true if the row has all moves that have been made" <|
            \_ ->
                checkRow [ 1, 2, 3, 4, 5, 6, 7, 8 ] [ 2, 5, 3, 1, 8 ]
                    |> Expect.true "Expect the row to be a winner"
        , test "Returns false if the row has all moves that have been made" <|
            \_ ->
                checkRow [ 1, 2, 3, 4, 5, 6, 7, 8 ] [ 2, 9, 3, 1, 8 ]
                    |> Expect.false "Expect the row to not be a winner"
        ]


checkBoardTest : Test
checkBoardTest =
    let
        board =
            [ [ 3, 15, 0, 2, 22 ]
            , [ 9, 18, 13, 17, 5 ]
            , [ 19, 8, 7, 25, 23 ]
            , [ 20, 11, 10, 24, 4 ]
            , [ 14, 21, 16, 12, 6 ]
            ]
    in
    describe "check if a board is a winner"
        [ test "Returns true if the board is a winner" <|
            \_ ->
                checkBoard [ 2, 15, 7, 11, 10, 21, 8, 18 ] board
                    |> Expect.true "Board is a winner"
        , test "Returns false if the board is not a winner" <|
            \_ ->
                checkBoard [ 2, 15, 7, 11, 10, 20, 8, 18 ] board
                    |> Expect.false "Board is not a winner"
        ]


firstWinningBoardTest : Test
firstWinningBoardTest =
    test "Finds the first winning board by playing out the moves in order" <|
        \_ ->
            testInput
                |> decode
                |> (\(Input ( moves, boards )) -> findFirstWinningBoard moves boards)
                |> Expect.equal
                    ( [ 24, 21, 14, 0, 2, 23, 17, 11, 5, 9, 4, 7 ]
                    , [ [ 14, 21, 17, 24, 4 ]
                      , [ 10, 16, 15, 9, 19 ]
                      , [ 18, 8, 23, 26, 20 ]
                      , [ 22, 11, 13, 6, 5 ]
                      , [ 2, 0, 12, 3, 7 ]
                      ]
                    )


part1 : Test
part1 =
    test "Part 1"
        (\_ ->
            testInput
                |> decode
                |> solve
                |> Expect.equal 4512
        )


part2 : Test
part2 =
    test "Part 2"
        (\_ ->
            testInput
                |> decode
                |> solve2
                |> Expect.equal 1924
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
                    { part1 = input |> solve
                    , part2 = input |> solve2
                    }
                )
            )


port receiveInput : (String -> msg) -> Sub msg


port output : Value -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveInput GotInput
