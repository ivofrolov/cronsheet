module Main exposing (..)

import Browser
import Cron
import Css as Css
import Html.Styled as Html
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> 
            { title = "Cronsheet"
            , body = List.map Html.toUnstyled [ view model ]
            }
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Cronstruct = (Cron.Cron, String)


type LineError
    = LineError Int String


type alias Model =
    { crontab : String
    , cronstruct : Result (List LineError) (List Cronstruct)
    }


init : () -> (Model, Cmd Msg)
init _ =
    let
        initialModel =
            { crontab = "* * * * * root reboot\n* * * * * user ping"
            , cronstruct = Ok []
            }
    in
        (initialModel, Cmd.none)



-- UPDATE


type Msg
    = Input String
    | Parse


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Input newContent ->
            ({ model | crontab = newContent }, Cmd.none)

        Parse ->
            ({ model | cronstruct = (parseCrontab model.crontab) }, Cmd.none)


parseCrontab : String -> Result (List LineError) (List Cronstruct)
parseCrontab crontab = 
    let
        (errors, values) = partitionByError (List.map parseCronline (String.lines crontab))
    in
        if List.isEmpty errors then
            Ok values
        else
            Err errors


parseCronline : String -> Result String Cronstruct
parseCronline cronline =
    let
        rule = parseCronRule cronline
        command = parseCronCommand cronline
    in
        Result.map2 Tuple.pair rule command


parseCronRule : String -> Result String Cron.Cron
parseCronRule cronline =
    let
        cronstring = String.join " " (List.take 5 (String.words cronline))
    in
        Result.mapError (\_ -> "Invalid cron string.") (Cron.fromString cronstring)


parseCronCommand : String -> Result String String
parseCronCommand cronline =
    let
        command = String.join " " (List.drop 6 (String.words cronline))
    in
        if String.isEmpty command then
            Err "No command specified."
        else
            Ok command


partitionByError : List (Result String value) -> (List LineError, List value)
partitionByError list =
    let
        step (i, x) (errors, values) =
            case x of
                Ok value -> (errors, value :: values)
                Err error -> (LineError (i + 1) error :: errors, values)
    in
        list
            |> List.indexedMap Tuple.pair
            |> List.foldr step ([], [])



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div
        [ Attrs.css
            [ Css.fontFamily Css.sansSerif
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.alignItems Css.center
            ]
        ]
        [ Html.h1 [] [ Html.text "Cronsheet" ]
        , Html.form
            [ Attrs.css
                [ Css.width (Css.vw 50)
                , Css.displayFlex
                , Css.flexDirection Css.column
                ]
            , Events.onSubmit Parse
            ]
            [ Html.textarea
                [ Attrs.rows 10
                , Attrs.wrap "off"
                , Attrs.placeholder "Paste your crontab here"
                , Attrs.value model.crontab
                , Events.onInput Input
                ]
                []
            , Html.button
                [ Attrs.disabled (String.isEmpty model.crontab)
                , Attrs.css 
                    [ Css.alignSelf Css.flexEnd
                    ]
                ]
                [ Html.text "Parse" ]
            ]
        , Html.pre [] [ Html.text (Debug.toString model.cronstruct) ]
        , parseResult model.cronstruct
        ]


parseResult : Result (List LineError) (List Cronstruct) -> Html.Html Msg
parseResult result =
    case result of
        Ok value -> cronSchedule value
        Err error -> errorMessage error


errorMessage : List LineError -> Html.Html Msg
errorMessage errors =
    let
        toString (LineError idx err) =
            String.join ": " [ String.fromInt idx, err ]
        rendered =
            errors
                |> List.map toString
                |> String.join "\n"
    in
        Html.pre [] [ Html.text rendered ]


gridTemplate : Int -> Int -> Css.Style
gridTemplate columns rows =
    Css.batch
        [ Css.property "--columns" (String.fromInt columns)
        , Css.property "--rows" (String.fromInt rows)
        , Css.property "display" "grid"
        , Css.property "row-gap" "1px"
        , Css.property "grid-template-columns" "200px repeat(var(--columns), 1fr)"
        , Css.property "grid-template-rows" "repeat(var(--rows), 1lh)"
        ]


cronSchedule : List Cronstruct -> Html.Html Msg
cronSchedule cronstructs =
    let
        rows = List.length cronstructs
        cols = 24 * 60 // 15  -- one day with a division value of 15 minutes
    in
        Html.div
            [ Attrs.css [ Css.width (Css.vw 90), gridTemplate cols rows ] ]
            (List.concat (List.indexedMap (scheduleRow 15) cronstructs))


scheduleRow : Int -> Int -> Cronstruct -> List (Html.Html Msg)
scheduleRow divisionValue rowNumber (rule, command) =
    scheduleCommand (rowNumber + 1) command :: scheduleTicks divisionValue (rowNumber + 1) rule


scheduleCommand : Int -> String -> Html.Html Msg
scheduleCommand rowNumber command =
    Html.span
        [ Attrs.css [ Css.property "grid-row" (String.fromInt rowNumber) ] ]
        [ Html.text command ]


gridCell : Int -> (Int, Int) -> Html.Html Msg
gridCell rowNumber (start, end) =
    Html.div
        [ Attrs.css
            [ Css.property "grid-row" (String.fromInt rowNumber)
            , Css.property "grid-column-start" (String.fromInt start)
            , Css.property "grid-column-end" (String.fromInt end)
            , Css.backgroundColor (Css.hex "55af6a")
            ] 
        ]
        []


scheduleTicks : Int -> Int -> Cron.Cron -> List (Html.Html Msg)
scheduleTicks divisionValue rowNumber (Cron.Cron m h dm mo dw) =
    minuteTicks (60 // divisionValue) m
    |> hourTicks (60 // divisionValue) h
    |> List.map (gridCell rowNumber)


minuteTicks : Int -> Cron.Expr Int -> List (Int, Int)
minuteTicks cells expr =
    (1, 1) :: []


hourTicks : Int -> Cron.Expr Int -> List (Int, Int) -> List (Int, Int)
hourTicks cells expr ticks =
    let
        offsets = List.range 0 23 |> List.map ((*) cells) |> List.map ((+) 1)
    in
        ticks
            |> List.repeat 24
            |> List.map2 offsetTicks offsets
            |> List.concat


offsetTicks : Int -> List (Int, Int) -> List (Int, Int)
offsetTicks offset ticks =
    let
        fn = (+) offset
    in
        List.map (Tuple.mapBoth fn fn) ticks
