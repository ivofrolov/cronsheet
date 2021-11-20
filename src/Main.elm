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


type alias TimeCell = (Int, Int)


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
            { crontab = ""
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
            [ Css.fontFamily Css.monospace
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
                , Attrs.placeholder "Paste your crontab here\n\n* * * * * user command\n..."
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
        , renderResult model.cronstruct
        ]


gridStyle : Int -> Int -> Css.Style
gridStyle columns rows =
    Css.batch
        [ Css.property "--columns" (String.fromInt columns)
        , Css.property "--rows" (String.fromInt rows)
        , Css.property "display" "grid"
        , Css.property "row-gap" "1px"
        , Css.property "grid-template-columns" "200px repeat(var(--columns), 1fr)"
        , Css.property "grid-template-rows" "repeat(var(--rows), 1lh)"
        ]


gridCellStyle : Int -> Int -> Int -> Css.Style
gridCellStyle rowNumber columnStart columnEnd =
    Css.batch
        [ Css.property "grid-row" (String.fromInt rowNumber)
        , Css.property "grid-column-start" (String.fromInt columnStart)
        , Css.property "grid-column-end" (String.fromInt columnEnd)
        ] 


renderResult : Result (List LineError) (List Cronstruct) -> Html.Html Msg
renderResult result =
    let
        scaleValue = 15 -- minutes per grid cell
        extractSchedule (rule, command) = (command, generateTimeCells scaleValue rule)
    in
        case result of
            Ok values ->
                renderCronSchedule scaleValue (List.map extractSchedule values)
            Err errors ->
                renderErrors (List.map extractErrorMessage errors)


renderErrors : List String -> Html.Html Msg
renderErrors errors =
    Html.pre [] [ Html.text (String.join "\n" errors) ]


renderCronSchedule : Int -> List (String, List TimeCell) -> Html.Html Msg
renderCronSchedule scaleValue rows =
    -- NOTE: currently only one day is supported
    let
        columnsNumber = 24 * 60 // scaleValue
        rowsNumber = List.length rows
        body =
            rows
                |> List.map2 (\x (y, z) -> renderCronScheduleRow x y z) (List.range 1 rowsNumber)
                |> List.concat
    in
        Html.div
            [ Attrs.css
                [ Css.width (Css.vw 90)
                , Css.margin2 (Css.rem 1) (Css.zero)
                , gridStyle columnsNumber rowsNumber
                ]
            ]
            body


renderCronScheduleRow : Int -> String -> List TimeCell -> List (Html.Html Msg)
renderCronScheduleRow rowNumber command timeCells =
    let
        renderedCommand = renderCronScheduleCommandCell rowNumber command
        renderedTime = List.map (renderCronScheduleTimeCell rowNumber) timeCells
    in
        renderedCommand :: renderedTime


renderCronScheduleCommandCell : Int -> String -> Html.Html Msg
renderCronScheduleCommandCell rowNumber command =
    Html.span
        [ Attrs.css [ gridCellStyle rowNumber 1 1 ] ]
        [ Html.text command ]


renderCronScheduleTimeCell : Int -> TimeCell -> Html.Html Msg
renderCronScheduleTimeCell rowNumber (columnStart, columnEnd) =
    Html.div
        [ Attrs.css
            [ gridCellStyle rowNumber columnStart columnEnd
            , Css.backgroundColor (Css.hex "888888")
            ]
        ]
        []


extractErrorMessage : LineError -> String
extractErrorMessage (LineError line message) =
    String.join ": " [ String.fromInt line, message ]


generateTimeCells : Int -> Cron.Cron -> List TimeCell
generateTimeCells scaleValue (Cron.Cron m h dm mo dw) =
    -- NOTE: currently only one day is supported
    let
        toTimeCells x = Tuple.pair x (x + 1)
    in
        minuteTicks scaleValue m |> hourTicks scaleValue h |> List.map toTimeCells



-- TIME CELLS


minuteTicks : Int -> Cron.Expr Int -> List Int
minuteTicks scaleValue expr =
    case expr of
        Cron.Single term ->
            termTicks scaleValue term
        Cron.Multiple terms ->
            List.concatMap (termTicks scaleValue) terms |> unique
        Cron.Every ->
            List.range 1 (60 // scaleValue)


hourTicks : Int -> Cron.Expr Int -> List Int -> List Int
hourTicks scaleValue expr ticks =
    -- NOTE: currently only one day is supported
    let
        cellsPerHour = 60 // scaleValue
        offsets = List.range 0 23 |> List.map ((*) cellsPerHour) |> List.map ((+) 1)
        listAdd y xs = List.map ((+) y) xs
    in
        ticks
            |> List.repeat 24
            |> List.map2 listAdd offsets
            |> List.concat


termTicks : Int -> Cron.Term Int -> List Int
termTicks scaleValue term =
    case term of
        Cron.Atom atom ->
            atomTicks scaleValue atom
        Cron.EveryStep step ->
            everyStepTicks scaleValue 0 59 step
        Cron.Step atom step ->
            stepTicks scaleValue atom step


atomTicks : Int -> Cron.Atom Int -> List Int
atomTicks scaleValue atom =
    case atom of
        Cron.Particle single ->
            List.singleton (single // scaleValue + 1)
        Cron.Range start stop ->
            List.range (start // scaleValue + 1) (stop // scaleValue + 1)


stepTicks : Int -> Cron.Atom Int -> Int -> List Int
stepTicks scaleValue atom step =
    case atom of
        Cron.Particle start ->
            everyStepTicks scaleValue start 59 step
        Cron.Range start stop ->
            everyStepTicks scaleValue start stop step


everyStepTicks : Int -> Int -> Int -> Int -> List Int
everyStepTicks scaleValue start stop step =
    let
        effectiveStep = max scaleValue step
    in
        (stop - start) // effectiveStep
            |> List.range 0
            |> List.map (\x -> (x * effectiveStep) // scaleValue)
            |> List.map ((+) ((start // scaleValue) + 1))



-- UTILITIES


unique : List comparable -> List comparable
unique list =
    let
        step x ys =
            case ys of
                [] -> [x]
                y::_ -> if x == y then ys else x::ys
    in
        List.foldr step [] list
