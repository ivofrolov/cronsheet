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
        , view =
            \model ->
                { title = "Cronsheet"
                , body = List.map Html.toUnstyled [ view model ]
                }
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Cronstruct =
    ( Cron.Cron, String )


type alias TimeCell =
    ( Int, Int )


type LineError
    = LineError Int String


type alias Model =
    { crontab : String
    , cronstruct : Result (List LineError) (List Cronstruct)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialModel =
            { crontab = ""
            , cronstruct = Ok []
            }
    in
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = Input String
    | Parse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input newContent ->
            ( { model | crontab = newContent }, Cmd.none )

        Parse ->
            ( { model | cronstruct = parseCrontab model.crontab }, Cmd.none )


parseCrontab : String -> Result (List LineError) (List Cronstruct)
parseCrontab crontab =
    let
        ( errors, values ) =
            partitionByError (List.map parseCronline (String.lines crontab))
    in
    if List.isEmpty errors then
        Ok values

    else
        Err errors


parseCronline : String -> Result String Cronstruct
parseCronline cronline =
    let
        rule =
            parseCronRule cronline

        command =
            parseCronCommand cronline
    in
    Result.map2 Tuple.pair rule command


parseCronRule : String -> Result String Cron.Cron
parseCronRule cronline =
    let
        cronstring =
            String.join " " (List.take 5 (String.words cronline))
    in
    Result.mapError (\_ -> "Invalid cron string.") (Cron.fromString cronstring)


parseCronCommand : String -> Result String String
parseCronCommand cronline =
    let
        command =
            String.join " " (List.drop 6 (String.words cronline))
    in
    if String.isEmpty command then
        Err "No command specified."

    else
        Ok command


partitionByError : List (Result String value) -> ( List LineError, List value )
partitionByError list =
    let
        step ( i, x ) ( errors, values ) =
            case x of
                Ok value ->
                    ( errors, value :: values )

                Err error ->
                    ( LineError (i + 1) error :: errors, values )
    in
    list
        |> List.indexedMap Tuple.pair
        |> List.foldr step ( [], [] )



-- STYLES


defaultFontStyle : Css.Style
defaultFontStyle =
    Css.batch
        [ Css.fontFamily Css.monospace
        ]


defaultBorderStyle : Css.Style
defaultBorderStyle =
    Css.batch
        [ Css.border3 (Css.px 1) Css.solid (Css.hex "#888")
        ]


gridStyle : Int -> Int -> Css.Style
gridStyle columns rows =
    Css.batch
        [ Css.property "--columns" (String.fromInt columns)
        , Css.property "--rows" (String.fromInt rows)
        , Css.property "display" "grid"
        , Css.property "row-gap" "1px"
        , Css.property "grid-template-columns" "33ch repeat(var(--columns), 1fr)"
        , Css.property "grid-template-rows" "repeat(var(--rows), 1lh)"
        ]


gridCellStyle : Int -> Int -> Int -> Int -> Css.Style
gridCellStyle rowStart rowEnd columnStart columnEnd =
    Css.batch
        [ Css.property "grid-row-start" (String.fromInt rowStart)
        , Css.property "grid-row-end" (String.fromInt rowEnd)
        , Css.property "grid-column-start" (String.fromInt columnStart)
        , Css.property "grid-column-end" (String.fromInt columnEnd)
        ]


commandHeaderStyle : Css.Style
commandHeaderStyle =
    Css.batch
        [ Css.borderBottom3 (Css.px 1) Css.solid (Css.hex "#888")
        ]


commandStyle : Css.Style
commandStyle =
    Css.batch
        [ Css.whiteSpace Css.noWrap
        , Css.overflow Css.hidden
        , Css.textOverflow Css.ellipsis
        , Css.paddingRight (Css.ch 1)
        ]


busyTimeSlotStyle : Css.Style
busyTimeSlotStyle =
    Css.batch
        [ Css.backgroundColor (Css.hex "#888")
        ]



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.main_
        [ Attrs.css
            [ defaultFontStyle
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
                , Attrs.placeholder "Paste your crontab here and press the button"
                , Attrs.value model.crontab
                , Attrs.css
                    [ defaultBorderStyle
                    ]
                , Events.onInput Input
                ]
                []
            , Html.button
                [ Attrs.disabled (String.isEmpty model.crontab)
                , Attrs.css
                    [ defaultFontStyle
                    , defaultBorderStyle
                    , Css.alignSelf Css.flexEnd
                    ]
                ]
                [ Html.text "Show the schedule" ]
            ]
        , renderResult model.cronstruct
        ]


renderResult : Result (List LineError) (List Cronstruct) -> Html.Html Msg
renderResult result =
    let
        extractSchedule ( rule, command ) =
            ( command, generateTimeCells dayScaleFactor hourScaleFactor rule )

        -- we will render 15 minutes per grid cell in 24 hour row
        -- so currently only one day is supported
        hourScaleFactor =
            4

        dayScaleFactor =
            24

        columnsNumber =
            dayScaleFactor * hourScaleFactor
    in
    case result of
        Ok values ->
            renderCronSchedule columnsNumber (List.map extractSchedule values)

        Err errors ->
            renderErrors (List.map extractErrorMessage errors)


renderErrors : List String -> Html.Html Msg
renderErrors errors =
    Html.pre [] [ Html.text (String.join "\n" errors) ]


renderCronSchedule : Int -> List ( String, List TimeCell ) -> Html.Html Msg
renderCronSchedule columnsNumber rows =
    let
        rowsNumber =
            List.length rows

        header =
            renderCronScheduleHeader 1

        body =
            rows
                |> List.map2 (\x ( y, z ) -> renderCronScheduleRow x y z) (List.range 2 (rowsNumber + 1))
                |> List.concat
    in
    Html.div
        [ Attrs.css
            [ Css.width (Css.vw 90)
            , Css.margin2 (Css.rem 1) Css.zero
            , gridStyle columnsNumber rowsNumber
            ]
        ]
        (List.append header body)


renderCell : Int -> Int -> Int -> Int -> List Css.Style -> List (Html.Html Msg) -> Html.Html Msg
renderCell rowStart rowEnd columnStart columnEnd styles content =
    Html.div
        [ Attrs.css
            (List.append
                styles
                [ gridCellStyle rowStart rowEnd columnStart columnEnd
                ]
            )
        ]
        content


renderCronScheduleHeader : Int -> List (Html.Html Msg)
renderCronScheduleHeader rowNumber =
    let
        renderCommandHeader =
            renderCell rowNumber (rowNumber + 1) 1 2 [ commandHeaderStyle ] [ Html.text "Command / Time, hours" ]

        renderTimeSlotHeader h =
            renderCell rowNumber (rowNumber + 1) (h * 4 + 2) (h * 4 + 4 + 2) [] [ Html.text (String.fromInt h) ]
    in
    renderCommandHeader :: List.map renderTimeSlotHeader (List.range 0 23)


renderCronScheduleRow : Int -> String -> List TimeCell -> List (Html.Html Msg)
renderCronScheduleRow rowNumber command timeCells =
    let
        renderCommand =
            renderCell rowNumber (rowNumber + 1) 1 2 [ commandStyle ] [ Html.text command ]

        renderTimeSlot ( columnStart, columnEnd ) =
            renderCell rowNumber (rowNumber + 1) (columnStart + 2) (columnEnd + 2) [ busyTimeSlotStyle ] []
    in
    renderCommand :: List.map renderTimeSlot timeCells


extractErrorMessage : LineError -> String
extractErrorMessage (LineError line message) =
    String.join ": " [ String.fromInt line, message ]


generateTimeCells : Int -> Int -> Cron.Cron -> List TimeCell
generateTimeCells dayScaleFactor hourScaleFactor (Cron.Cron m h dm mo dw) =
    -- currently only one day is supported
    let
        toTimeCell x =
            Tuple.pair x (x + 1)

        minutes =
            minuteTicks hourScaleFactor m

        hours =
            hourTicks dayScaleFactor h
    in
    mapOffset minutes (scale hourScaleFactor hours)
        |> List.concat
        |> List.map toTimeCell



-- TIME TICKS


minuteTicks : Int -> Cron.Expr Int -> List Int
minuteTicks hourScaleFactor expr =
    exprTicks 60 hourScaleFactor expr


hourTicks : Int -> Cron.Expr Int -> List Int
hourTicks dayScaleFactor expr =
    exprTicks 24 dayScaleFactor expr


exprTicks : Int -> Int -> Cron.Expr Int -> List Int
exprTicks real scaled expr =
    case expr of
        Cron.Single term ->
            termTicks real scaled term

        Cron.Multiple terms ->
            unique (List.concatMap (termTicks real scaled) terms)

        Cron.Every ->
            ticks (real // scaled) 0 (real - 1) 1


termTicks : Int -> Int -> Cron.Term Int -> List Int
termTicks real scaled term =
    case term of
        Cron.Atom atom ->
            atomTicks real scaled atom

        Cron.Step atom step ->
            stepTicks real scaled atom step

        Cron.EveryStep step ->
            ticks (real // scaled) 0 (real - 1) step


atomTicks : Int -> Int -> Cron.Atom Int -> List Int
atomTicks real scaled atom =
    case atom of
        Cron.Particle start ->
            ticks (real // scaled) start start 1

        Cron.Range start stop ->
            ticks (real // scaled) start stop 1


stepTicks : Int -> Int -> Cron.Atom Int -> Int -> List Int
stepTicks real scaled atom step =
    case atom of
        Cron.Particle start ->
            ticks (real // scaled) start (real - 1) step

        Cron.Range start stop ->
            ticks (real // scaled) start stop step


ticks : Int -> Int -> Int -> Int -> List Int
ticks scaledStep start stop step =
    let
        effectiveStep =
            max scaledStep step
    in
    List.range 0 ((stop - start) // effectiveStep)
        |> List.map (\x -> (x * effectiveStep + start) // scaledStep)



-- UTILITIES


unique : List comparable -> List comparable
unique list =
    let
        step x ys =
            case ys of
                [] ->
                    [ x ]

                y :: _ ->
                    if x == y then
                        ys

                    else
                        x :: ys
    in
    List.foldr step [] list


scale : number -> List number -> List number
scale factor values =
    List.map ((*) factor) values


offset : number -> List number -> List number
offset amount values =
    List.map ((+) amount) values


mapOffset : List number -> List number -> List (List number)
mapOffset values amounts =
    List.map (\amount -> offset amount values) amounts
