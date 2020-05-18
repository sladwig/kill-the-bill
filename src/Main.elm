module Main exposing (..)

import Base64 exposing (decode, encode)
import Browser
import Browser.Navigation exposing (Key, pushUrl)
import Html exposing (Html, b, br, div, span, table, td, text, textarea, tr)
import Html.Attributes exposing (class, cols, colspan, placeholder, rows, value)
import Html.Events exposing (onInput)
import String.Extra exposing (leftOfBack, rightOfBack)
import Url exposing (Url)
import Url.Builder exposing (Root(..), custom)
import Url.Parser exposing (Parser, fragment, parse)
-- import Round 
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale, spanishLocale, Decimals(..))

import Debug exposing (log)

-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = route
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Change String



-- MODEL


type alias Model =
    { key : Key
    , url : Url
    , content : String
    , editMode : Bool
    }



-- INIT


init : string -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    let
        content =
            tryDecoding (Maybe.withDefault "" (getContent url))

        editMode =
            String.length content < 1
    in
    ( { key = key, url = url, content = String.trim content, editMode = editMode }, Cmd.none )


getContent : Url -> Maybe String
getContent =
    parse fragmentParser >> Maybe.andThen identity


fragmentParser : Parser (Maybe String -> Maybe String) (Maybe String)
fragmentParser =
    fragment identity



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newContent ->
            ( { model | content = newContent }, pushUrl model.key (pathForInvoice newContent) )

        LinkClicked _ ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )



-- VIEWs


route : Model -> Browser.Document Msg
route model =
    if model.editMode then
        view model

    else
        viewInvoice model


view : Model -> Browser.Document Msg
view model =
    { title = "und Rechnung schreiben!"
    , body =
        [ div []
            [ textarea [ rows 20, cols 120, placeholder "Text", value model.content, onInput Change ] []
            , div [] ([] ++ viewItems model.content)
            ]
        ]
    }


viewInvoice : Model -> Browser.Document Msg
viewInvoice model =
    { title = "und die Rechnung bitte!"
    , body =
        [ div [ class "invoice-box" ]
            [ table [] (invoiceList model)
            ]
        ]
    }


invoiceList : Model -> List (Html Msg)
invoiceList model =
    [ tr [ class "top" ]
        [ td [ colspan 2 ]
            [ table []
                [ tr []
                    [ td [ class "title" ] [ br [] [], text "SAFTIG" ]
                    , td [ class "" ]
                        [ text "Kundennummer Nr #: 5342895"
                        , br [] []
                        , text "Clientennummer Nr #: 4326721344"
                        , br [] []
                        , text "Vorgansnummer Nr #: 461 343f-DRF454"
                        , br [] []
                        , b [] [ text "Bitte stets angeben!" ]
                        , br [] []
                        , br [] []
                        , br [] []
                        , br [] []
                        , text "Datum: heute"
                        , br [] []
                        , text "Fällig:"
                        , b [] [ text "gestern" ]
                        ]
                    ]
                ]
            ]
        ]
    , tr [ class "information" ]
        [ td [ colspan 2 ]
            [ table []
                [ tr []
                    [ td []
                        [ b [] [ text "Fette Rechnung Nr #: 5342895" ]
                        , br [] []
                        , br [] []
                        , br [] []
                        , text "Direkt an dich gerichtet"
                        , br [] []
                        ]
                    , td []
                        []
                    ]
                ]
            ]
        ]
    , viewItem "heading" "In Rechnung gestellt" "Kosten"
    ]
        ++ viewItems model.content
        ++ [ br [] []
           , viewItem "heading" "TOTAL" (moneyMaker (String.fromFloat (total model.content)))
           , br [] []
           , br [] []
           ]


numberArtist : Locale
numberArtist = {spanishLocale | decimals = Exact 2, thousandSeparator = ".", decimalSeparator = ","} 

viewItems : String -> List (Html Msg)
viewItems allItems =
    List.map parseLine (String.lines allItems)


parseLine : String -> Html Msg
parseLine line =
    let
        item =
            leftOfBack " " line

        value =
            rightOfBack " " line
    in
    viewItem "item" item value


total : String -> Float
total allItems =
    List.sum (List.map totalLine (String.lines allItems))


totalLine : String -> Float
totalLine line =
    Maybe.withDefault 0 (asFloat (rightOfBack " " line))

-- try to convert to money and leave as is, if conversion fails
moneyMaker : String -> String
moneyMaker value = 
    case (asFloat value) of
        Just money -> currencyfy money
        Nothing -> value
-- Basics.abs    Maybe.withDefault value (currencyfy (asFloat value))


currencyfy : Float -> String
currencyfy value =
    (format numberArtist value) ++ " €"

asFloat : String -> Maybe Float
asFloat value =
     (String.toFloat (String.replace "," "." value))


-- heading


viewItem : String -> String -> String -> Html Msg
viewItem itemClass description value =
    tr [ class itemClass ] [ td [] [ text description ], td [] [ text (moneyMaker value)] ]



-- HELPER


tryDecoding : String -> String
tryDecoding result =
    case decode result of
        Err _ ->
            "can't decode"

        Ok decoded ->
            decoded


encoding : String -> String
encoding toEncode =
    encode toEncode


pathForInvoice : String -> String
pathForInvoice asInvoice =
    custom Absolute [] [] (Just (encode asInvoice))
