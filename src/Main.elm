module Main exposing (..)

import Base64 exposing (decode, encode)
import Browser
import Browser.Navigation exposing (Key, pushUrl)
import Html exposing (Html, div, text, textarea)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)
import Url exposing (Url)
import Url.Builder exposing (Root(..), custom)
import Url.Parser exposing (Parser, fragment, parse)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
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
    }



-- INIT


init : string -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key, url = url, content = tryDecoding (Maybe.withDefault "" (getContent url)) }, Cmd.none )


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


view : Model -> Browser.Document Msg
view model =
    { title = "Die Rechnung bitte!"
    , body =
        [ text (Url.toString model.url)
        , div []
            [ textarea [ placeholder "Text", value model.content, onInput Change ] []
            , div [] (viewItems model.content)
            ]
        ]
    }


viewItems : String -> List (Html Msg)
viewItems allItems =
    List.map viewItem (String.lines allItems)


viewItem : String -> Html Msg
viewItem item =
    div [] [ text item ]



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
