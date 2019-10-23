module Main exposing (main)

import Browser
import Debug exposing (log)
import Html exposing (Html, br, button, div, input, span, text)
import Html.Attributes exposing (checked, disabled, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import List
import Maybe exposing (Maybe)


type alias Person =
    { name : String, isMan : Maybe Bool }


recordToString : Person -> String
recordToString r =
    "{ Name: " ++ r.name ++ ", Gender: " ++ gender r.isMan ++ " }"


gender : Maybe Bool -> String
gender isMan =
    case isMan of
        Maybe.Just True ->
            "M"

        Maybe.Just False ->
            "F"

        Maybe.Nothing ->
            "?"


byIndex : Model -> Maybe Person
byIndex { pos, list } =
    List.head (List.reverse (List.take pos list))


atPosition : Model -> String
atPosition model =
    case byIndex model of
        Nothing ->
            ""

        Just rec ->
            recordToString rec


type Msg
    = Left
    | Right
    | BufferName String
    | ToggleBufferGender
    | Add


type alias Model =
    { pos : Int, list : List Person, buffer : Person }


init : Model
init =
    { pos = 1
    , list =
        [ { name = "Den", isMan = Maybe.Just True }
        , { name = "Tanya", isMan = Maybe.Just False }
        , { name = "Glitch the Alien", isMan = Nothing }
        ]
    , buffer = { name = "", isMan = Nothing }
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Left ->
            if atFarLeft model then
                model

            else
                { model | pos = model.pos - 1 }

        Right ->
            if atFarRight model then
                model

            else
                { model | pos = model.pos + 1 }

        BufferName text ->
            let
                { pos, list, buffer } =
                    model
            in
            { model | buffer = { buffer | name = text } }

        ToggleBufferGender ->
            let
                { pos, list, buffer } =
                    model
            in
            { model | buffer = { buffer | isMan = Just (not (maybeToBool buffer.isMan)) } }

        Add ->
            { model
                | list = model.list ++ [ { name = model.buffer.name, isMan = model.buffer.isMan } ]
                , buffer = { name = "", isMan = Nothing }
            }


checkboxToBool : String -> Bool
checkboxToBool check =
    case check of
        "on" ->
            log "Checkbox: on" True

        a ->
            log ("Checkbox: " ++ a) False


atFarLeft : Model -> Bool
atFarLeft { pos } =
    pos == 1


atFarRight : Model -> Bool
atFarRight { pos, list } =
    pos == List.length list


maybeToBool : Maybe Bool -> Bool
maybeToBool bool =
    case bool of
        Nothing ->
            False

        Just b ->
            b


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Left, disabled (atFarLeft model) ] [ text "<" ]
        , div [] [ text (atPosition model) ]
        , button [ onClick Right, disabled (atFarRight model) ] [ text ">" ]
        , br [] []
        , span [] [ text "Name: " ]
        , input [ value model.buffer.name, onInput BufferName ] []
        , span [] [ text ("Gender: " ++ gender model.buffer.isMan) ]
        , input [ type_ "checkbox", checked (maybeToBool model.buffer.isMan), onClick ToggleBufferGender ] []
        , button [ onClick Add ] [ text "Add" ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
