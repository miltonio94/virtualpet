module Instruction exposing (..)

import Html exposing (Html, beginnerProgram, div, p, text, a, button, br, input, form, fieldset, label)
import Html.Attributes exposing (class, type_, checked)
import Html.Events exposing (onClick)


main =
    beginnerProgram { model = [ defaultInstruction ], view = view, update = update }


type alias Instruction =
    { selectedInstruction : SelectedInstruction, amount : Int }


type SelectedInstruction
    = Forward
    | TurnRight
    | TurnLeft
    | Backward
    | Noise
    | NotSelected


defaultInstruction : Instruction
defaultInstruction =
    { selectedInstruction = NotSelected, amount = 0 }



-- type alias Instructions =
--     List Instruction


type alias Model =
    List Instruction


type Msg
    = NewInstruction Instruction
    | AddInstruction


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewInstruction instruction ->
            model

        AddInstruction ->
            List.append model [ defaultInstruction ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ bookends "Start"
        , div
            [ class "instructionSet" ]
            [ form
                []
                [ instructionsFromModel model
                ]
            , button
                [ onClick AddInstruction ]
                [ text "new instruction" ]
            ]
        , bookends "Finish"
        ]


bookends : String -> Html Msg
bookends a =
    div
        [ class "bookends" ]
        [ p
            []
            [ text a ]
        ]


instructionsFromModel : Model -> Html Msg
instructionsFromModel model =
    div [] (List.map (\x -> selectedInstructionToRadios x.selectedInstruction x.amount) model)


instruction : SelectedInstruction -> Int -> Html Msg
instruction i amount =
    div
        [ class "instruction" ]
        [ text <| selectedInstructionToString i
        , br [] []
        , text (toString amount)
        ]


selectedInstructionToString : SelectedInstruction -> String
selectedInstructionToString i =
    case i of
        Forward ->
            toString i

        TurnRight ->
            toString i

        TurnLeft ->
            toString i

        Backward ->
            toString i

        Noise ->
            toString i

        NotSelected ->
            toString i


selectedInstructionToRadios : SelectedInstruction -> Int -> Html Msg
selectedInstructionToRadios i amount =
    case i of
        Forward ->
            div []
                [ radio "Forward" True
                , radio "Turn Right" False
                , radio "Turn Left" False
                , radio "Backward" False
                , radio "Noise" False
                , text <| toString amount
                ]

        TurnRight ->
            div []
                [ radio "Forward" False
                , radio "TurnRight" True
                , radio "TurnLeft" False
                , radio "Backward" False
                , radio "Noise" False
                , text <| toString amount
                ]

        TurnLeft ->
            div []
                [ radio "Forward" False
                , radio "TurnRight" False
                , radio "TurnLeft" True
                , radio "Backward" False
                , radio "Noise" False
                , text <| toString amount
                ]

        Backward ->
            div []
                [ radio "Forward" False
                , radio "TurnRight" False
                , radio "TurnLeft" False
                , radio "Backward" True
                , radio "Noise" False
                , text <| toString amount
                ]

        Noise ->
            div []
                [ radio "Forward" False
                , radio "TurnRight" False
                , radio "TurnLeft" False
                , radio "Backward" False
                , radio "Noise" True
                , text <| toString amount
                ]

        NotSelected ->
            div []
                [ radio "Forward" False
                , radio "TurnRight" False
                , radio "TurnLeft" False
                , radio "Backward" False
                , radio "Noise" False
                , text <| toString amount
                ]


radio : String -> Bool -> Html Msg
radio name selected =
    label []
        [ input
            [ type_ "radio"
            , onClick <| NewInstruction defaultInstruction
            , checked selected
            ]
            []
        , text
            name
        , br [] []
        ]
