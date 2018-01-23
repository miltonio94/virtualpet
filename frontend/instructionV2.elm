module InstructionV2 exposing (..)

import Html exposing (Html, beginnerProgram, div, p, text, a, button, br, input, form, fieldset, label, select, option)
import Html.Attributes exposing (class, type_, checked, id, placeholder)
import Html.Events exposing (onClick, on, onInput, targetValue)
import Json.Decode as Json
import String exposing (toInt)


-- import Html.Events.Extra exposing (targetValueStringParse)


main =
    beginnerProgram { model = defaultModel, view = view, update = update }



-- Model


type InstructionType
    = Foward
    | Backwards
    | TurnLeft
    | TurnRight
    | MakeNoise
    | Empty


defaultModel =
    { instructions = []
    , currentInstruction = defaultInstruction
    }


defaultInstruction =
    { instructionType = Foward
    , instructionAmount = 0
    }


type alias InstructionAmount =
    Int


createInstruction : InstructionType -> InstructionAmount -> Instruction
createInstruction t a =
    { instructionType = t
    , instructionAmount = a
    }


type alias Instruction =
    { instructionType : InstructionType
    , instructionAmount : InstructionAmount
    }


type alias Instructions =
    List Instruction


type alias Model =
    { instructions : Instructions
    , currentInstruction : Instruction
    }



-- Update


type Msg
    = AddInstruction Instruction
    | UpdateInstructionType String
    | UpdateInstructionAmount String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddInstruction i ->
            { model
                | instructions = model.instructions ++ [ i ]
                , currentInstruction = defaultInstruction
            }

        UpdateInstructionType t ->
            let
                c =
                    model.currentInstruction

                updatedC =
                    { c
                        | instructionType = (stringToInstructionType t)
                    }
            in
                { model
                    | currentInstruction = updatedC
                }

        UpdateInstructionAmount a ->
            let
                c =
                    model.currentInstruction

                updatedC =
                    { c | instructionAmount = (stringToInstructionAmount a) }
            in
                { model | currentInstruction = updatedC }


stringToInstructionType : String -> InstructionType
stringToInstructionType iType =
    if iType == (instructionTypeToString Foward) then
        Foward
    else if iType == (instructionTypeToString Backwards) then
        Backwards
    else if iType == (instructionTypeToString TurnLeft) then
        TurnLeft
    else if iType == (instructionTypeToString TurnRight) then
        TurnRight
    else if iType == (instructionTypeToString MakeNoise) then
        MakeNoise
    else
        Empty


stringToInstructionAmount : String -> InstructionAmount
stringToInstructionAmount amount =
    Result.withDefault 0 (String.toInt amount)


instructionTypeToString : InstructionType -> String
instructionTypeToString iType =
    case iType of
        Foward ->
            toString Foward

        Backwards ->
            toString Backwards

        TurnLeft ->
            "Turn Left"

        TurnRight ->
            "Turn Right"

        MakeNoise ->
            "Make Noise"

        Empty ->
            ""


instructionAmountToString : InstructionAmount -> String
instructionAmountToString amount =
    toString amount



-- View


view model =
    instructionMenu model


createOption t s =
    option
        [ on "change" <|
            Json.succeed <|
                UpdateInstructionType t
        ]
        [ text s ]


instructionMenu : Model -> Html Msg
instructionMenu model =
    div
        [ id "instructionMenu" ]
        [ select
            [ onChange UpdateInstructionType
            ]
            [ option [] [ text "Foward" ]
            , option [] [ text "Backward" ]
            , option [] [ text "Turn Left" ]
            , option [] [ text "Turn Right" ]
            , option [] [ text "Make Noise" ]
            ]
        , input [ placeholder "Amount of", onInput UpdateInstructionAmount ] []
        , button
            [ onClick (AddInstruction model.currentInstruction) ]
            [ text "Create Instruction" ]
        ]


onChange a =
    on "change" (Json.map a targetValue)
