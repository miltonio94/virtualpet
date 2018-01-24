module InstructionV2 exposing (..)

import Html exposing (Html, beginnerProgram, div, p, text, a, button, br, input, form, fieldset, label, select, option)
import Html.Attributes exposing (class, type_, checked, id, placeholder, selected, value, hidden)
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
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddInstruction i ->
            if i.instructionAmount <= 0 then
                model
            else
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

        Submit ->
            { model | instructions = [] }


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
    let
        isInstructionEmpty =
            List.isEmpty model.instructions
    in
        div
            []
            [ instructionMenu model
            , renderInstructions model
            , submitInstruction isInstructionEmpty
            ]


createOption : String -> InstructionType -> Html msg
createOption text_ currentSelected =
    -- if text_ == (instructionTypeToString currentSelected) then
    option
        [ instructionTypeToString currentSelected
            |> (==) text_
            |> selected
        ]
        [ text text_ ]


instructionMenu : Model -> Html Msg
instructionMenu model =
    let
        instructionType =
            model.currentInstruction.instructionType

        innerText =
            instructionAmountToString model.currentInstruction.instructionAmount
    in
        div
            [ id "instructionMenu" ]
            [ select
                [ onChange UpdateInstructionType
                ]
                [ createOption (instructionTypeToString Foward) instructionType
                , createOption (instructionTypeToString Backwards) instructionType
                , createOption (instructionTypeToString TurnLeft) instructionType
                , createOption (instructionTypeToString TurnRight) instructionType
                , createOption (instructionTypeToString MakeNoise) instructionType
                ]
            , input [ placeholder "Amount of", onInput UpdateInstructionAmount, value innerText ] [ text innerText ]
            , button
                [ onClick (AddInstruction model.currentInstruction) ]
                [ text "Create Instruction" ]
            ]


onChange a =
    on "change" (Json.map a targetValue)


renderInstructions : Model -> Html Msg
renderInstructions model =
    let
        instructions =
            model.instructions
    in
        div
            [ class "instructions_rendered" ]
            (List.map renderInstruction instructions)


submitInstruction : Bool -> Html Msg
submitInstruction isVisible =
    div
        [ hidden isVisible ]
        [ button [ onClick Submit ] [ text "Submit Instructions" ]
        ]


renderInstruction : Instruction -> Html Msg
renderInstruction instruction =
    let
        iType =
            instructionTypeToString instruction.instructionType

        amount =
            instructionAmountToString instruction.instructionAmount
    in
        div [ class "instruction" ] [ text (iType ++ " by " ++ amount) ]
