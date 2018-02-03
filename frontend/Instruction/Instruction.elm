module InstructionV2 exposing (..)

import Html exposing (Html, program, div, p, text, a, button, br, input, form, fieldset, label, select, option)
import Html.Attributes exposing (class, type_, checked, id, placeholder, selected, value, hidden)
import Html.Events exposing (onClick, on, onInput, targetValue)
import Json.Decode as Json
import Json.Encode as EJson
import String exposing (toInt)
import Http


-- import Html.Events.Extra exposing (targetValueStringParse)


main =
    program
        { init = ( defaultModel, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        , update = update
        }



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
    | PostResult (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddInstruction i ->
            if i.instructionAmount <= 0 then
                ( model, Cmd.none )
            else
                ( { model
                    | instructions = i :: model.instructions
                    , currentInstruction = defaultInstruction
                  }
                , Cmd.none
                )

        UpdateInstructionType t ->
            let
                c =
                    model.currentInstruction

                updatedC =
                    { c
                        | instructionType = (stringToInstructionType t)
                    }
            in
                ( { model
                    | currentInstruction = updatedC
                  }
                , Cmd.none
                )

        UpdateInstructionAmount a ->
            let
                c =
                    model.currentInstruction

                updatedC =
                    { c | instructionAmount = (stringToInstructionAmount a) }
            in
                ( { model | currentInstruction = updatedC }, Cmd.none )

        Submit ->
            ( { model | instructions = [] }, postInstructions model )

        PostResult e ->
            ( model, Cmd.none )


instructionsToListOfTuples : Instructions -> List EJson.Value
instructionsToListOfTuples instructions =
    List.map
        (\i ->
            EJson.object
                [ ( "instructionType", (EJson.string <| instructionTypeToString <| i.instructionType) )
                , ( "instructionAmount", EJson.string <| instructionAmountToString <| i.instructionAmount )
                ]
        )
        instructions


postInstructions model =
    let
        data =
            instructionsToListOfTuples model.instructions

        post =
            Http.post
                "/postInstructions"
                (Http.jsonBody
                    (EJson.list data)
                )
                Json.string
    in
        Http.send PostResult post


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



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
