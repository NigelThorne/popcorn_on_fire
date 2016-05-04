module Main (main) where

import Dict exposing (Dict)
import ElmFire
import ElmFire.Dict
import ElmFire.Op
import Html exposing (Attribute, Html, button, div, input, li, ol, text, span)
import Html.Attributes exposing (name, value, type', placeholder)
import Html.Events exposing (on, onClick, targetValue)
import Json.Decode
import Json.Encode
import Signal exposing (Address, Mailbox)
import String
import Task exposing (Task)


url =
    "https://popcornflow.firebaseio.com/"

database =
    { location = ElmFire.fromUrl url
    , orderOptions = ElmFire.noOrder
    , encoder = Json.Encode.int
    , decoder = Json.Decode.int
    }

type alias Name = String
type alias ProblemId = Int
type alias ExperimentId = Int
type alias OptionId = Int
type alias Finding = String
type alias Conclusion = String
type alias Description = String
type alias Expectation = String
type alias Observation = String
type alias Observer = String

type Action
    = NoOp
    | NewProblem Name ProblemId
    --| IncreasePriority ProblemId
    --| NewOption ProblemId OptionId
    --| NewExperiment OptionId ExperimentId
    --| CommitToExperiment ExperimentId
    --| StartExperiment ExperimentId
    --| AddFindingsToExperiment ExperimentId Finding
    --| EndExperiment ExperimentId
    --| ConcludeExperiment ExperimentId Conclusion

actions : Mailbox Action
actions =
    Signal.mailbox NoOp

--fetchDict : Task ElmFire.Error ()
--fetchDict =
--    ElmFire.Dict.getDict database `Task.andThen` (Dict >> Signal.send actions.address)

--adjustDict : String -> Int -> Task ElmFire.Error ElmFire.Reference
--adjustDict name score =
--    ElmFire.Op.operate database (
--        ElmFire.Op.update name (
--            Just << Maybe.withDefault score << Maybe.map (max score)))

type alias Model =
    { state : Dict String Int, nextProblemId : ProblemId }

type alias Problem =
    { id : ProblemId, name : Name, description : Description, observer: Observer }

update : Action -> ( Model, x ) -> ( Model, Maybe (Task ElmFire.Error ()) )
update action ( model, _ ) =
    case action of
        NoOp ->
            ( model, Nothing )

        --Dict dict ->
        --    ( { model | state = dict }, Nothing )

        --Name name ->
        --    ( { model | name = name }, Nothing )

        --Score score ->
        --    ( { model | score = score }, Nothing )

        --SubmitAndFetch ->
        --    ( model, Just (adjustDict model.name model.score `Task.andThen` always fetchDict) )


onInput : Address a -> (String -> a) -> Attribute
onInput addr fun =
    on "input" targetValue (Signal.message addr << fun)


view : Model -> Html
view { model } =
    div
        []
        [ viewProblemForm model
        , div [] (List.map (\pair -> viewProblem (snd pair) ) (Dict.toList model))
        ]

viewProblem : Problem -> Html
viewProblem problem = 
    div 
        []
        [ span [ type' "hidden", name "ProblemId"] [ text problem.id ]
        , span [ placeholder "Name", name "Name"] [ text problem.name ]
        , span [ placeholder "Description", name "Description"] [ text problem.description ]
        , span [ placeholder "Observer", name "Observer"] [ text problem.observer ] 
        ] 

viewProblemForm : Model -> Html
viewProblemForm model = 
    div 
        []
        [ input [ type' "hidden", name "ProblemId", value model.nextProblemId ] []
        , input [ placeholder "Name", name "Name" ] []
        , input [ placeholder "Description", name "Description" ] []
        , input [ placeholder "Observer", name "Observer" ] []
        , button [ onClick actions.address NewProblem ] [ text "Add Problem" ]
        ] 

--viewOptionForm : Model -> Html
--viewOptionForm = 
--    div 
--        []
--        [ input [ type' "hidden", onInput actions.address ProblemId ] []
--        , input [ type' "hidden", onInput actions.address OptionId ] []
--        , input [ placeholder "Option", onInput actions.address Option ] []
--        , input [ placeholder "Description", onInput actions.address Description ] []
--        , button [ onClick actions.address NewOption ] [ text "Add Option" ]
--        ] 

--viewExperimentForm : Model -> Html
--viewExperimentForm =
--    div 
--        []
--        [ input [ type' "hidden", onInput actions.address OptionId ] []
--        , input [ type' "hidden", onInput actions.address ExperimentId ] []
--        , input [ placeholder "Experiment", onInput actions.address Experiment ] []
--        , input [ placeholder "Description", onInput actions.address Description ] []
--        , input [ placeholder "Expectation", onInput actions.address Expectation ] []
--        , input [ placeholder "Observation", onInput actions.address Observation ] []
--        , button [ onClick actions.address NewExperiment ] [ text "Add Experiment" ]
--        ] 


modelAndTasks : Signal ( Model, Maybe (Task ElmFire.Error ()) )
modelAndTasks =
    Signal.foldp update ( { dict = Dict.empty, name = "", score = 0 }, Just fetchDict ) actions.signal


main : Signal Html
main =
    Signal.map (view << fst) modelAndTasks


port run : Signal (Task ElmFire.Error ())
port run =
    Signal.filterMap snd (Task.succeed ()) modelAndTasks
