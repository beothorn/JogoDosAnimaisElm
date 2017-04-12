module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

main : Program Never Model Msg
main = beginnerProgram { 
    model = startingScreenModel
    ,view = view
    ,update = update
 }

view : Model -> Html Msg
view model = 
    case model.currentScreen of
        Introduction -> introduction
        AskAbout animalNode -> askAbout animalNode
        AskForNewAnimal wrong -> giveUp wrong model.animalInput
        AskForDifference wrongAnimal -> askDifference wrongAnimal model.animalInput model.differenceInput
        RightAnswer -> rightAnswer

update : Msg -> Model -> Model
update message model = 
    case message of
        StartGame -> { model |
            animalInput = ""
            ,differenceInput = ""
            ,currentScreen = AskAbout model.animalTree
        }
        RestartGame -> changeScreen Introduction model
        RightAnimal -> changeScreen RightAnswer model
        WrongAnimal wrong -> changeScreen (AskForNewAnimal wrong) model
        EnterNewAnimal wrongAnimal -> changeScreen (AskForDifference wrongAnimal) model
        UpdateAnimalTree wrongGuess rightAnimal currentDifference -> { model | 
            currentScreen = Introduction
            ,animalTree = insertOnTree wrongGuess rightAnimal currentDifference model.animalTree
        }
        GoAskAbout node -> changeScreen (AskAbout node) model
        AnimalInput animal -> { model | animalInput = animal }
        DifferenceInput difference -> { model | differenceInput = difference }

type AnimalNode = 
    Question String AnimalNode AnimalNode 
    | Animal String

type Msg = 
    StartGame
    | RestartGame
    | RightAnimal
    | WrongAnimal String
    | AnimalInput String
    | DifferenceInput String
    | EnterNewAnimal String
    | UpdateAnimalTree String String String
    | GoAskAbout AnimalNode

type Screen = 
    Introduction
    | AskAbout AnimalNode
    | AskForNewAnimal String
    | AskForDifference String
    | RightAnswer

type alias Model = {
    currentScreen : Screen
    ,animalTree : AnimalNode
    ,animalInput : String
    ,differenceInput : String
}

startingTree : AnimalNode
startingTree = (
    Question "é carnívoro" 
        (Question "pode ser animal de estimação" 
            (Question "late" 
                (Animal "cachorro")
                (Animal "gato"))
            (Question "é aquático" 
                (Animal "tubarão")
                (Animal "leão")))
        (Question "pode ser animal de estimação" 
            (Question "é grande" 
                (Animal "cavalo")
                (Question "é aquático" 
                    (Animal "peixe")
                    (Animal "tartaruga")))
            (Question "voa" 
                (Animal "borboleta")
                (Question "é aquático" 
                    (Animal "golfinho")
                    (Animal "águia")))))

startingScreenModel : Model
startingScreenModel = 
    Model 
        Introduction
        startingTree 
        ""
        ""

introduction : Html Msg
introduction = div [] [
    p [] [text "Imagine um animal"]
    ,button [onClick StartGame] [text "OK"]
 ]

askIfAnimal : String -> Html Msg
askIfAnimal animal = div [] [
    p [] [text ("O animal é "++ animal ++"?")]
    ,button [onClick RightAnimal] [text "Sim"]
    ,button [onClick (WrongAnimal animal)] [text "Não"]
 ]

askIfAnimalDoes : String -> AnimalNode -> AnimalNode -> Html Msg
askIfAnimalDoes questionText nodeYes nodeNo = div [] [
    p [] [text ("Hmm... Por acaso o animal "++ questionText ++"?")]
    ,button [onClick (GoAskAbout nodeYes)] [text "Sim"]
    ,button [onClick (GoAskAbout nodeNo)] [text "Não"]
 ]

askAbout : AnimalNode -> Html Msg
askAbout animalNode = 
    case animalNode of
        Question questionText nodeYes nodeNo -> askIfAnimalDoes questionText nodeYes nodeNo
        Animal animalName -> askIfAnimal animalName

rightAnswer : Html Msg
rightAnswer = div [] [
    p [] [text "Acertei!"]
    ,button [onClick RestartGame] [text "Jogar Novamente"]
 ]

giveUp : String -> String -> Html Msg
giveUp wrongAnimal currentAnimalInput = div [] [
    p [] [text "Desisto! Qual era o animal?"]
    ,input [
        placeholder "nome do Animal (minúsculo)"
        ,value currentAnimalInput
        ,onInput AnimalInput
    ] []
    ,button [onClick (EnterNewAnimal wrongAnimal)] [text "OK"]
 ]

askDifference : String -> String -> String -> Html Msg
askDifference wrongGuess rightAnimal currentDifference = div [] [
    p [] [text (wrongGuess++" é diferente de "++rightAnimal++" porque "++rightAnimal)]
    ,input [
        placeholder "nada, voa, tem cauda"
        ,value currentDifference
        ,onInput DifferenceInput
    ] []
    ,button [onClick (UpdateAnimalTree wrongGuess rightAnimal currentDifference)] [text "OK"]
 ]

insertOnTree : String -> String -> String -> AnimalNode -> AnimalNode
insertOnTree animalNode newAnimalNode newQuestion oldTree = 
    case oldTree of
        Question questionText nodeYes nodeNo -> 
            Question 
                questionText 
                (insertOnTree animalNode newAnimalNode newQuestion nodeYes) 
                (insertOnTree animalNode newAnimalNode newQuestion nodeNo)
        Animal animalName -> 
            if animalName == animalNode then 
                Question newQuestion (Animal newAnimalNode) (Animal animalNode)
            else Animal animalName

changeScreen : Screen -> Model -> Model
changeScreen screen model = { model | currentScreen = screen }