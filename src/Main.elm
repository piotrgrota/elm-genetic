module Main exposing (..)

import Array
import Html
import Html.Attributes exposing (style)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (fill, height, id, in_, result, rx, stdDeviation, viewBox, width, x, y)
import Time exposing (Time)
import Vector exposing (..)
import ViewUtils exposing (..)




popSize : Int
popSize =
    35


newBall x y =
    { position = Vector x y
    , acceleration = Vector 0 0
    , velocity = Vector 0 0
    , completed = False
    , crashed = False
    , fitness = 0
    , genes = []
    }


newBallWithGenes x y genes =
    { position = Vector x y
    , acceleration = Vector 0 0
    , velocity = Vector 0 0
    , completed = False
    , crashed = False
    , fitness = 0
    , genes = genes
    }


type alias Ball =
    { position : Vector
    , acceleration : Vector
    , velocity : Vector
    , completed : Bool
    , crashed : Bool
    , fitness : Float
    , genes : List Vector
    }


copyBall r =
    { position = Vector r.position.x r.position.y
    , acceleration = Vector r.acceleration.x r.acceleration.y
    , velocity = Vector r.velocity.x r.velocity.y
    , completed = r.completed
    , crashed = r.crashed
    , fitness = r.fitness
    , genes = r.genes
    }


type Msg
    = Tick Time
    | BallInit (List Ball)
    |EvolutionStart (List EvolutionStructure)


type alias Model =
    { popSize : Int
    , balls : List Ball
    , counter : Int
    , matingPool : List Ball
    }


tick : Sub Msg
tick =
    Time.every (20 * Time.millisecond) Tick


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ tick ]


init =
    { popSize = popSize
    , balls = [ newBall 50 95 ]
    , counter = 0
    , matingPool = []
    }


update  msg model =
    case msg of
        BallInit newBalls ->
            ( { model | balls = newBalls }, Cmd.none )
        EvolutionStart evolutionStructure ->
            ( evolution model evolutionStructure ,Cmd.none)
        Tick time ->
            let
                f =
                    List.map (\r -> updateBall r r.genes (model.counter + 1)) model.balls
            in
            if model.counter < 400 then
                ( { model | balls = f, counter = model.counter + 1 }, Cmd.none )

            else
                ( model, Cmd.none ) 
                    |> evaluate
                    |> selection


selection ( model, msg )  =
    (  model , Random.generate EvolutionStart (newPopulationGenerator model.popSize model.matingPool) )


type alias Rect = {
    pos : Vector
    ,width: Float
    ,height: Float
}

obstacle : Rect
obstacle = {
    pos = Vector 20 60
    ,width = 60
    ,height = 3
    }


target : Vector
target =
    Vector 50 6


renderTarget =
    [ circle [ Svg.Attributes.cx "50", Svg.Attributes.cy "6", Svg.Attributes.r "3", fill "red" ] [] ]


renderBlock block =
    let
        ( strX, strY ) =
            ( toString block.position.x, toString block.position.y )
    in
    circle [ Svg.Attributes.cx strX, Svg.Attributes.cy strY, Svg.Attributes.r "1.5", fill "white" ] []


renderBalls model =
    List.map (\r -> renderBlock r) model.balls


viewSvg model =
    let
        parentStyle =
            Html.Attributes.style [ ( "margin", "0 auto" ), ( "display", "block" ) ]
    in
    svg
        [ width "800", height "600", viewBox "0 0 100 100", parentStyle ]
        ([ renderBackground ]
            ++ renderObstacle
            ++ renderTarget
            ++ renderBalls model
        )




renderObstacle =
    [rect [ x (toString obstacle.pos.x), y (toString obstacle.pos.y), width (toString obstacle.width), height (toString obstacle.height), fill "blue" ] []]

widthSize =
    100


heightSize =
    100


view model =
    Html.div []
        [ viewSvg model
        , Html.p [] [ text ("Life Span: " ++ toString model.counter) ]
        ]


itemFromArray : Array.Array Vector -> Int -> Vector
itemFromArray array index =
    case Array.get index array of
        Just a ->
            a

        Nothing ->
            Vector 0 0


updateBall r genes counter =
    let
        tmp =
            itemFromArray (Array.fromList genes) counter

        acc =
            addTwoVectors r.acceleration tmp

        vel =
            addTwoVectors r.velocity acc

        pos =
            addTwoVectors r.position vel

        acc2 =
            multiVectorByScalar acc 0

        newvel =
            limit vel 4

        dist =
            distance r.position target

        ( completed, tmpPos ) =
            if dist < 6 then
                ( True, Vector target.x target.y )
            else
                ( False, Vector r.position.x r.position.y )


        checkCrashed =
            if (r.position.x + 2) > widthSize || (r.position.x - 2 ) < 0 then
                True
            else if (r.position.y + 2) > heightSize || (r.position.y - 2) < 0 then
                True
            else if r.position.x > obstacle.pos.x &&  r.position.x < obstacle.pos.x + obstacle.width
                    && r.position.y > obstacle.pos.y && r.position.y < obstacle.pos.y + obstacle.height then
                True 
            else
                False
    in
    if not checkCrashed && not completed then
        { r | position = pos, acceleration = acc2, velocity = newvel }
    else
        { r | completed = completed, crashed = checkCrashed }


type alias EvolutionStructure =
    { genes : List ( Float, Vector )
    , parentsIndex : ( Int, Int )
    , randomCrossing : Int
    }






mutateChild : List Vector -> List ( Float, Vector ) -> List Vector
mutateChild genes mutatedData =
    List.map2
        (\r ( chance, cord ) ->
            if chance < 0.01 then
                cord
            else
                r
        )
        genes
        mutatedData

crossoverDnas : List Vector -> List Vector -> Int -> List Vector
crossoverDnas dna1 dna2 splitIndex =
    let
        ( dnaPart1, dnaPart2 ) =
            ( List.take splitIndex  dna1, List.drop splitIndex dna2 )
    in
        List.append dnaPart1 dnaPart2

ballSelection : Ball -> EvolutionStructure -> Array.Array Ball -> Ball
ballSelection ball e matingPool =
    let
        ( parentAIndex, parentBIndex ) = e.parentsIndex
        mutationGenes = e.genes
        parentA = Array.get parentAIndex matingPool
        parentB =
            Array.get parentBIndex matingPool

        parentABall =
            Maybe.withDefault ball parentA

        parentBBall =
            Maybe.withDefault ball parentB

        crossing = crossoverDnas parentABall.genes parentBBall.genes e.randomCrossing

        newGenes = mutateChild crossing e.genes
    in
    { ball | completed = False, crashed = False, fitness = 0,position = Vector 50 95 ,acceleration = Vector 0 0, velocity = Vector 0 0 ,genes = newGenes }


evolution : Model -> List EvolutionStructure -> Model
evolution model evolution =
    let
        newBalls =
            List.map2 (\r e -> ballSelection r e (Array.fromList model.matingPool)) model.balls evolution
    in
    { model | balls = newBalls ,matingPool = [] ,counter = 0 }


createEvolutionStructure g p r =
    { genes = g
    , parentsIndex = p
    , randomCrossing = r
    }


newPopulationGenerator : Int -> List a -> Random.Generator (List EvolutionStructure)
newPopulationGenerator popSize matingPool =
    let
        randomFloat =
            Random.float 0 1

        randomFloatChance =
            Random.float 0 1

        randomAngle =
            Random.map (\f -> f * pi * 2) randomFloat

        randomVector =
            Random.map (\a -> Vector (cos a) (sin a)) randomAngle

        normalizedVector =
            Random.map (\v -> (multiVectorByScalar << normalize) v 0.2) randomVector

        randomIndexParentA =
            Random.int 0 (List.length matingPool - 1)

        randomIndexParentB =
            Random.int 0 (List.length matingPool - 1)

        parentsPair =
            Random.pair randomIndexParentA randomIndexParentB

        crossOverIndex =
            Random.int 0 399

        randomDna =
            Random.list 400 <| Random.pair randomFloatChance normalizedVector
    in
    Random.list popSize
        (Random.map3 (\parentPairIndexes indexForMutation dna -> createEvolutionStructure dna parentPairIndexes indexForMutation)
            parentsPair
            crossOverIndex
            randomDna
        )


makeBallsGenerator : Random.Generator (List Ball)
makeBallsGenerator =
    let
        randomFloat =
            Random.float 0 1

        randomAngle =
            Random.map (\f -> f * pi * 2) randomFloat

        randomVector =
            Random.map (\a -> Vector (cos a) (sin a)) randomAngle

        normalizedVector =
            Random.map (\v -> (multiVectorByScalar << normalize) v 0.2) randomVector

        randomDna =
            Random.list 400 normalizedVector
    in
    Random.list popSize (Random.map (\pos -> newBallWithGenes 50 95 pos) randomDna)


main =
    Html.program
        { init = ( init, Random.generate BallInit makeBallsGenerator )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


evaluateBall : Ball -> List Ball
evaluateBall ball =
    List.repeat (round ball.fitness * 100) (copyBall ball)


evaluate : (Model,Cmd Msg) -> ( Model, Cmd Msg )
evaluate (model , msg) =
    let
        ballWithFitness = List.map (\f -> { f | fitness = calculateFitness f.position target ( f.crashed, f.completed ) widthSize }) <| model.balls

        fitness = List.map (\r -> r.fitness) ballWithFitness

        sorted = List.sort fitness

        maxFit =
            case List.foldl (::) [] sorted |> List.head of
                Just n ->
                    n

                Nothing ->
                    200

        normalizedBall =
            List.map (\r -> { r | fitness = r.fitness / maxFit }) ballWithFitness

        matingpool =
            List.concat (List.map (\r -> evaluateBall r) normalizedBall)
    in
    ({ model | balls = normalizedBall, matingPool = matingpool, counter = 400 }, msg)



