module Main exposing (..)

import Html exposing (Html)
import Html.App as App
import Html.Events
import List exposing (isEmpty)
import Html.Attributes exposing (placeholder)
import String exposing (contains)
import Result


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Matches =
    List Article


type alias Model =
    { query : Query
    , matches : Matches
    , hasSearched : Bool
    , allArticlesHasLoaded : Bool
    }


initialModel : Model
initialModel =
    { query = justSearchQuery ""
    , matches = []
    , hasSearched = False
    , allArticlesHasLoaded = False
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


type alias Apk =
    Float


type alias VolumeInMl =
    Int


type alias AlcoholContent =
    Float


type alias Price =
    Float


type alias Name =
    String


type alias Type_ =
    String


type alias Article =
    { apk : Apk
    , volumeInMl : VolumeInMl
    , alcoholContent : AlcoholContent
    , price : Price
    , name : String
    , type_ : Type_
    }


exampleArticle : Article
exampleArticle =
    { apk = 2.34
    , volumeInMl = 500
    , alcoholContent = 5.0
    , price = 23.4
    , name = "pripps blå"
    , type_ = "varugrupp"
    }


loadArticles : List Article
loadArticles =
    [ exampleArticle ]


articles : List Article
articles =
    loadArticles


type alias Query =
    { searchString : String
    , minApk : Maybe Apk
    , maxApk : Maybe Apk
    , minVolumeInMl : Maybe VolumeInMl
    , maxVolumeInMl : Maybe VolumeInMl
    , minAlcoholContent : Maybe AlcoholContent
    , maxAlcoholContent : Maybe AlcoholContent
    , minPrice : Maybe Price
    , maxPrice : Maybe Price
    }


justSearchQuery : String -> Query
justSearchQuery s =
    { searchString = s
    , minApk = Nothing
    , maxApk = Nothing
    , minVolumeInMl = Nothing
    , maxVolumeInMl = Nothing
    , minAlcoholContent = Nothing
    , maxAlcoholContent = Nothing
    , minPrice = Nothing
    , maxPrice = Nothing
    }


match : Query -> Article -> Bool
match query article =
    let
        stringMatches =
            query.searchString `contains` article.name
    in
        stringMatches



-- Returns list of articles matching searchString


lookup : List Article -> Query -> Matches
lookup articles query =
    articles
        |> List.filter (match query)



-- UPDATE


type Msg
    = NoOp
    | UpdateSearchString String
    | PerformLookup
    | UpdateMinApk String
    | UpdateMaxApk String
    | UpdateMinVolumeInMl String
    | UpdateMaxVolumeInMl String
    | UpdateMinAlcoholContent String
    | UpdateMaxAlcoholContent String
    | UpdateMinPrice String
    | UpdateMaxPrice String


floatOrNothing : String -> Maybe Float
floatOrNothing =
    String.toFloat >> Result.toMaybe


intOrNothing : String -> Maybe Int
intOrNothing =
    String.toInt >> Result.toMaybe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        query =
            model.query

        newModel =
            case msg of
                NoOp ->
                    model

                PerformLookup ->
                    { model
                        | matches = lookup articles model.query
                        , hasSearched = True
                    }

                UpdateSearchString newSearchString ->
                    let
                        updateSearchString =
                            { query | searchString = newSearchString }
                    in
                        { model | query = updateSearchString }

                UpdateMinApk minApkString ->
                    let
                        updateMinApk =
                            { query | minApk = floatOrNothing minApkString }
                    in
                        { model | query = updateMinApk }

                UpdateMaxApk maxApkString ->
                    let
                        updateMaxApk =
                            { query | maxApk = floatOrNothing maxApkString }
                    in
                        { model | query = updateMaxApk }

                UpdateMinVolumeInMl minVolumeInMlString ->
                    let
                        updateMinVolumeInMl =
                            { query | minVolumeInMl = intOrNothing minVolumeInMlString }
                    in
                        { model | query = updateMinVolumeInMl }

                UpdateMaxVolumeInMl maxVolumeInMlString ->
                    let
                        updateMaxVolumeInMl =
                            { query | maxVolumeInMl = intOrNothing maxVolumeInMlString }
                    in
                        { model | query = updateMaxVolumeInMl }

                UpdateMinAlcoholContent minAlcoholContentString ->
                    let
                        updateMinAlcoholContent =
                            { query | minAlcoholContent = floatOrNothing minAlcoholContentString }
                    in
                        { model | query = updateMinAlcoholContent }

                UpdateMaxAlcoholContent maxAlcoholContentString ->
                    let
                        updateMaxAlcoholContent =
                            { query | maxAlcoholContent = floatOrNothing maxAlcoholContentString }
                    in
                        { model | query = updateMaxAlcoholContent }

                UpdateMinPrice minPriceString ->
                    let
                        updateMinPrice =
                            { query | minPrice = floatOrNothing minPriceString }
                    in
                        { model | query = updateMinPrice }

                UpdateMaxPrice maxPriceString ->
                    let
                        updateMaxPrice =
                            { query | maxPrice = floatOrNothing maxPriceString }
                    in
                        { model | query = updateMaxPrice }

        -- { model | maxApk = Result.toMaybe (String.toFloat maxApkString) }
    in
        ( newModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


showArticle : Article -> Html Msg
showArticle article =
    Html.text article.name


showResults : Model -> List (Html Msg)
showResults model =
    if model.hasSearched then
        if isEmpty model.matches then
            [ Html.text ("No matches when searching for '" ++ model.query.searchString ++ "'!")
            ]
        else
            model.matches |> List.map showArticle
    else
        [ Html.text "Click the button to search!" ]



-- getQueryInput : String -> Msg -> Html Msg


getQueryInput : String -> String -> (String -> Msg) -> List (Html Msg)
getQueryInput labelText placeholderText onInputMsg =
    [ Html.text labelText
    , Html.input [ placeholder placeholderText, Html.Events.onInput onInputMsg ] []
    ]


getQueryFromToInput : String -> List ( String, String -> Msg ) -> List (Html Msg)
getQueryFromToInput text placeholderAndMsgList =
    let
        inputBox ( placeText, msg ) =
            Html.input [ placeholder placeText, Html.Events.onInput msg ] []

        inputBoxes =
            placeholderAndMsgList
                |> List.map inputBox
                |> List.intersperse (Html.text "-")
    in
        [ Html.text text ] ++ inputBoxes ++ [ Html.br [] [] ]


inputs : Html Msg
inputs =
    Html.div []
        ([ Html.text "Search term: "
         , Html.input [ placeholder "pripps", Html.Events.onInput UpdateSearchString ] []
         , Html.button [ Html.Events.onClick PerformLookup ] [ Html.text "Button text" ]
         , Html.br [] []
         ]
            ++ (getQueryFromToInput "APK:" [ ( "0.8", UpdateMinApk ), ( "1.8", UpdateMaxApk ) ])
            ++ (getQueryFromToInput "Vol:" [ ( "50", UpdateMinVolumeInMl ), ( "75", UpdateMaxVolumeInMl ) ])
            ++ (getQueryFromToInput "Alc%:" [ ( "4.0", UpdateMinVolumeInMl ), ( "75", UpdateMaxVolumeInMl ) ])
            ++ (getQueryFromToInput "Price:" [ ( "14", UpdateMinPrice ), ( "29", UpdateMaxPrice ) ])
        )



-- )
-- ++ (getQueryInput "Max APK:" "9.5" UpdateMaxApk)
-- ++ htmlBr
-- ++ (getQueryInput "Min vol:" "50" UpdateMinVolumeInMl)
-- ++ (getQueryInput "Max vol:" "75" UpdateMaxVolumeInMl)
-- ++ htmlBr
-- ++ (getQueryInput "" "" UpdateMinAlcoholContent)
-- ++ (getQueryInput "" "" UpdateMaxAlcoholContent)
-- ++ htmlBr
-- ++ (getQueryInput "" "" UpdateMinPrice)
-- ++ (getQueryInput "" "" UpdateMaxPrice)
-- minApk = Nothing
-- maxApk = Nothing
-- minVolumeInMl = Nothing
-- maxVolumeInMl = Nothing
-- minAlcoholContent = Nothing
-- maxAlcoholContent = Nothing
-- minPrice = Nothing
-- maxPrice = Nothing
-- , Html.input [ placeholder "0.0", Html.Events.onInput UpdateSearchString ] []
-- , Html.button [ Html.Events.onClick PerformLookup ] [ Html.text "Button text" ]
-- ]


statusView : Model -> Html Msg
statusView model =
    Html.div [ Html.Attributes.style [ ( "color", "red" ) ] ]
        [ Html.text "Not all articles have loaded!" ]


view : Model -> Html Msg
view model =
    let
        style =
            Html.Attributes.style
    in
        Html.div [ style [ ( "transform", "translate(1%, 10%)" ) ] ]
            [ statusView model
            , inputs
            , Html.div [ style [] ] (showResults model)
            ]
