module Main exposing (Model, Msg, initialModel, main, subscriptions, update, view)

import Browser
import DnDList
import Html
import Html.Attributes
import Html.Events
import Html.Events.Extra as Events
import Html.Extra as Html



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- DATA


type Influence
    = Positive
    | Neutral
    | Negative


type alias Item =
    { id : String
    , title : String
    , bubble : String
    , titleStyle : String
    , influence : Influence
    }


data : List Item
data =
    let
        fromInt int =
            case int of
                1 ->
                    { id = String.fromInt int
                    , title = "Curiosity"
                    , bubble = "I have plenty of things to investigate and to think about"
                    , titleStyle = "bg-orange-500"
                    , influence = Neutral
                    }

                2 ->
                    { id = String.fromInt int
                    , title = "Honor"
                    , bubble = "I feel proud that my personal values are reflected in how I work"
                    , titleStyle = "bg-blue-400"
                    , influence = Neutral
                    }

                3 ->
                    { id = String.fromInt int
                    , title = "Acceptance"
                    , bubble = "The people around me approve of what I do and who I am"
                    , titleStyle = "bg-yellow-500"
                    , influence = Neutral
                    }

                4 ->
                    { id = String.fromInt int
                    , title = "Freedom"
                    , bubble = "I am independent of others with my own work and responsibilities"
                    , titleStyle = "bg-red-600"
                    , influence = Negative
                    }

                5 ->
                    { id = String.fromInt int
                    , title = "Status"
                    , bubble = "My position is good, and recognized by the people who work with me"
                    , titleStyle = "bg-pink-500"
                    , influence = Neutral
                    }

                6 ->
                    { id = String.fromInt int
                    , title = "Goal"
                    , bubble = "My position is good, and recognized by the people who work with me"
                    , titleStyle = "bg-purple-700"
                    , influence = Positive
                    }

                7 ->
                    { id = String.fromInt int
                    , title = "Order"
                    , bubble = "There are enough rules and policies for a stable environment"
                    , titleStyle = "bg-red-300"
                    , influence = Neutral
                    }

                8 ->
                    { id = String.fromInt int
                    , title = "Mastery"
                    , bubble = "My work challenges my competence but it is still within my abilities"
                    , titleStyle = "bg-teal-400"
                    , influence = Neutral
                    }

                9 ->
                    { id = String.fromInt int
                    , title = "Power"
                    , bubble = "There’s enough room for me to influence what happens around me"
                    , titleStyle = "bg-yellow-700"
                    , influence = Neutral
                    }

                0 ->
                    { id = String.fromInt int
                    , title = "Relatedness"
                    , bubble = "I have good social contacts with the people in and around my work"
                    , titleStyle = "bg-green-700"
                    , influence = Neutral
                    }

                _ ->
                    { id = String.fromInt int
                    , title = "EMPTY CARD"
                    , bubble = "This is just empty card"
                    , titleStyle = "bg-red-300"
                    , influence = Neutral
                    }
    in
    List.range 0 9
        |> List.map fromInt



-- SYSTEM


config : DnDList.Config Item
config =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Free
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }


system : DnDList.System Item Msg
system =
    DnDList.create config MyMsg


type Mode
    = Ordering
    | Influencing



-- MODEL


type alias Model =
    { dnd : DnDList.Model
    , items : List Item
    , mode : Mode
    }


initialModel : Model
initialModel =
    { dnd = system.model
    , items = data
    , mode = Influencing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    system.subscriptions model.dnd



-- UPDATE


type Msg
    = MyMsg DnDList.Msg
    | SetInfluence String Influence
    | ToggleMode


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        MyMsg msg ->
            let
                ( dnd, items ) =
                    system.update msg model.dnd model.items
            in
            if model.mode == Ordering then
                ( { model | dnd = dnd, items = items }
                , system.commands dnd
                )

            else
                ( model, Cmd.none )

        SetInfluence id influence ->
            ( { model
                | items =
                    List.map
                        (\item ->
                            if item.id == id then
                                { item | influence = influence }

                            else
                                item
                        )
                        model.items
              }
            , Cmd.none
            )

        ToggleMode ->
            if model.mode == Ordering then
                ( { model | mode = Influencing }, Cmd.none )

            else
                ( { model | mode = Ordering }, Cmd.none )



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div
        [ Html.Attributes.class "flex flex-col justify-center h-screen"
        ]
        [ Html.section
            []
            [ model.items
                |> List.indexedMap (itemView model.mode model.dnd)
                |> Html.div containerStyles
            , ghostView model.mode model.dnd model.items
            ]
        , Html.button [ Html.Events.onClick ToggleMode ] [ Html.text "Switch mode" ]
        ]


itemView : Mode -> DnDList.Model -> Int -> Item -> Html.Html Msg
itemView mode dnd index item =
    let
        itemId : String
        itemId =
            "frdrag-" ++ item.id

        attrs : List (Html.Attribute msg)
        attrs =
            [ Html.Attributes.class "m-4 hover:cursor-pointer"
            , Html.Attributes.id itemId
            , influenceStyle item.influence
            ]
    in
    case system.info dnd of
        Just { dragIndex } ->
            if dragIndex /= index then
                Html.div
                    (attrs ++ system.dropEvents index itemId)
                    [ cardView mode item ]

            else
                Html.div
                    [ Html.Attributes.class "m-4 w-[220px] h-[200px] scale-110 rounded-md z-0 border-dashed border-2 border-gray-300 "
                    ]
                    []

        Nothing ->
            Html.div
                (attrs ++ system.dragEvents index itemId)
                [ cardView mode item ]


influenceStyle influence =
    Html.Attributes.class <|
        case influence of
            Positive ->
                "-translate-y-8"

            Negative ->
                "translate-y-8"

            Neutral ->
                ""


cardView mode card =
    Html.div
        [ Html.Attributes.class "flex flex-col w-[220px] h-[200px] shadow-md rounded-md bg-white"
        , Html.Attributes.class "hover:shadow-2xl hover:z-10"
        , Html.Attributes.class "transition ease-in-out"
        ]
        [ Html.div
            [ Html.Attributes.class card.titleStyle
            , Html.Attributes.class "px-4 py-2 uppercase text-center text-white font-bold"
            ]
            [ Html.text card.title ]
        , Html.div
            [ Html.Attributes.class "flex-grow px-4 py-2"
            ]
            [ Html.text card.bubble ]
        , Html.viewIf (mode == Influencing) <|
            Html.div [ Html.Attributes.class "flex" ]
                [ Html.button [ Html.Attributes.class "px-2 py-1 flex-grow bg-green-200", Events.onClickPreventDefaultAndStopPropagation <| SetInfluence card.id Positive ] [ Html.text "👍" ]
                , Html.button [ Html.Attributes.class "px-2 py-1 flex-grow bg-gray-200", Events.onClickPreventDefaultAndStopPropagation <| SetInfluence card.id Neutral ] [ Html.text "😐" ]
                , Html.button [ Html.Attributes.class "px-2 py-1 flex-grow bg-red-200", Events.onClickPreventDefaultAndStopPropagation <| SetInfluence card.id Negative ] [ Html.text "👎" ]
                ]
        ]


ghostView : Mode -> DnDList.Model -> List Item -> Html.Html Msg
ghostView mode dnd items =
    let
        maybeDragItem : Maybe Item
        maybeDragItem =
            system.info dnd
                |> Maybe.andThen (\{ dragIndex } -> items |> List.drop dragIndex |> List.head)
    in
    case maybeDragItem of
        Just item ->
            Html.div
                (Html.Attributes.class "m-4"
                    :: influenceStyle item.influence
                    :: system.ghostStyles dnd
                )
                [ cardView mode item ]

        Nothing ->
            Html.text ""



-- STYLES


containerStyles : List (Html.Attribute msg)
containerStyles =
    [ Html.Attributes.class "flex flex-wrap gap-y-12" ]


