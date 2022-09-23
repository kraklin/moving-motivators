module Main exposing (Model, Msg, initialModel, main, subscriptions, update, view)

import Browser
import Browser.Navigation
import DnDList
import Html
import Html.Attributes as Attrs
import Html.Events
import Html.Events.Extra as Events
import Html.Extra as Html
import Url exposing (Url)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequest
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
                    , influence = Neutral
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
                    , bubble = "My purpose in life is reflected in the work that I do"
                    , titleStyle = "bg-purple-700"
                    , influence = Neutral
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
    | Result



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
    , mode = Ordering
    }


init : () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
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
    | SetMode Mode
    | UrlChanged Url
    | UrlRequest Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        UrlChanged url ->
            ( model, Cmd.none )

        UrlRequest request ->
            ( model, Cmd.none )

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

        SetMode mode -> 
                ( { model | mode = mode }, Cmd.none )
        ToggleMode ->
            if model.mode == Ordering then
                ( { model | mode = Influencing }, Cmd.none )

            else
                ( { model | mode = Ordering }, Cmd.none )



-- VIEW


stepView mode =
    case mode of
        Ordering ->
            Html.div [ Attrs.class "mt-6 text-md" ]
                [ Html.h2 [ Attrs.class "text-[2rem]" ] [ Html.text "Step One" ]
                , Html.p [Attrs.class "mt-4"]
                    [ Html.text "Define which motivators are important to you." ]
                , Html.p [Attrs.class "mt-4"]
                    [ Html.text "Place the cards in order from 1 (least important) to 10 (most important)" ]
                , Html.p [Attrs.class "mt-4"]
                    [ Html.text "When you are done with ordering the cards, you can continue with Step 2." ]
                , Html.button [Html.Events.onClick <| SetMode Influencing][Html.text "Continue with Step 2"]
                ]

        Influencing ->
            Html.div [Attrs.class "mt-6 text-md"]
                [ Html.h2 [ Attrs.class "text-[2rem]" ] [ Html.text "Step Two" ]
                , Html.p [Attrs.class "mt-4"]
                    [ Html.text "Discuss how change affects your motivators."]
                , Html.p [Attrs.class "mt-4"]
                    [ Html.text "For example: If you’re wondering if you should change jobs, which would mean moving to another city, learning a new skill and making all new friends, how does that affect what motivates you? It’ll most likely increase some motivators and decrease others."
                    ]
                , Html.p [Attrs.class "mt-4"]
                    [ Html.text "Move the cards up with 👍 for a positive change and down with 👎 for a negative one. Move the cards to the neutral position with \u{1F7F0} " ]
                , Html.p [Attrs.class "mt-4"]
                    [ Html.text "Then look at whether you have more cards up or down. This is a great way to help make decisions." ]
                , Html.button [Html.Events.onClick <| SetMode Result][Html.text "Continue with Step 3"]
                ]

        Result ->
            Html.div []
                [ Html.h2 [ Attrs.class "" ]
                    [ Html.text "Step Three" ]
                , Html.p []
                    [ Html.text "Time for reflection and discussion. Talk to your teammates about which motivators are least and most important to them. This will give you better insight into what drives your colleagues and allow you to create stronger relationships and increase collaboration. Use it also as a tool to reflect and assess your own life decisions. When most of your important motivators go down or when the least important ones go up it might be time for reflection."
                    ]
                ]


view : Model -> Browser.Document Msg
view model =
    { title = "Moving motivators"
    , body =
        [ Html.div
            [ Attrs.class "flex flex-col h-screen"
            ]
            [ Html.h1 [ Attrs.class "px-4 py-2 bg-blue-500 text-white text-center text-2xl" ]
                [ Html.text "Moving motivators"
                ]
            , Html.section [ Attrs.class "mx-auto px-6 max-w-[800px]" ] [ stepView model.mode ]
            , Html.section
                [ Attrs.class "px-8 my-16" ]
                [ model.items
                    |> List.indexedMap (itemView model.mode model.dnd)
                    |> Html.div containerStyles
                , ghostView model.mode model.dnd model.items
                ]
            ]
        ]
    }


itemView : Mode -> DnDList.Model -> Int -> Item -> Html.Html Msg
itemView mode dnd index item =
    let
        itemId : String
        itemId =
            "frdrag-" ++ item.id

        attrs : List (Html.Attribute msg)
        attrs =
            [ Attrs.class "m-4 hover:cursor-pointer"
            , Attrs.class "transition ease-in-out"
            , Attrs.id itemId
            , influenceStyle item.influence
            ]

        numberView =
            let
                colour =
                    case item.influence of
                        Positive ->
                            Attrs.class "text-green-500"

                        Negative ->
                            Attrs.class "text-red-500"

                        Neutral ->
                            Attrs.class "text-gray-300"

                importance =
                    if index == 0 then
                        Html.text "least important"

                    else if index == 9 then
                        Html.text "most important"

                    else
                        Html.nothing
            in
            Html.div [ Attrs.class "relative overflow-hidden h-[3rem] mb-1" ]
                [ Html.div
                    [ Attrs.class "absolute -top-[1.2rem] flex justify-between items-center text-[4rem] font-bold w-[150px]"
                    , colour
                    ]
                    [ Html.text <| String.fromInt (index + 1), Html.span [ Attrs.class "text-xs text-gray-800 text-right ml-4" ] [ importance ] ]
                ]
    in
    case system.info dnd of
        Just { dragIndex } ->
            if dragIndex /= index then
                Html.div
                    (attrs ++ system.dropEvents index itemId)
                    [ numberView, cardView mode item ]

            else
                Html.div []
                    [ numberView
                    , Html.div
                        [ Attrs.class "m-4 w-[150px] h-[200px] scale-110 rounded-md z-0 border-dashed border-2 border-gray-300 "
                        ]
                        []
                    ]

        Nothing ->
            Html.div
                (attrs ++ system.dragEvents index itemId)
                [ numberView, cardView mode item ]


influenceStyle influence =
    Attrs.class <|
        case influence of
            Positive ->
                "-translate-y-12"

            Negative ->
                "translate-y-12"

            Neutral ->
                ""


cardView mode card =
    Html.div
        [ Attrs.class "flex flex-col w-[150px] h-[200px] shadow-md rounded-md bg-white"
        , Attrs.class "hover:shadow-2xl hover:z-10"
        , Attrs.class "transition ease-in-out"
        ]
        [ Html.div
            [ Attrs.class card.titleStyle
            , Attrs.class "px-4 py-2 uppercase text-center text-white font-bold"
            ]
            [ Html.text card.title ]
        , Html.div
            [ Attrs.class "flex flex-grow items-center px-4 py-2 text-xs text-gray-700"
            ]
            [ Html.p [] [ Html.text card.bubble ] ]
        , Html.viewIf (mode == Influencing) <|
            Html.div [ Attrs.class "flex" ]
                [ Html.button [ Attrs.class "px-2 py-1 flex-grow bg-green-100", Events.onClickPreventDefaultAndStopPropagation <| SetInfluence card.id Positive ] [ Html.text "👍" ]
                , Html.button [ Attrs.class "px-2 py-1 flex-grow bg-gray-100", Events.onClickPreventDefaultAndStopPropagation <| SetInfluence card.id Neutral ] [ Html.text "\u{1F7F0}" ]
                , Html.button [ Attrs.class "px-2 py-1 flex-grow bg-red-100", Events.onClickPreventDefaultAndStopPropagation <| SetInfluence card.id Negative ] [ Html.text "👎" ]
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
                (Attrs.class "m-4"
                    :: influenceStyle item.influence
                    :: system.ghostStyles dnd
                )
                [ cardView mode item ]

        Nothing ->
            Html.text ""



-- STYLES


containerStyles : List (Html.Attribute msg)
containerStyles =
    [ Attrs.class "flex flex-wrap gap-y-20" ]
