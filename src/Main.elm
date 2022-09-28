module Main exposing (Model, Msg, initialModel, main, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Card exposing (Card, Influence(..))
import DnDList
import Html
import Html.Attributes as Attrs
import Html.Events as Events
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



-- SYSTEM


config : DnDList.Config Card
config =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Free
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }


system : DnDList.System Card Msg
system =
    DnDList.create config MyMsg


type Mode
    = Ordering
    | Influencing
    | Result



-- MODEL


type alias Model =
    { dnd : DnDList.Model
    , items : List Card
    , mode : Mode
    , url : Url
    , key : Nav.Key
    }


initialModel : Url -> Nav.Key -> Model
initialModel url key =
    { dnd = system.model
    , items = Card.deck
    , mode = Ordering
    , url = url
    , key = key
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        initialModel_ =
            initialModel url key

        result =
            Url.toString url
                |> String.split "#"
                |> List.reverse
                |> List.head
                |> Maybe.map Card.decode
                |> Maybe.andThen
                    (\initialDeck ->
                        if List.length initialDeck /= 10 then
                            Nothing

                        else
                            Just { initialModel_ | items = initialDeck, mode = Result }
                    )
                |> Maybe.withDefault initialModel_
    in
    ( result, Cmd.none )


deckUrl url deck =
    (Url.toString <| { url | fragment = Nothing }) ++ "#" ++ Card.encode deck



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    system.subscriptions model.dnd



-- UPDATE


type Msg
    = MyMsg DnDList.Msg
    | NoOp
    | SetInfluence String Influence
    | ToggleMode
    | SetMode Mode
    | UrlChanged Url
    | UrlRequest Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        UrlChanged url ->
            ( model, Cmd.none )

        UrlRequest request ->
            case request of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

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
            let
                cmd =
                    Nav.replaceUrl model.key (deckUrl model.url model.items)
            in
            ( { model | mode = mode }, cmd )

        ToggleMode ->
            if model.mode == Ordering then
                ( { model | mode = Influencing }, Cmd.none )

            else
                ( { model | mode = Ordering }, Cmd.none )



-- VIEW


stepView mode deck url =
    case mode of
        Ordering ->
            Html.div [ Attrs.class "mt-6 text-md" ]
                [ Html.h2 [ Attrs.class "text-[2rem]" ] [ Html.text "Step One" ]
                , Html.p [ Attrs.class "mt-4" ]
                    [ Html.text "Define which motivators are important to you." ]
                , Html.p [ Attrs.class "mt-4" ]
                    [ Html.text "Place the cards in order from 1 (least important) to 10 (most important)" ]
                , Html.p [ Attrs.class "mt-4" ]
                    [ Html.text "When you are done with ordering the cards, you can continue with Step 2." ]
                , Html.div [ Attrs.class "flex justify-end mt-8" ]
                    [ buttonView (SetMode Influencing) "Continue with Step 2"
                    ]
                ]

        Influencing ->
            Html.div [ Attrs.class "mt-6 text-md" ]
                [ Html.h2 [ Attrs.class "text-[2rem]" ] [ Html.text "Step Two" ]
                , Html.p [ Attrs.class "mt-4" ]
                    [ Html.text "Discuss how change affects your motivators." ]
                , Html.p [ Attrs.class "mt-4" ]
                    [ Html.text "For example: If you’re wondering if you should change jobs, which would mean moving to another city, learning a new skill and making all new friends, how does that affect what motivates you? It’ll most likely increase some motivators and decrease others."
                    ]
                , Html.p [ Attrs.class "mt-4" ]
                    [ Html.text "Move the cards up with 👍 for a positive change and down with 👎 for a negative one. Move the cards to the neutral position with \u{1F7F0} " ]
                , Html.p [ Attrs.class "mt-4" ]
                    [ Html.text "Then look at whether you have more cards up or down. This is a great way to help make decisions." ]
                , Html.div [ Attrs.class "flex justify-between items-center mt-8" ]
                    [ secondaryButtonView (SetMode Ordering) "Go back to reorder cards"
                    , buttonView (SetMode Result) "Continue with Step 3"
                    ]
                ]

        Result ->
            Html.div [ Attrs.class "mt-6 text-md" ]
                [ Html.h2 [ Attrs.class "text-[2rem]" ] [ Html.text "Step Three" ]
                , Html.p [ Attrs.class "mt-4" ]
                    [ Html.text "Time for reflection and discussion." ]
                , Html.p [ Attrs.class "mt-4" ]
                    [ Html.text "Talk to your teammates about which motivators are least and most important to them. This will give you better insight into what drives your colleagues and allow you to create stronger relationships and increase collaboration. Use it also as a tool to reflect and assess your own life decisions. When most of your important motivators go down or when the least important ones go up it might be time for reflection."
                    ]
                , Html.p [ Attrs.class "mt-8 font-semibold text-center" ]
                    [ Html.text "You can share your results (or access them later at the URL below)" ]
                , Html.p [ Attrs.class "mt-4 text-center px-4 py-2 bg-gray-200 text-gray-700 font-mono" ]
                    [ Html.text <| deckUrl url deck
                    ]
                , Html.div [ Attrs.class "flex justify-between items-center mt-8" ]
                    [ secondaryButtonView (SetMode Ordering) "Go back to reorder your cards (Step 1)"
                    , secondaryButtonView (SetMode Influencing) "Go back to move them up or down (Step 2)"
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
            , Html.section [ Attrs.class "mx-auto px-6 max-w-[800px]" ] [ stepView model.mode model.items model.url ]
            , Html.section
                [ Attrs.class "px-8 my-16 mx-auto max-w-[1890px]" ]
                [ model.items
                    |> List.indexedMap (itemView model.mode model.dnd)
                    |> Html.div containerStyles
                , ghostView model.mode model.dnd model.items
                ]
            ]
        ]
    }


buttonView onClick caption =
    Html.button
        [ Attrs.class "block px-4 py-2 rounded bg-blue-500 text-white hover:bg-blue-400"
        , Events.onClick onClick
        ]
        [ Html.text caption ]


secondaryButtonView onClick caption =
    Html.button
        [ Attrs.class "block px-2 font-bold py-1 text-blue-500 hover:text-blue-400 hover:underline"
        , Events.onClick onClick
        ]
        [ Html.text caption ]


itemView : Mode -> DnDList.Model -> Int -> Card -> Html.Html Msg
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
                        [ Attrs.class "m-4 w-[150px] h-[250px] scale-110 rounded-md z-0 border-dashed border-2 border-gray-300 "
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
        [ Attrs.class "flex flex-col w-[150px] h-[250px] shadow-md rounded-md bg-white"
        , Attrs.class "hover:shadow-2xl hover:z-10"
        , Attrs.class "transition ease-in-out"
        ]
        [ Html.div
            [ Attrs.class card.titleStyle
            , Attrs.class "px-4 py-2 uppercase text-center text-white font-bold rounded-t-md"
            ]
            [ Html.text card.title ]
        , Html.map (always NoOp) card.icon
        , Html.div
            [ Attrs.class "flex flex-grow items-center p-2 text-xs text-gray-700"
            ]
            [ Html.p
                [ Attrs.class card.bubbleStyle
                , Attrs.class "p-1 rounded border"
                ]
                [ Html.text card.bubble ]
            ]
        , Html.viewIf (mode == Influencing) <|
            Html.div [ Attrs.class "flex" ]
                [ Html.button [ Attrs.class "px-2 py-1 flex-grow bg-green-100", Events.onClickPreventDefaultAndStopPropagation <| SetInfluence card.id Positive ] [ Html.text "👍" ]
                , Html.button [ Attrs.class "px-2 py-1 flex-grow bg-gray-100", Events.onClickPreventDefaultAndStopPropagation <| SetInfluence card.id Neutral ] [ Html.text "\u{1F7F0}" ]
                , Html.button [ Attrs.class "px-2 py-1 flex-grow bg-red-100", Events.onClickPreventDefaultAndStopPropagation <| SetInfluence card.id Negative ] [ Html.text "👎" ]
                ]
        ]


ghostView : Mode -> DnDList.Model -> List Card -> Html.Html Msg
ghostView mode dnd items =
    let
        maybeDragItem : Maybe Card
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
