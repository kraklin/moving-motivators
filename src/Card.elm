module Card exposing
    ( Card
    , CardType(..)
    , Deck
    , Influence(..)
    , deck
    , decode
    , encode
    )

import List.Extra as List


type Influence
    = Positive
    | Neutral
    | Negative


type alias Card =
    { id : String
    , title : String
    , bubble : String
    , titleStyle : String
    , influence : Influence
    }


type alias Deck =
    List Card


type CardType
    = Curiosity
    | Honor
    | Acceptance
    | Mastery
    | Power
    | Freedom
    | Relatedness
    | Order
    | Goal
    | Status


deck : Deck
deck =
    [ Curiosity
    , Honor
    , Acceptance
    , Mastery
    , Power
    , Freedom
    , Relatedness
    , Order
    , Goal
    , Status
    ]
        |> List.map fromType


fromType : CardType -> Card
fromType cardType =
    fromTypeAndInfluence cardType Neutral


fromTypeAndInfluence : CardType -> Influence -> Card
fromTypeAndInfluence cardType influence =
    case cardType of
        Curiosity ->
            { id = "C"
            , title = "Curiosity"
            , bubble = "I have plenty of things to investigate and to think about"
            , titleStyle = "bg-orange-500"
            , influence = influence
            }

        Honor ->
            { id = "H"
            , title = "Honor"
            , bubble = "I feel proud that my personal values are reflected in how I work"
            , titleStyle = "bg-blue-400"
            , influence = influence
            }

        Acceptance ->
            { id = "A"
            , title = "Acceptance"
            , bubble = "The people around me approve of what I do and who I am"
            , titleStyle = "bg-yellow-500"
            , influence = influence
            }

        Freedom ->
            { id = "F"
            , title = "Freedom"
            , bubble = "I am independent of others with my own work and responsibilities"
            , titleStyle = "bg-red-600"
            , influence = influence
            }

        Status ->
            { id = "S"
            , title = "Status"
            , bubble = "My position is good, and recognized by the people who work with me"
            , titleStyle = "bg-pink-500"
            , influence = influence
            }

        Goal ->
            { id = "G"
            , title = "Goal"
            , bubble = "My purpose in life is reflected in the work that I do"
            , titleStyle = "bg-purple-700"
            , influence = influence
            }

        Order ->
            { id = "O"
            , title = "Order"
            , bubble = "There are enough rules and policies for a stable environment"
            , titleStyle = "bg-red-300"
            , influence = influence
            }

        Mastery ->
            { id = "M"
            , title = "Mastery"
            , bubble = "My work challenges my competence but it is still within my abilities"
            , titleStyle = "bg-teal-400"
            , influence = influence
            }

        Power ->
            { id = "P"
            , title = "Power"
            , bubble = "There’s enough room for me to influence what happens around me"
            , titleStyle = "bg-yellow-700"
            , influence = influence
            }

        Relatedness ->
            { id = "R"
            , title = "Relatedness"
            , bubble = "I have good social contacts with the people in and around my work"
            , titleStyle = "bg-green-700"
            , influence = influence
            }


decodeCardType : String -> Maybe CardType
decodeCardType id =
    case id of
        "C" ->
            Just Curiosity

        "H" ->
            Just Honor

        "A" ->
            Just Acceptance

        "M" ->
            Just Mastery

        "P" ->
            Just Power

        "F" ->
            Just Freedom

        "R" ->
            Just Relatedness

        "O" ->
            Just Order

        "G" ->
            Just Goal

        "S" ->
            Just Status

        _ ->
            Nothing


decodeInfluence : String -> Maybe Influence
decodeInfluence influence =
    case influence of
        "-" ->
            Just Positive

        "_" ->
            Just Negative

        "." ->
            Just Neutral

        _ ->
            Nothing


encode : Deck -> String
encode list =
    let
        influenceString influence =
            case influence of
                Positive ->
                    "-"

                Negative ->
                    "_"

                Neutral ->
                    "."
    in
    list
        |> List.foldl (\item acc -> acc ++ [ item.id, influenceString item.influence ]) []
        |> String.join ""


decode : String -> Deck
decode toDecode =
    let
        groupToCard group =
            case group of
                [ cardId, influenceString ] ->
                    Maybe.map2 fromTypeAndInfluence
                        (decodeCardType cardId)
                        (decodeInfluence influenceString)

                _ ->
                    Nothing
    in
    toDecode
        |> String.toList
        |> List.map String.fromChar
        |> List.groupsOf 2
        |> List.filterMap groupToCard
