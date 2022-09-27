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
import Svg
import Svg.Attributes as SvgAttrs


type Influence
    = Positive
    | Neutral
    | Negative


type alias Card =
    { id : String
    , title : String
    , icon : Svg.Svg Never
    , bubble : String
    , titleStyle : String
    , bubbleStyle : String
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
            , icon = iconCuriosity
            , bubble = "I have plenty of things to investigate and to think about"
            , titleStyle = "bg-orange-500"
            , bubbleStyle = " text-orange-900 bg-orange-50 border-orange-800"
            , influence = influence
            }

        Honor ->
            { id = "H"
            , title = "Honor"
            , icon = iconHonor
            , bubble = "I feel proud that my personal values are reflected in how I work"
            , titleStyle = "bg-blue-400"
            , bubbleStyle = " text-blue-900 bg-blue-50 border-blue-800"
            , influence = influence
            }

        Acceptance ->
            { id = "A"
            , title = "Acceptance"
            , icon = iconAcceptance
            , bubble = "The people around me approve of what I do and who I am"
            , titleStyle = "bg-yellow-500"
            , bubbleStyle = " text-yellow-900 bg-yellow-50 border-yellow-800"
            , influence = influence
            }

        Freedom ->
            { id = "F"
            , title = "Freedom"
            , icon = iconFreedom
            , bubble = "I am independent of others with my own work and responsibilities"
            , titleStyle = "bg-red-600"
            , bubbleStyle = " text-red-900 bg-red-50 border-red-800"
            , influence = influence
            }

        Status ->
            { id = "S"
            , title = "Status"
            , icon = iconStatus
            , bubble = "My position is good, and recognized by the people who work with me"
            , titleStyle = "bg-pink-500"
            , bubbleStyle = " text-pink-900 bg-pink-50 border-pink-800"
            , influence = influence
            }

        Goal ->
            { id = "G"
            , title = "Goal"
            , icon = iconGoal
            , bubble = "My purpose in life is reflected in the work that I do"
            , titleStyle = "bg-purple-700"
            , bubbleStyle = " text-purple-900 bg-purple-50 border-purple-800"
            , influence = influence
            }

        Order ->
            { id = "O"
            , title = "Order"
            , icon = iconOrder
            , bubble = "There are enough rules and policies for a stable environment"
            , titleStyle = "bg-rose-300"
            , bubbleStyle = " text-rose-900 bg-rose-50 border-rose-800"
            , influence = influence
            }

        Mastery ->
            { id = "M"
            , title = "Mastery"
            , icon = iconMastery
            , bubble = "My work challenges my competence but it is still within my abilities"
            , titleStyle = "bg-teal-400"
            , bubbleStyle = " text-teal-900 bg-teal-50 border-teal-800"
            , influence = influence
            }

        Power ->
            { id = "P"
            , title = "Power"
            , icon = iconPower
            , bubble = "There’s enough room for me to influence what happens around me"
            , titleStyle = "bg-amber-700"
            , bubbleStyle = " text-amber-900 bg-amber-50 border-amber-800"
            , influence = influence
            }

        Relatedness ->
            { id = "R"
            , title = "Relatedness"
            , icon = iconRelatedness
            , bubble = "I have good social contacts with the people in and around my work"
            , titleStyle = "bg-green-700"
            , bubbleStyle = " text-green-900 bg-green-50 border-green-800"
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



{- Icons -}


iconAcceptance =
    Svg.svg
        [ SvgAttrs.viewBox "0 0 580 460"
        ]
        [ Svg.g
            [ SvgAttrs.fill "none"
            , SvgAttrs.fillRule "evenodd"
            , SvgAttrs.stroke "#000"
            , SvgAttrs.strokeWidth "8"
            ]
            [ Svg.g
                [ SvgAttrs.transform "translate(86 321)"
                ]
                [ Svg.rect
                    [ SvgAttrs.width "51"
                    , SvgAttrs.height "191"
                    , SvgAttrs.x "58"
                    , SvgAttrs.y "3"
                    , SvgAttrs.fill "#00BDE6"
                    , SvgAttrs.rx "25.5"
                    , SvgAttrs.transform "rotate(37 83 98)"
                    ]
                    []
                , Svg.rect
                    [ SvgAttrs.width "51"
                    , SvgAttrs.height "191"
                    , SvgAttrs.x "300"
                    , SvgAttrs.y "2"
                    , SvgAttrs.fill "#00BDE6"
                    , SvgAttrs.rx "25.5"
                    , SvgAttrs.transform "scale(-1 1) rotate(37 0 -875)"
                    ]
                    []
                , Svg.path
                    [ SvgAttrs.fill "#F17DA8"
                    , SvgAttrs.d "M206 6a343 343 0 0 1 90 13l1 4v116a6 6 0 0 1-6 6H119a6 6 0 0 1-6-6V23a6 6 0 0 1 5-6c28-7 57-11 88-11Z"
                    ]
                    []
                , Svg.path
                    [ SvgAttrs.fill "#0AB36A"
                    , SvgAttrs.d "M246 17h20v129h-20z"
                    ]
                    []
                , Svg.path
                    [ SvgAttrs.fill "#F04137"
                    , SvgAttrs.d "M140 17h20v129h-20z"
                    ]
                    []
                ]
            , Svg.path
                [ SvgAttrs.fill "#F4821E"
                , SvgAttrs.d "M261 307h56v31h-56z"
                ]
                []
            , Svg.g
                [ SvgAttrs.fill "#4A3083"
                , SvgAttrs.transform "translate(233 316)"
                ]
                [ Svg.path
                    [ SvgAttrs.d "M51 31a3 3 0 0 1-2 4L11 56a3 3 0 0 1-4-2 91 91 0 0 1 0-44 3 3 0 0 1 4-2l38 21 2 2ZM63 31a3 3 0 0 0 2 4l38 21a3 3 0 0 0 4-2 91 91 0 0 0 0-44 3 3 0 0 0-4-2L65 29l-2 2Z"
                    ]
                    []
                , Svg.circle
                    [ SvgAttrs.cx "56"
                    , SvgAttrs.cy "31"
                    , SvgAttrs.r "19"
                    ]
                    []
                ]
            , Svg.g
                [ SvgAttrs.transform "translate(106 44)"
                ]
                [ Svg.g
                    [ SvgAttrs.fill "#A8A9AD"
                    ]
                    [ Svg.path
                        [ SvgAttrs.d "M76 4a38 38 0 0 1 33 20 38 38 0 1 1 27 66h-5v22h-22a38 38 0 0 1-3 50 38 38 0 0 1-65-30 38 38 0 0 1-28-63 38 38 0 0 1 28-13 38 38 0 0 1 8-41C56 8 66 4 76 4ZM292 4a38 38 0 0 0-33 20 38 38 0 1 0-27 66h5v22h22a38 38 0 0 0 3 50 38 38 0 0 0 65-30 38 38 0 0 0 28-63 38 38 0 0 0-28-13 38 38 0 0 0-8-41c-7-7-17-11-27-11Z"
                        ]
                        []
                    ]
                , Svg.rect
                    [ SvgAttrs.width "217"
                    , SvgAttrs.height "58"
                    , SvgAttrs.x "74"
                    , SvgAttrs.y "133"
                    , SvgAttrs.fill "#F4821E"
                    , SvgAttrs.rx "19"
                    ]
                    []
                , Svg.rect
                    [ SvgAttrs.width "170"
                    , SvgAttrs.height "217"
                    , SvgAttrs.x "97"
                    , SvgAttrs.y "50"
                    , SvgAttrs.fill "#F4821E"
                    , SvgAttrs.rx "72"
                    ]
                    []
                , Svg.ellipse
                    [ SvgAttrs.cx "183"
                    , SvgAttrs.cy "231.5"
                    , SvgAttrs.fill "#FFF"
                    , SvgAttrs.rx "18"
                    , SvgAttrs.ry "17.5"
                    ]
                    []
                , Svg.g
                    [ SvgAttrs.fill "#A8A9AC"
                    ]
                    [ Svg.path
                        [ SvgAttrs.d "M178 204c-5-14-28-14-43-8s-33 24-57 19-20-35-25-23-13 36 10 58c15 15 39 18 72 11 19-5 31-12 38-21 10-13 10-22 5-36ZM190 204c5-14 28-14 43-8s33 24 57 19 20-35 25-23 13 36-10 58c-16 15-40 18-72 11-19-5-32-12-38-21-10-13-10-22-5-36Z"
                        ]
                        []
                    ]
                , Svg.path
                    [ SvgAttrs.fill "#FCB014"
                    , SvgAttrs.d "M182 152a1 1 0 0 1 1 1l16 54a1 1 0 0 1 0 1h-33a1 1 0 0 1-1-1l16-54 1-1Z"
                    ]
                    []
                , Svg.circle
                    [ SvgAttrs.cx "135"
                    , SvgAttrs.cy "149"
                    , SvgAttrs.r "36"
                    , SvgAttrs.fill "#FFF"
                    ]
                    []
                , Svg.circle
                    [ SvgAttrs.cx "228"
                    , SvgAttrs.cy "149"
                    , SvgAttrs.r "36"
                    , SvgAttrs.fill "#FFF"
                    ]
                    []
                ]
            ]
        ]


iconCuriosity =
    Svg.svg
        [ SvgAttrs.viewBox "0 0 580 460"
        ]
        [ Svg.g
            [ SvgAttrs.fill "none"
            , SvgAttrs.fillRule "evenodd"
            , SvgAttrs.stroke "#000"
            , SvgAttrs.strokeWidth "8"
            ]
            [ Svg.g
                [ SvgAttrs.transform "rotate(-58 367 -105)"
                ]
                [ Svg.path
                    [ SvgAttrs.fill "#5A5A5C"
                    , SvgAttrs.d "M44 3v80H12V3z"
                    ]
                    []
                , Svg.rect
                    [ SvgAttrs.width "230"
                    , SvgAttrs.height "47"
                    , SvgAttrs.x "-88"
                    , SvgAttrs.y "140"
                    , SvgAttrs.fill "#CE8759"
                    , SvgAttrs.rx "16"
                    , SvgAttrs.transform "rotate(90 27 163)"
                    ]
                    []
                ]
            , Svg.path
                [ SvgAttrs.fill "#D8D8D8"
                , SvgAttrs.d "M180 53a125 125 0 0 1 88 213 125 125 0 0 1-213-88A125 125 0 0 1 180 53Zm0 26a99 99 0 0 0-70 169 99 99 0 0 0 169-70 99 99 0 0 0-99-99Z"
                ]
                []
            ]
        ]


iconFreedom =
    Svg.svg
        [ SvgAttrs.viewBox "0 0 580 460"
        ]
        [ Svg.g
            [ SvgAttrs.fill "none"
            , SvgAttrs.fillRule "evenodd"
            , SvgAttrs.stroke "#000"
            , SvgAttrs.strokeWidth "8"
            ]
            [ Svg.path
                [ SvgAttrs.fill "#FFF"
                , SvgAttrs.d "M200 290a32 32 0 0 1 23 55 32 32 0 0 1-46 0 32 32 0 0 1-45 0 32 32 0 1 1 1-46 32 32 0 0 1 45 0 32 32 0 0 1 22-9ZM388 131a29 29 0 0 1 27 19 28 28 0 0 1 38 1 30 30 0 0 1 9 25 28 28 0 0 1 33 6c6 5 9 13 9 21s-3 16-9 21a28 28 0 0 1-33 6 31 31 0 0 1-9 25 28 28 0 0 1-38 1l-2 4a28 28 0 0 1-39 11 30 30 0 0 1-13-15 28 28 0 0 1-19 7c-8 0-14-3-20-8a30 30 0 0 1-9-25 28 28 0 0 1-32-6c-6-5-9-13-9-21s3-16 9-21a28 28 0 0 1 32-6 31 31 0 0 1 9-25 28 28 0 0 1 39-1 30 30 0 0 1 13-15c4-3 9-4 14-4Z"
                ]
                []
            , Svg.g
                [ SvgAttrs.transform "translate(202 198)"
                ]
                [ Svg.path
                    [ SvgAttrs.fill "#D8D8D8"
                    , SvgAttrs.strokeLinecap "round"
                    , SvgAttrs.d "M1 55h32"
                    ]
                    []
                , Svg.path
                    [ SvgAttrs.fill "#EDE80C"
                    , SvgAttrs.d "M35 28c5 0 10 2 14 6s7 10 7 17h106c0 6-2 11-6 14v1a21 21 0 0 1-31-1 41 41 0 0 1-17 23 39 39 0 0 1-42 0 41 41 0 0 1-17-22 21 21 0 0 1-30 0 22 22 0 0 1 16-38Z"
                    ]
                    []
                , Svg.path
                    [ SvgAttrs.fill "#EDE80C"
                    , SvgAttrs.strokeLinecap "round"
                    , SvgAttrs.strokeLinejoin "round"
                    , SvgAttrs.d "M90 51h26c3-1 4-4 4-8s-1-7-4-8h-14 28c5-1 7-4 7-9s-2-8-6-9h-18 25c6-1 9-4 9-9s-2-7-6-8h-29l-10 2c-3 1-15 13-36 36-3 4-5 8-5 12s5 10 14 17"
                    ]
                    []
                , Svg.circle
                    [ SvgAttrs.cx "31"
                    , SvgAttrs.cy "45"
                    , SvgAttrs.r "7"
                    , SvgAttrs.fill "#FFF"
                    ]
                    []
                ]
            ]
        ]


iconGoal =
    Svg.svg
        [ SvgAttrs.viewBox "0 0 580 460"
        ]
        [ Svg.g
            [ SvgAttrs.fill "none"
            , SvgAttrs.fillRule "evenodd"
            , SvgAttrs.stroke "#000"
            ]
            [ Svg.path
                [ SvgAttrs.fill "#FFF"
                , SvgAttrs.strokeWidth "8"
                , SvgAttrs.d "M201 132a32 32 0 0 1 23 55 32 32 0 0 1-46 0 32 32 0 0 1-45 0 32 32 0 1 1 1-46 32 32 0 0 1 45 0 32 32 0 0 1 22-9ZM472 272a32 32 0 0 1 23 55 32 32 0 0 1-46 0 32 32 0 0 1-45 0 32 32 0 1 1 1-46 32 32 0 0 1 45 0 32 32 0 0 1 22-9Z"
                ]
                []
            , Svg.path
                [ SvgAttrs.fill "#A8A9AD"
                , SvgAttrs.strokeWidth "9"
                , SvgAttrs.d "m439 376-78-209h-63l-45 109-17-30h-29l-61 130z"
                ]
                []
            , Svg.path
                [ SvgAttrs.fill "#FFF"
                , SvgAttrs.strokeWidth "9"
                , SvgAttrs.d "m236 246 10 18a25 25 0 0 1-25-4 25 25 0 0 1-24 7l10-21h29Zm125-79 20 55a25 25 0 0 1-29-4 25 25 0 0 1-48-1 25 25 0 0 1-29 5l23-55h63Z"
                ]
                []
            , Svg.path
                [ SvgAttrs.strokeLinecap "square"
                , SvgAttrs.strokeWidth "9"
                , SvgAttrs.d "M331 139v23"
                ]
                []
            , Svg.path
                [ SvgAttrs.fill "#F04135"
                , SvgAttrs.strokeLinejoin "round"
                , SvgAttrs.strokeWidth "8"
                , SvgAttrs.d "M330 116v26c3-4 6-7 10-8 6-2 10 6 26 2 11-3 16-10 15-21-4 4-10 6-17 5-11-1-14-11-24-10-7 1-10 3-10 6Z"
                ]
                []
            ]
        ]


iconHonor =
    Svg.svg
        [ SvgAttrs.viewBox "0 0 580 460"
        ]
        [ Svg.g
            [ SvgAttrs.fill "none"
            , SvgAttrs.fillRule "evenodd"
            , SvgAttrs.stroke "#000"
            , SvgAttrs.strokeWidth "8"
            , SvgAttrs.transform "translate(202 52)"
            ]
            [ Svg.circle
                [ SvgAttrs.cx "88"
                , SvgAttrs.cy "274"
                , SvgAttrs.r "79"
                , SvgAttrs.fill "#FFAF18"
                ]
                []
            , Svg.path
                [ SvgAttrs.fill "#EF4035"
                , SvgAttrs.d "M172 4v74l-72 80H76L4 78V4h168Z"
                ]
                []
            , Svg.path
                [ SvgAttrs.fill "#FFF"
                , SvgAttrs.d "M58 4v133l-20-23V4h20ZM118 5v132l20-23V5h-20Z"
                ]
                []
            , Svg.path
                [ SvgAttrs.fill "#F6821E"
                , SvgAttrs.d "m88 202 21 43 47 7-34 33 8 47-42-22-42 22 8-47-34-33 47-7 21-43Z"
                ]
                []
            , Svg.circle
                [ SvgAttrs.cx "88"
                , SvgAttrs.cy "174"
                , SvgAttrs.r "23"
                , SvgAttrs.fill "#F6821E"
                ]
                []
            ]
        ]


iconMastery =
    Svg.svg
        [ SvgAttrs.viewBox "0 0 580 460"
        ]
        [ Svg.g
            [ SvgAttrs.fill "none"
            , SvgAttrs.fillRule "evenodd"
            , SvgAttrs.stroke "#000"
            , SvgAttrs.strokeWidth "8"
            ]
            [ Svg.g
                [ SvgAttrs.fill "#585A5A"
                , SvgAttrs.transform "translate(326 182)"
                ]
                [ Svg.circle
                    [ SvgAttrs.cx "55"
                    , SvgAttrs.cy "31"
                    , SvgAttrs.r "27"
                    ]
                    []
                , Svg.path
                    [ SvgAttrs.d "m68 74 7 65H35l6-65h27ZM79 153l18 61-84 2 19-63h47Z"
                    ]
                    []
                , Svg.rect
                    [ SvgAttrs.width "50"
                    , SvgAttrs.height "14"
                    , SvgAttrs.x "30"
                    , SvgAttrs.y "60"
                    , SvgAttrs.rx "7"
                    ]
                    []
                , Svg.rect
                    [ SvgAttrs.width "101"
                    , SvgAttrs.height "14"
                    , SvgAttrs.x "4"
                    , SvgAttrs.y "213"
                    , SvgAttrs.rx "7"
                    ]
                    []
                , Svg.rect
                    [ SvgAttrs.width "61"
                    , SvgAttrs.height "14"
                    , SvgAttrs.x "24"
                    , SvgAttrs.y "139"
                    , SvgAttrs.rx "7"
                    ]
                    []
                ]
            , Svg.g
                [ SvgAttrs.transform "translate(154 55)"
                ]
                [ Svg.circle
                    [ SvgAttrs.cx "77"
                    , SvgAttrs.cy "236"
                    , SvgAttrs.r "61"
                    , SvgAttrs.fill "#B34B23"
                    ]
                    []
                , Svg.path
                    [ SvgAttrs.fill "#B34B23"
                    , SvgAttrs.d "m129 280 11 63H14l11-63h104Z"
                    ]
                    []
                , Svg.path
                    [ SvgAttrs.fill "#B44C23"
                    , SvgAttrs.d "m64 7 4 21 72 45 4 16-13 18-23-2-3-8-11-3c-3 2-7 3-13 3-4 0-8 0-9-2l-6-6-1 9a27 27 0 0 0 4 18v1h1c4 6 10 11 26 18s27 18 31 32l3 20H27a55 55 0 0 0-6-30c-6-12-14-31-14-64C7 65 26 37 64 7Z"
                    ]
                    []
                , Svg.rect
                    [ SvgAttrs.width "121"
                    , SvgAttrs.height "14"
                    , SvgAttrs.x "16"
                    , SvgAttrs.y "189"
                    , SvgAttrs.fill "#B14C23"
                    , SvgAttrs.rx "7"
                    ]
                    []
                , Svg.rect
                    [ SvgAttrs.width "121"
                    , SvgAttrs.height "14"
                    , SvgAttrs.x "16"
                    , SvgAttrs.y "266"
                    , SvgAttrs.fill "#B14C23"
                    , SvgAttrs.rx "7"
                    ]
                    []
                , Svg.rect
                    [ SvgAttrs.width "145"
                    , SvgAttrs.height "18.1"
                    , SvgAttrs.x "4"
                    , SvgAttrs.y "338"
                    , SvgAttrs.fill "#B14C23"
                    , SvgAttrs.rx "9"
                    ]
                    []
                ]
            ]
        ]


iconOrder =
    Svg.svg
        [ SvgAttrs.viewBox "0 0 580 460"
        ]
        [ Svg.g
            [ SvgAttrs.fill "none"
            , SvgAttrs.fillRule "evenodd"
            , SvgAttrs.stroke "#000"
            ]
            [ Svg.g
                [ SvgAttrs.fill "#047AAA"
                ]
                [ Svg.path
                    [ SvgAttrs.strokeWidth "7"
                    , SvgAttrs.d "M212 187h21v12h-21z"
                    ]
                    []
                , Svg.path
                    [ SvgAttrs.strokeWidth "8"
                    , SvgAttrs.d "M199 199h88v68h-88z"
                    ]
                    []
                ]
            , Svg.g
                [ SvgAttrs.fill "#F53E3A"
                ]
                [ Svg.path
                    [ SvgAttrs.strokeWidth "7"
                    , SvgAttrs.d "M168 255h21v12h-21z"
                    ]
                    []
                , Svg.path
                    [ SvgAttrs.strokeWidth "8"
                    , SvgAttrs.d "M156 267h88v68h-88z"
                    ]
                    []
                ]
            , Svg.g
                [ SvgAttrs.fill "#EEE70C"
                ]
                [ Svg.path
                    [ SvgAttrs.strokeWidth "7"
                    , SvgAttrs.d "M344 187h21v12h-21z"
                    ]
                    []
                , Svg.path
                    [ SvgAttrs.strokeWidth "8"
                    , SvgAttrs.d "M286 199h88v68h-88z"
                    ]
                    []
                ]
            , Svg.g
                [ SvgAttrs.fill "#0FB56C"
                ]
                [ Svg.path
                    [ SvgAttrs.strokeWidth "7"
                    , SvgAttrs.d "M388 256h21v12h-21z"
                    ]
                    []
                , Svg.path
                    [ SvgAttrs.strokeWidth "8"
                    , SvgAttrs.d "M244 267h175v68H244z"
                    ]
                    []
                ]
            , Svg.g
                [ SvgAttrs.fill "#F53E3A"
                ]
                [ Svg.g
                    [ SvgAttrs.strokeWidth "7"
                    ]
                    [ Svg.path
                        [ SvgAttrs.d "M257 117h21v12h-21zM299 117h21v12h-21z"
                        ]
                        []
                    ]
                , Svg.path
                    [ SvgAttrs.strokeWidth "8"
                    , SvgAttrs.d "M244 130h88v68h-88z"
                    ]
                    []
                ]
            ]
        ]


iconPower =
    Svg.svg
        [ SvgAttrs.viewBox "0 0 580 460"
        ]
        [ Svg.g
            [ SvgAttrs.fill "none"
            , SvgAttrs.fillRule "evenodd"
            , SvgAttrs.stroke "#000"
            , SvgAttrs.strokeWidth "8"
            ]
            [ Svg.g
                [ SvgAttrs.fill "#F6E4D2"
                , SvgAttrs.transform "translate(85 236)"
                ]
                [ Svg.rect
                    [ SvgAttrs.width "15"
                    , SvgAttrs.height "100"
                    , SvgAttrs.x "4"
                    , SvgAttrs.y "6"
                    , SvgAttrs.rx "6"
                    ]
                    []
                , Svg.rect
                    [ SvgAttrs.width "15"
                    , SvgAttrs.height "100"
                    , SvgAttrs.x "52"
                    , SvgAttrs.y "6"
                    , SvgAttrs.rx "6"
                    ]
                    []
                , Svg.rect
                    [ SvgAttrs.width "15"
                    , SvgAttrs.height "100"
                    , SvgAttrs.x "100"
                    , SvgAttrs.y "6"
                    , SvgAttrs.rx "6"
                    ]
                    []
                , Svg.rect
                    [ SvgAttrs.width "15"
                    , SvgAttrs.height "100"
                    , SvgAttrs.x "148"
                    , SvgAttrs.y "6"
                    , SvgAttrs.rx "6"
                    ]
                    []
                , Svg.rect
                    [ SvgAttrs.width "15"
                    , SvgAttrs.height "100"
                    , SvgAttrs.x "196"
                    , SvgAttrs.y "6"
                    , SvgAttrs.rx "6"
                    ]
                    []
                , Svg.rect
                    [ SvgAttrs.width "15"
                    , SvgAttrs.height "100"
                    , SvgAttrs.x "241"
                    , SvgAttrs.y "5"
                    , SvgAttrs.rx "6"
                    , SvgAttrs.transform "rotate(-19 248 55)"
                    ]
                    []
                ]
            , Svg.path
                [ SvgAttrs.fill "#EDE80E"
                , SvgAttrs.d "m586 114-99 40-1-1-8-5c-22-11-45-21-69-30-8-4-14-3-19 2-4 5-5 9-4 14 4 7 8 12 14 15l21 13c3 3 6 8 5 12 0 2-28 28-82 76-7 7-7 14 0 20 3 3 9 6 14 5 4-1 9-4 14-9l21-20 23-20-23 20c1 4 3 8 7 11s9 3 15 1l17-16-17 16c1 5 3 9 7 11 4 3 9 3 15 2l17-16-17 16c1 5 4 9 8 11 2 2 8 3 12 1 2 0 4-1 7-4l47-41 4-8c2-3 3-7 3-10l67-27 1-79Z"
                ]
                []
            ]
        ]


iconRelatedness =
    Svg.svg
        [ SvgAttrs.viewBox "0 0 580 460"
        ]
        [ Svg.g
            [ SvgAttrs.fill "none"
            , SvgAttrs.fillRule "evenodd"
            , SvgAttrs.stroke "#000"
            , SvgAttrs.strokeWidth "8"
            ]
            [ Svg.path
                [ SvgAttrs.fill "#ECE810"
                , SvgAttrs.d "M327 118a52 52 0 0 1 37 89c-10 9-23 15-37 15h-23l-1 4v12a46 46 0 0 0 9 25c-8-2-15-6-22-12-9-9-14-18-15-29h-69a52 52 0 0 1-37-89c10-9 23-15 37-15Z"
                ]
                []
            , Svg.path
                [ SvgAttrs.fill "#02BCE7"
                , SvgAttrs.d "M224 164a52 52 0 0 1 37 89c-10 9-23 15-37 15h-23l-1 4v12a46 46 0 0 0 9 25c-8-2-15-6-22-12-9-9-14-18-15-29h-69a52 52 0 0 1-37-89c10-9 23-15 37-15Z"
                ]
                []
            , Svg.path
                [ SvgAttrs.fill "#F6831E"
                , SvgAttrs.d "M284 217a52 52 0 0 0-37 89c10 9 23 15 37 15h23l1 4v12a46 46 0 0 1-9 25c8-2 15-6 22-12 9-9 14-18 15-29h69a52 52 0 0 0 37-89c-10-9-23-15-37-15Z"
                ]
                []
            , Svg.path
                [ SvgAttrs.fill "#0DB26A"
                , SvgAttrs.d "M343 134a52 52 0 0 0-37 89c10 9 23 15 37 15h23l1 4v12a46 46 0 0 1-9 25c8-2 15-6 22-12 9-9 14-18 15-29h69a52 52 0 0 0 37-89c-10-9-23-15-37-15Z"
                ]
                []
            ]
        ]


iconStatus =
    Svg.svg
        [ SvgAttrs.viewBox "0 0 580 460"
        ]
        [ Svg.g
            [ SvgAttrs.fill "none"
            , SvgAttrs.fillRule "evenodd"
            , SvgAttrs.stroke "#000"
            ]
            [ Svg.path
                [ SvgAttrs.fill "#F7821D"
                , SvgAttrs.strokeWidth "9"
                , SvgAttrs.d "M214 167h146v169H214z"
                ]
                []
            , Svg.path
                [ SvgAttrs.strokeLinecap "round"
                , SvgAttrs.strokeWidth "8"
                , SvgAttrs.d "M287 285v25"
                ]
                []
            , Svg.path
                [ SvgAttrs.fill "#EDE80B"
                , SvgAttrs.strokeWidth "9"
                , SvgAttrs.d "M360 252h140v84H360z"
                ]
                []
            , Svg.g
                [ SvgAttrs.strokeLinecap "round"
                , SvgAttrs.strokeWidth "8"
                ]
                [ Svg.path
                    [ SvgAttrs.d "M418 285v25M430 285v25M442 285v25"
                    ]
                    []
                ]
            , Svg.g []
                [ Svg.path
                    [ SvgAttrs.fill "#FDAF17"
                    , SvgAttrs.strokeWidth "9"
                    , SvgAttrs.d "M74 217h140v119H74z"
                    ]
                    []
                , Svg.g
                    [ SvgAttrs.strokeLinecap "round"
                    , SvgAttrs.strokeWidth "8"
                    ]
                    [ Svg.path
                        [ SvgAttrs.d "M138 285v25M150 285v25"
                        ]
                        []
                    ]
                ]
            ]
        ]
