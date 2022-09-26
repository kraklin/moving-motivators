module Tests exposing (..)

import Card exposing (CardType(..), Influence(..))
import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Encode and decode"
        [ Test.describe "encode"
            [ Test.test "encoding card deck should produce CHAMPFROGS string" <|
                \_ ->
                    Card.deck
                        |> Card.encode
                        |> Expect.equal "C.H.A.M.P.F.R.O.G.S."
            , Test.test "encoding card deck with all positive influence should produce CHAMPFROGS string with - after letters" <|
                \_ ->
                    Card.deck
                        |> List.map (\card -> { card | influence = Positive })
                        |> Card.encode
                        |> Expect.equal "C-H-A-M-P-F-R-O-G-S-"
            , Test.test "encoding card deck with all negative influence should produce CHAMPFROGS string with _ after letters" <|
                \_ ->
                    Card.deck
                        |> List.map (\card -> { card | influence = Negative })
                        |> Card.encode
                        |> Expect.equal "C_H_A_M_P_F_R_O_G_S_"
            ]
        , Test.describe "decode"
            [ Test.test "decoding C. to Curiosity card" <|
                \_ ->
                    Card.decode "C.H.A.M.P.F.R.O.G.S."
                        |> Expect.equal Card.deck
            ]
        ]
