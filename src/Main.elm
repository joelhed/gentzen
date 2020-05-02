module Main exposing (..)
import Browser
import Html exposing (Html, h1, div, span, text, hr)
import Html.Attributes exposing (style, class)


main = Browser.document
  { init = init
  , subscriptions = subscriptions
  , update = update
  , view = view
  }


-- Model

type Statement = Statement String

type Proof
  = Assumption Statement
  | Proof (List Proof) Statement

type alias Model =
  { proof: Proof
  }


init : () -> ( Model, Cmd Msg )
init = always
  ( { proof =
      
      Proof 
        [ Proof
          [ Proof
            [ Assumption (Statement "P"), Assumption (Statement "P -> (Q ^ R)") ]
            (Statement "Q ^ R")
          ]
          (Statement "Q")
        , Assumption (Statement "~Q")
        ]
        (Statement "~P")
    }
  , Cmd.none
  )


type Msg = Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( model
  , Cmd.none
  )


subscriptions : Model -> Sub Msg
subscriptions = always Sub.none

-- View


wrapInPremiseDiv : Html Msg -> Html Msg
wrapInPremiseDiv x = div [ class "premise" ] [ x ]


renderStatement : Statement -> Html Msg
renderStatement (Statement s) = span [ class "statement" ] [text s]
  

renderProof : Proof -> Html Msg
renderProof proof = 
  case proof of
    Assumption statement ->
      renderStatement statement

    Proof premises conclusion ->
      div [ class "proof" ]
        [ div [ class "premises" ] <| List.map (wrapInPremiseDiv << renderProof) premises
        -- , hr [] []
        , div [ class "conclusion" ] [ renderStatement conclusion ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Gentzen"
    , body =
      [ div [ class "container" ] 
        [ h1 [] [ text "Gentzen proof editor"]
        -- The drawing surface
        , div [ class "proof-area" ]
            [ renderProof model.proof
            ]
        ]
      ]
    }

