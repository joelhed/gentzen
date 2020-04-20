module Main exposing (..)
import Browser
import Html exposing (Html, h1, div, span, text, hr)
import Html.Attributes exposing (style)


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
          [ Assumption (Statement "P"), Assumption (Statement "P -> Q") ]
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

renderStatement : Statement -> Html Msg
renderStatement (Statement s) = span [ style "padding" "0.5em" ] [text s]
  

renderProof : Proof -> Html Msg
renderProof proof = 
  case proof of
    Assumption statement ->
      renderStatement statement

    Proof premises conclusion ->
      div [ style "display" "inline-block" ]
        [ div [] <| List.map renderProof premises
        , hr [] []
        , div [ style "text-align" "center" ] [ renderStatement conclusion ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "TEST"
    , body =
      [ h1 [] [ text "TEST"]
      -- The drawing surface
      , div []
          [ renderProof model.proof
          ]
      ]
    }

