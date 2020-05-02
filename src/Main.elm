module Main exposing (..)
import Browser
import Html exposing (Html, h1, div, span, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Tree


main = Browser.document
  { init = init
  , subscriptions = subscriptions
  , update = update
  , view = view
  }


-- Model

type Statement = Statement String

type alias Proof = Tree.Node Statement
type alias IndexedProof = Tree.Node ( Int, Statement )

assumption : Statement -> Proof
assumption s = Tree.Node s []


inference : List Proof -> Statement -> Proof
inference premises conclusion = Tree.Node conclusion premises


type alias Model =
  { proof: Proof
  }


init : () -> ( Model, Cmd Msg )
init = always
  ( { proof =
      
      inference 
        [ inference
          [ inference
            [ assumption (Statement "P"), assumption (Statement "P -> (Q ^ R)") ]
            (Statement "Q ^ R")
          ]
          (Statement "Q")
        , assumption (Statement "~Q")
        ]
        (Statement "~P")
    }
  , Cmd.none
  )


type Msg
  = UpdateStatement Int String


changeIfIdx : Int -> String -> Int -> Statement -> Statement
changeIfIdx idx1 newString idx2 oldStatement =
  if idx1 == idx2 then
    (Statement newString)
  else
    oldStatement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateStatement idx s ->
      ( { model
        | proof = Tree.indexedMap (changeIfIdx idx s) model.proof
        }
      , Cmd.none
      )


subscriptions : Model -> Sub Msg
subscriptions = always Sub.none

-- View


wrapInPremiseDiv : Html Msg -> Html Msg
wrapInPremiseDiv x = div [ class "premise" ] [ x ]


renderStatement : Int -> Statement -> Html Msg
renderStatement idx (Statement s) =
  input
    [ class "statement"
    , value s
    , onInput (UpdateStatement idx)
    ]
    []


renderIndexedProof : IndexedProof -> Html Msg
renderIndexedProof proof = 
  case proof of
    Tree.Node (idx, statement) [] ->
      renderStatement idx statement

    Tree.Node (idx, conclusion) premises ->
        div [ class "proof" ]
          [ div [ class "premises" ] <| List.map (wrapInPremiseDiv << renderIndexedProof) premises
          -- , hr [] []
          , div [ class "conclusion" ] [ renderStatement idx conclusion ]
          ]


renderProof : Proof -> Html Msg
renderProof proof =
  Tree.indexedMap (\idx statement -> ( idx, statement )) proof
  |> renderIndexedProof


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

