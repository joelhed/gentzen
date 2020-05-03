module Main exposing (..)
import Browser
import Html exposing (Html, h1, div, span, text, input, button, hr)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Tree


main = Browser.document
  { init = init
  , subscriptions = subscriptions
  , update = update
  , view = view
  }


-- Model

type Statement = Statement String Bool

type alias Proof = Tree.Node Statement
type alias EnumeratedProof = Tree.Node ( Int, Statement )

type alias Model =
  { proof: Proof
  }


assumption : String -> Proof
assumption s = Tree.Node (Statement s False) []


inference : List Proof -> String -> Proof
inference premises conclusion = Tree.Node (Statement conclusion False) premises


init : () -> ( Model, Cmd Msg )
init = always
  ( { proof =
      
      inference 
        [ inference
          [ inference
            [ assumption "P", assumption "P -> (Q ^ R)" ]
            "Q ^ R"
          ]
          "Q"
        , assumption "~Q"
        ]
        "~P"
    }
  , Cmd.none
  )


type Msg
  = UpdateStatement Int String
  | AddPremise Int
  | RemovePremise Int
  | ToggleDischarge Int


updateStatementWithIdx : Int -> (Statement -> Statement) -> Proof -> Proof
updateStatementWithIdx idx1 f =
  Tree.indexedMap <| \idx2 ->
    if idx1 == idx2 then
      f
    else
      identity


updateStatementString : String -> Statement -> Statement
updateStatementString newString (Statement _ isDischarged) =
  Statement newString isDischarged


toggleStatementDischarge : Statement -> Statement
toggleStatementDischarge (Statement s isDischarged) =
  Statement s (not isDischarged)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateStatement idx s ->
      ( { model
        | proof = updateStatementWithIdx idx (updateStatementString s) model.proof
        }
      , Cmd.none
      )
    
    AddPremise idx ->
      ( { model
        | proof = Tree.addChildAtIdx idx (assumption "") model.proof
        }
      , Cmd.none
      )

    RemovePremise idx ->
      ( { model
        | proof = Tree.removeNodeAtIdx idx model.proof
          |> Maybe.withDefault model.proof
        }
      , Cmd.none
      )

    ToggleDischarge idx ->
      ( { model
        | proof = updateStatementWithIdx idx toggleStatementDischarge model.proof
        }
      , Cmd.none
      )
      


subscriptions : Model -> Sub Msg
subscriptions = always Sub.none

-- View


wrapInPremiseDiv : Html Msg -> Html Msg
wrapInPremiseDiv x = div [ class "premise" ] [ x ]


renderRemovePremiseButton : Int -> Html Msg
renderRemovePremiseButton idx =
  button [ onClick (RemovePremise idx) ] [ text "-" ]


renderStatement : Int -> Bool -> Statement -> Html Msg
renderStatement idx renderDischargeBrackets (Statement s isDischarged) =
  let
    dischargeStyle =
      if isDischarged then
        [ onClick (ToggleDischarge idx) ]
      else
        [ onClick (ToggleDischarge idx), class "discharge-off" ]

    surroundWithBrackets l =
      if renderDischargeBrackets then
        [ span dischargeStyle [ text "[" ] ]
        ++ l
        ++ [ span dischargeStyle [ text "]" ] ]
      else
        l
  in
  span [ class "statement" ]
    <| surroundWithBrackets
      [ input [ value s, onInput (UpdateStatement idx) ] []
      , renderRemovePremiseButton idx
      ]


renderAddPremiseButton : Int -> Html Msg
renderAddPremiseButton idx =
  button [ onClick (AddPremise idx) ] [ text "+" ]


renderEnumeratedProof : EnumeratedProof -> Html Msg
renderEnumeratedProof proof = 
  case proof of
    Tree.Node (idx, statement) [] ->
      div []
        [ hr [ class "assumption-line" , onClick (AddPremise idx) ] []
        , renderStatement idx True statement
        ]

    Tree.Node (idx, conclusion) premises ->
      div [ class "proof" ]
        [ div [ class "premises" ] 
          <| (List.map (wrapInPremiseDiv << renderEnumeratedProof) premises)
          ++ [ renderAddPremiseButton idx ]
        , div [ class "conclusion" ] [ renderStatement idx False conclusion ]
        ]


renderProof : Proof -> Html Msg
renderProof = renderEnumeratedProof << Tree.enumerate


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

