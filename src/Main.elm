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


viewRemovePremiseButton : Int -> Html Msg
viewRemovePremiseButton idx =
  button [ class "remove-premise", onClick (RemovePremise idx) ] [ text "✕" ]


viewStatement : Int -> Bool -> Statement -> Html Msg
viewStatement idx showBrackets (Statement s isDischarged) =
  let
    dischargeStyle =
      [ onClick (ToggleDischarge idx), class "discharge" ] ++
      if isDischarged then
        []
      else
        [ class "discharge-off" ]

    surroundWithBrackets l =
      if showBrackets then
        [ span dischargeStyle [ text "[" ] ]
        ++ l
        ++ [ span dischargeStyle [ text "]" ] ]
      else
        l

    -- WARNING: This isn't calculated from the actual character width, but came from
    -- experimentation. It might not always work.
    fieldWidth =
      (Basics.max (7 * String.length s) 10)
      
  in
  span [ class "statement" ]
    <| surroundWithBrackets
      [ input
        [ placeholder "..."
        , value s
        , onInput (UpdateStatement idx)
        , style "width" <| String.fromInt fieldWidth ++ "px"
        ]
        []
      ]
      ++ [ viewRemovePremiseButton idx ]


viewAssumption : Int -> Statement -> Html Msg
viewAssumption idx statement =
  div [ class "assumption" ]
    [ hr [ class "assumption-line" , onClick (AddPremise idx) ] []
    , viewStatement idx True statement
    ]


viewAddPremiseButton : Int -> Html Msg
viewAddPremiseButton idx =
  button [ class "add-premise", onClick (AddPremise idx) ] [ text "+" ]


viewSubproof : Int -> Statement -> List EnumeratedProof -> Html Msg
viewSubproof idx conclusion premises =
  div [ class "proof" ]
    [ div [ class "premises" ]
      <| (List.map (wrapInPremiseDiv << viewEnumeratedProof) premises)
      ++ [ viewAddPremiseButton idx ]
    , div [ class "conclusion" ] [ viewStatement idx False conclusion ]
    ]


viewEnumeratedProof : EnumeratedProof -> Html Msg
viewEnumeratedProof proof =
  case proof of
    Tree.Node (idx, statement) [] ->
      viewAssumption idx statement

    Tree.Node (idx, conclusion) premises ->
      viewSubproof idx conclusion premises


viewProof : Proof -> Html Msg
viewProof = viewEnumeratedProof << Tree.enumerate


view : Model -> Browser.Document Msg
view model =
    { title = "Gentzen"
    , body =
      [ div [ class "container" ]
        [ h1 [] [ text "Gentzen proof editor"]
        -- The drawing surface
        , div [ class "proof-area" ]
            [ viewProof model.proof
            ]
        ]
      ]
    }

