module Main exposing (..)

import Browser exposing (sandbox)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)




-- MAIN
main =
  Browser.sandbox { init = init, update = update, view = view }




-- MODEL
  -- Shaping what the state of our todoapp looks like
type alias Todo =
  { text: String
  , completed: Bool
  }

type Filter
  = All
  | Completed
  | Remaining

type alias Model = 
  { todos: List Todo
  , inputText: String
  , filter: Filter
  }

init =
    { todos = []
    , inputText = ""
    , filter = All
    }




-- UPDATE
  -- enum of all actions that are possible in our todoapp 
type Message
  = AddTodo
  | RemoveTodoFromList Int
  | ToggleTodoCompleted Int
  | ChangeInput String
  | ChangeFilter Filter

update : Message -> Model -> Model
update message model
  = case message of
    AddTodo ->
      { model
        | todos = addTodo model.inputText model.todos
        , inputText = ""
      }
    RemoveTodoFromList index -> 
      { model | todos = removeTodoFromList index model.todos }
    ToggleTodoCompleted index ->
      { model | todos = toggleTodoCompleted index model.todos }
    ChangeInput input -> 
      { model | inputText = input }
    ChangeFilter filter ->
      { model | filter = filter }

addTodo : String -> List Todo -> List Todo
addTodo input todos = 
  todos ++ [{text = input, completed = False}]

removeTodoFromList : Int -> List Todo -> List Todo
removeTodoFromList index list =
  List.take index list ++ List.drop (index + 1) list

toggleTodoCompleted : Int -> List Todo -> List Todo
toggleTodoCompleted indexToToggle list =
  List.indexedMap (\currentIndex todo ->
    if currentIndex == indexToToggle then
      { todo | completed = not todo.completed }
      else 
        todo 
    ) list

changeInput : String -> Model -> Model
changeInput stringInput model =
  { model | inputText = stringInput }




-- VIEW
view: Model -> Html Message
view model =
    Html.form [ onSubmit AddTodo ]
        [ h1 [] [ text "Todos in Elm" ]
        , input [ value model.inputText, onInput ChangeInput, placeholder "What do you want to do?" ] []
        , viewSelectFilter model.filter
        , if List.isEmpty model.todos then
            p [] [ text "The list is clean 🧘‍♀️" ]
          else
            ol [] (model.todos
                |> List.filter (applyFilter model.filter)
                |> List.indexedMap viewTodo)
        ]

viewTodo : Int -> Todo -> Html Message
viewTodo index todo =
    li
        [ style "text-decoration"
            (if todo.completed then
                "line-through"
             else
                "none"
            )
        ]
        [ text todo.text
        , button [ type_ "button", onClick (ToggleTodoCompleted index) ] [ text "Toggle" ]
        , button [ type_ "button", onClick (RemoveTodoFromList index) ] [ text "Delete" ]
        ]

type alias RadioWithLabelProps =
  { filter: Filter
  , label: String
  , name: String
  , checked: Bool
  }

viewRadioWithLabel : RadioWithLabelProps -> Html Message
viewRadioWithLabel config =
  label []
    [ input
      [ type_ "radio"
      , name config.name
      , checked config.checked
      , onClick (ChangeFilter config.filter)
      ] []
    , text config.label 
    ]

viewSelectFilter : Filter -> Html Message
viewSelectFilter filter =
    fieldset []
        [ legend [] [ text "Current filter" ]
        , viewRadioWithLabel 
            { filter = All
            , name = "filter"
            , checked = filter == All
            , label = "All items" 
            }
        , viewRadioWithLabel 
            { filter = Completed
            , name = "filter"
            , checked = filter == Completed
            , label = "Completed items" 
            }
        , viewRadioWithLabel 
            { filter = Remaining
            , name = "filter"
            , checked = filter == Remaining
            , label = "Remaining items"
            }
        ]

applyFilter : Filter -> Todo -> Bool
applyFilter filter todo =
  case filter of
    All ->
      True

    Completed -> 
      todo.completed

    Remaining ->
      not todo.completed


