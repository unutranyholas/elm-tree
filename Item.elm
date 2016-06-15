module Item exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (concat)


-- import Html.Events exposing (..)
--
-- MODEL


type alias Model =
    { name : String
    , color : String
    , isActive : Bool
    }


init : String -> Model
init name =
    { name = name
    , color = "#660000"
    , isActive = True
    }



-- UPDATES


type Msg
    = Rename String
    | Activate


update : Msg -> Model -> Model
update msg model =
    case msg of
        Rename name ->
            { model | name = name }

        Activate ->
            { model | isActive = True }



-- VIEW


view : Model -> Html Msg
view model =
    case model.isActive of
        False ->
            div [ style (defaultItem model.color), onClick Activate ]
                [ text model.name
                ]

        True ->
            div [ style (activeItem model.color) ]
                [ input [ type' "text", style inputStyle, value model.name, onInput Rename ] []
                ]



-- STYLES


type alias Color =
    String


type alias Attributes =
    List ( String, String )


basicItem : Attributes
basicItem =
    [ ( "padding", "12px 24px" )
    , ( "display", "inline-block" )
    , ( "fontSize", "14px" )
    , ( "fontFamily", "Helvetica, Arial, sans-serif" )
    ]


activeItem : Color -> Attributes
activeItem color =
    concat
        [ basicItem
        , [ ( "backgroundColor", color ) ]
        ]


defaultItem : Color -> Attributes
defaultItem color =
    concat
        [ basicItem
        , [ ( "color", color ) ]
        ]


inputStyle : Attributes
inputStyle =
    [ ( "border", "none" )
    , ( "background", "transparent" )
    , ( "color", "#fff" )
    , ( "padding", "0px" )
    , ( "fontSize", "inherit" )
    , ( "fontFamily", "inherit" )
    ]
