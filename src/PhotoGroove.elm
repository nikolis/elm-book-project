module PhotoGroove exposing (main, photoDecoder) 

import Html exposing (..)
import Html.Events exposing (onClick)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Decode exposing (Decoder, bool, int, list, string, succeed)
import Html.Attributes as Attr exposing ( class, src, id, classList, title, name, type_)
import Browser
import Random
import Http

type ThumbnailSize 
    = Small
    | Medium
    | Large

type alias Photo = 
    { url : String 
    , size : Int
    , title : String
    }

photoDecoder : Decoder Photo
photoDecoder = 
    succeed Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"

type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String

type alias Model = 
    { status : Status
    , selectedUrl : String
    , chosenSize : ThumbnailSize
    }

type Msg 
    = ClickedPhoto String
    | ClickedSize ThumbnailSize 
    | ClickedSurptiseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))

urlPrefix : String
urlPrefix = "http://elm-in-action.com/"


view : Model -> Html Msg
view model = 
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model.chosenSize
            Loading ->
                []
            Errored errorMessage ->
                [text ("Error: " ++ errorMessage) ]
        

viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
    [h1 [] [text "Photo Groove xspt"]
    ,button 
        [ onClick ClickedSurptiseMe ]
        [ text "Surprise Me ! asfasdfafsdafaffads"]
    ,h3 [] [text "Thumbnail Size: "]
    ,div [id "choose-size"]
        (List.map viewSizeChooser [ Small, Medium, Large ])
    , div [ id "thumnails", class (sizeToString chosenSize)]
        (List.map (viewThumbnail selectedUrl) photos)
    , img
        [ class "large"
        , src (urlPrefix ++ "large/" ++ selectedUrl)
        ]
        []
    ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
  img 
      [ src (urlPrefix ++ thumb.url)
      , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ "KB]")
      , classList [ ("selected", selectedUrl == thumb.url ) ]
      , onClick (ClickedPhoto thumb.url)
      ]
      []

viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size = 
    label []
        [ input [ type_ "radio", name "size", onClick (ClickedSize size)] []
        , text (sizeToString size)
        ]

sizeToString : ThumbnailSize -> String
sizeToString size = 
    case size of 
        Small ->
            "small"

        Medium -> 
            "med"
        
        Large ->
            "large"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of 
        GotRandomPhoto photo ->
            ( {model | status = selectUrl photo.url model.status}, Cmd.none )
        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none)
        ClickedSurptiseMe ->
            case model.status of 
                Loaded (firstPhoto :: otherPhotos) _ ->
                    ( model
                    , Random.generate GotRandomPhoto
                        (Random.uniform firstPhoto otherPhotos)
                    )
                Loaded [] _ ->
                    (model, Cmd.none)
                Loading ->
                    (model, Cmd.none)
                Errored errorMessage ->
                    (model, Cmd.none)
        ClickedSize size ->
            ( { model | chosenSize = size}, Cmd.none )
        GotPhotos(Ok photos) ->
            case photos of
                first :: rest ->
                    ( 
                        {model | status = Loaded photos first.url }
                        , Cmd.none
                    )
                [] ->
                    ( {model | status = Errored "No photos found."}, Cmd.none)

        GotPhotos(Err httpError) ->
            Debug.log("O kipos einai anthiros")
            ( {model | status = Errored "Server error!"}, Cmd.none)



selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url
        Loading ->
            status
        Errored errorMessage ->
            status

initialModel : Model
initialModel = 
    { status = Loading
    , chosenSize = Large
    , selectedUrl = ""
    }


initialCmd : Cmd Msg
initialCmd = 
    Http.get 
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (list photoDecoder) 
        }

main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> (initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
