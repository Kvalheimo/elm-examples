module Commands exposing (..)

import Http
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)
import Msgs exposing (Msg)
import Models exposing (PlayerId, Player)
import RemoteData


fetchPlayers : Cmd Msg
fetchPlayers =
    Http.get fetchPlayersUrl playersDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.OnFetchPlayers


fetchPlayersUrl : String
fetchPlayersUrl =
    "http://localhost:4000/players"


playersDecoder : Decode.Decoder (List Player)
playersDecoder =
    Decode.list playerDecoder


playerDecoder : Decode.Decoder Player
playerDecoder =
    decode Player
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> required "level" Decode.int



savePlayerUrl : PlayerId -> String
savePlayerUrl playerId =
    "http://localhost:4000/players/" ++ playerId


savePlayerRequest : Player -> Http.Request Player
savePlayerRequest player =
    Http.request
        { body = playerEncoder player |> Http.jsonBody --Here we encode the given player and then convert the encoded value to a JSON string 
        , expect = Http.expectJson playerDecoder -- Here we specify how to parse the response, in this case we want to parse the returned JSON back into an Elm value
        , headers = []
        , method = "PATCH" -- PATCH is the http method that our API expects when updating records.
        , timeout = Nothing
        , url = savePlayerUrl player.id
        , withCredentials = False
        }


savePlayerCmd : Player -> Cmd Msg
savePlayerCmd player =
    savePlayerRequest player --Here we create the save request ➊ and then generate a command to send the request using Http.send
        |> Http.send Msgs.OnPlayerSave -- takes a message constructor (OnPlayerSave in this case). After the request is done, Elm will trigger the OnPlayerSave message with the response for the request.


playerEncoder : Player -> Encode.Value
playerEncoder player =
    let
        attributes =
            [ ( "id", Encode.string player.id )
            , ( "name", Encode.string player.name )
            , ( "level", Encode.int player.level )
            ]
    in
        Encode.object attributes