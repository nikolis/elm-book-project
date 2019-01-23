module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Json.Decode exposing (decodeString, list)
import PhotoGroove
import Test exposing (..)


decoderTest : Test
decoderTest =
    test "title defaults to (untitled)"
        (\_ ->
            """ {"url": "fruits.com", "size": 5} """
                |> decodeString PhotoGroove.photoDecoder
                |> Expect.equal
                    (Ok { url = "fruits.com", size = 5, title = "(untitled)"})
        )

decoderTesto =
    test "list json decoding"
        (\_ ->
            """ [{"url": "fruits.com", "size": 5},
                {"url": "fruits.com", "size": 5}]
             """
                |> decodeString (list PhotoGroove.photoDecoder) 
                |> Expect.equal
                    (Ok { url = "fruits.com", size = 5, title = "(untitled)"})
        )