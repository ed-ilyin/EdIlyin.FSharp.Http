namespace EdIlyin.FSharp.Http

open FSharp.Data
open Hopac
open EdIlyin.FSharp.Elm.Core
open EdIlyin.FSharp.Http.Response


type Connection = {
    host: string
    headers: seq<string * string>
    jar: System.Net.CookieContainer
}


module Request =
    let connection host headers jar = {
        host = host
        headers = headers
        jar = jar
    }


    let post parser endpoint connection body =
        let url = connection.host + endpoint
        do printfn "POST %s" url

        // // Workaround for PreProd connection issue //
        // let tls12: System.Net.SecurityProtocolType =
        //     LanguagePrimitives.EnumOfValue 3072

        // do System.Net.ServicePointManager.SecurityProtocol <-
        //     tls12
        // /////////////////////////////////////////////

        Http.AsyncRequest
            ( url = connection.host + endpoint
            , headers = connection.headers
            , cookieContainer = connection.jar
            , body = body
            )
            |> unpack parser


    let postForm parser endpoint connection form =
        FormValues form |> post parser endpoint connection


    let postJson parser endpoint connection json =
        Json.Encode.encode json
            |> HttpRequestBody.TextRequest
            |> post parser endpoint connection


    let addJsonUtf8ContentType headers =
        headers
            |> Map.ofList
            |> uncurry Map.add
                (HttpRequestHeaders.ContentType
                    "application/json; charset=UTF-8"
                )
            |> Map.toList



    let get parser endpoint connection =
        Http.AsyncRequest
            ( url = connection.host + endpoint
            , headers = connection.headers
            , cookieContainer = connection.jar
            )
            |> unpack parser


    let putJson parser endpoint connection json =
        let body =
            Json.Encode.encode json |> HttpRequestBody.TextRequest

        Http.AsyncRequest
            ( httpMethod = HttpMethod.Put
            , url = connection.host + endpoint
            , headers = connection.headers
            , cookieContainer = connection.jar
            , body = body
            )
            |> unpack parser


    let put parser endpoint connection =
        Json.Encode.null' |> putJson parser endpoint connection
