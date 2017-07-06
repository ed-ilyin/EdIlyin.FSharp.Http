namespace EdIlyin.FSharp.Http

open EdIlyin.FSharp.Elm.Core
open EdIlyin.FSharp.Boxcar
open Hopac
open FSharp.Data


module Response =
    let statusCode expecting =
        let label = sprintf "a status code %A" expecting

        Decode.primitive label
            (fun (r:HttpResponse) ->
                let sc = r.StatusCode
                if sc = expecting then Decode.Decoded sc
                else label => r |> Decode.ExpectingButGot
            )


    let bodyText =
        let label = "text body"

        Decode.primitive label
            (fun (r:HttpResponse) ->
                match r.Body with
                    | Binary _ -> label => r |> Decode.ExpectingButGot
                    | Text text -> Decode.Decoded text
            )


    let unpack parser async =
        boxcar {
            let! response = Job.fromAsync async |> Boxcar.catch

            let! result =
                try
                    do printfn "Response %i from %s"
                        response.StatusCode
                        response.ResponseUrl

                    Decode.decode parser response

                with | exn ->
                    sprintf "%s\n%A" exn.Message response |> Error
                |> Boxcar.fromResult

            return result
        }
