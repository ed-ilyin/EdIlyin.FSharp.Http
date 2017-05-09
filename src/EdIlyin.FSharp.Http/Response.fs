namespace EdIlyin.FSharp.Http

open EdIlyin.FSharp.Elm.Core
open EdIlyin.FSharp.Boxcar
open Hopac
open FSharp.Data

/// Documentation for my library
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///
module Response = 
    /// Returns 42
    ///
    /// ## Parameters
    ///  - `num` - whatever
    let statusCode expecting =
        Decode.satisfy
            (fun (r:HttpResponse) -> r => Some r.StatusCode)
            (fun sc -> if sc = expecting then Ok sc else Err sc)
            (sprintf "status code %A" expecting)


    let bodyText =
        Decode.satisfy (fun (r:HttpResponse) -> r, Some r.Body)
            (fun body ->
                match body with
                    | Binary _ -> Err "binary body"
                    | Text text -> Ok text
            )
            "text body"


    let unpack parser async =
        boxcar {
            let! response = Job.fromAsync async |> Boxcar.catch

            return!
                try
                    do printfn "Response %i from %s"
                        response.StatusCode
                        response.ResponseUrl

                    Decode.parseAny parser response

                with | exn ->
                    sprintf "%s\n%A" exn.Message response |> Err
                |> Boxcar.fromResult
        }
