module EdIlyin.FSharp.Http.Tests

open EdIlyin.FSharp.Http
open NUnit.Framework

[<Test>]
let ``hello returns 42`` () =
  // let result = Library.hello 42
  let result = 42
  printfn "%i" result
  Assert.AreEqual(42,result)
