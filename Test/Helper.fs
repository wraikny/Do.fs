[<AutoOpen>]
module Helper

open System
open System.Runtime.ExceptionServices

type Exception with
  member this.Reraise () =
    (ExceptionDispatchInfo.Capture this).Throw ()
    Unchecked.defaultof<_>

type MyException(msg) =
  inherit Exception(msg)

let assertEqual (expected: 'a) (actual: 'a) =
  NUnit.Framework.Assert.AreEqual(expected, actual)
