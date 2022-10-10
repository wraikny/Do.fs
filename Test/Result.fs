module Test.Result

open NUnit.Framework

// [<SetUp>]
// let Setup () = ()

type Result<'t, 'e> with
  member x.IsOk = x |> function
    | Ok _ -> true
    | _ -> false

  member x.IsError = x |> function
    | Error _ -> true
    | _ -> false

[<Test>]
let ``Return unit`` (): unit =
  let res = Do.result { return () }
  assertEqual (Ok ()) res

[<Test>]
let ``ReturnFrom Ok()`` (): unit =
  let res = Do.result { return! Ok () }
  assertEqual (Ok ()) res

[<Test>]
let ``ReturnFrom Error ()`` (): unit =
  let res = Do.result { return! Error () }
  Assert.True (res.IsError)

[<Test>]
let ``Return int`` (): unit =
  let x = 42
  let res = Do.result { return x }
  assertEqual (Ok x) res

[<Test>]
let ``Bind`` (): unit =
  let res = Do.result {
    let! a = Ok 2
    let! b = Ok 3
    return a + b
  }
  assertEqual (Ok 5) res

[<Test>]
let ``Bind Error ()`` (): unit =
  let res = Do.result {
    let! a = Ok 2
    let! b = Error ()
    return a + b
  }
  Assert.True(res.IsError)

[<Test>]
let ``While false`` (): unit =
  let res = Do.result {
    while false do ()
    return ()
  }
  assertEqual (Ok ()) res

[<Test>]
let ``While count`` (): unit =
  let res = Do.result {
    let mutable count = 10
    while count > 0 do count <- count - 1
    return count
  }
  assertEqual (Ok 0) res

[<Test>]
let ``While return Error ()`` (): unit =
  let res = Do.result {
    let mutable count = 10
    while count > 0 do
      count <- count - 1
      do! if count = 3 then Error () else Ok ()
    return count
  }
  Assert.True(res.IsError)

[<Test>]
let ``For`` (): unit =
  let res = Do.result {
    let mutable count = 0
    for i in 0..10 do
      count <- count + i
    return count
  }
  assertEqual (Ok 55) res

[<Test>]
let ``Exception`` (): unit =
  let message = "Exception!"
  let e = Assert.Throws<MyException>(fun () ->
    Do.result {
      let! a = Ok 4
      let! b = Ok 5
      raise (MyException(message))
      return a + b
    } |> ignore)

  assertEqual message e.Message

[<Test>]
let ``TryWith without Exception`` (): unit =
  let res = Do.result {
    try
      let! a = Ok 4
      let! b = Ok 5
      return a + b
    with e ->
      return 1
  }
  assertEqual (Ok 9) res

[<Test>]
let ``TryWith catch`` (): unit =
  let res = Do.result {
    try
      let! a = Ok 4
      let! b = Ok 5
      failwith "Exception!"
      return a + b
    with e ->
      return 1
  }
  assertEqual (Ok 1) res

[<Test>]
let ``TryWith reraise`` (): unit =
  let message = "Exception!"
  let e = Assert.Throws<MyException>(fun () ->
    Do.result {
    try
      let! a = Ok 4
      let! b = Ok 5
      raise (MyException(message))
      return a + b
    with e ->
      return e.Reraise()
    } |> ignore)

  assertEqual message e.Message

[<Test>]
let ``TryFinally without Exception1`` (): unit =
  let mutable executed = false
  let res = Do.result {
    try
      do! Ok ()
    finally
      executed <- true
    return executed
  }
  assertEqual (Ok true) res

[<Test>]
let ``TryFinally without Exception2`` (): unit =
  let mutable executed = false
  let res = Do.result {
    try
      do! Ok ()
      return executed
    finally
      executed <- true
  }
  assertEqual (Ok false) res

[<Test>]
let ``TryFinally with Exception`` (): unit =
  let message = "Exception!"
  let mutable count = 0
  let res = Assert.Throws<MyException>(fun () ->
    Do.result {
      try
        do! Ok ()
        raise (MyException message)
        count <- count + 1
        do! Error ()
      finally
        count <- count + 10
      return count
    }
    |> fun o -> Assert.True(o.IsOk))
  assertEqual message res.Message
  assertEqual 10 count

[<Test>]
let ``Use`` (): unit =
  let mutable executed = false
  let res = Do.result {
    use _d =
      { new System.IDisposable with
        member _.Dispose() = executed <- true }

    return 42
  }

  assertEqual (Ok 42) res
  Assert.True executed

[<Test>]
let ``Use!`` (): unit =
  let mutable executed = false
  let res = Do.result {
    use! _d =
      { new System.IDisposable with
        member _.Dispose() = executed <- true }
      |> Ok

    return 42
  }

  assertEqual (Ok 42) res
  Assert.True executed