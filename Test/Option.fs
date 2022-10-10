module Test.Option

open NUnit.Framework

// [<SetUp>]
// let Setup () = ()

[<Test>]
let ``Return unit`` (): unit =
  let res = Do.voption { return () }
  assertEqual (ValueSome ()) res

[<Test>]
let ``ReturnFrom Some()`` (): unit =
  let res = Do.voption { return! Some () }
  assertEqual (ValueSome ()) res

[<Test>]
let ``ReturnFrom None`` (): unit =
  let res = Do.voption { return! None }
  Assert.True (res.IsNone)

[<Test>]
let ``Return int`` (): unit =
  let x = 42
  let res = Do.voption { return x }
  assertEqual (ValueSome x) res

[<Test>]
let ``Bind`` (): unit =
  let res = Do.voption {
    let! a = ValueSome 2
    let! b = Some 3
    return a + b
  }
  assertEqual (ValueSome 5) res

[<Test>]
let ``Bind None`` (): unit =
  let res = Do.voption {
    let! a = ValueSome 2
    let! b = None
    return a + b
  }
  Assert.True(res.IsNone)

[<Test>]
let ``Bind Nullable(x)`` (): unit =
  let res = Do.voption {
    let! a = ValueSome 2
    let! b = System.Nullable(3)
    return a + b
  }
  assertEqual (ValueSome 5) res

[<Test>]
let ``Bind Nullable()`` (): unit =
  let res = Do.voption {
    let! a = ValueSome 2
    let! b = System.Nullable()
    return a + b
  }
  Assert.True(res.IsNone)


[<Test>]
let ``Bind object`` (): unit =
  let res = Do.voption {
    let! a = ValueSome 2
    let! _ = obj()
    return a
  }
  assertEqual (ValueSome 2) res

[<Test>]
let ``Bind null`` (): unit =
  let res = Do.voption {
    let! a = ValueSome 2
    let! _ = null
    return a
  }
  Assert.True(res.IsNone)

[<Test>]
let ``While false`` (): unit =
  let res = Do.voption {
    while false do ()
    return ()
  }
  assertEqual (ValueSome ()) res

[<Test>]
let ``While count`` (): unit =
  let res = Do.voption {
    let mutable count = 10
    while count > 0 do count <- count - 1
    return count
  }
  assertEqual (ValueSome 0) res

[<Test>]
let ``While return ValueNone`` (): unit =
  let res = Do.voption {
    let mutable count = 10
    while count > 0 do
      count <- count - 1
      do! if count = 3 then ValueNone else ValueSome ()
    return count
  }
  Assert.True(res.IsNone)

[<Test>]
let ``For`` (): unit =
  let res = Do.voption {
    let mutable count = 0
    for i in 0..10 do
      count <- count + i
    return count
  }
  assertEqual (ValueSome 55) res

[<Test>]
let ``Exception`` (): unit =
  let message = "Exception!"
  let e = Assert.Throws<MyException>(fun () ->
    Do.voption {
      let! a = Some 4
      let! b = ValueSome 5
      raise (MyException(message))
      return a + b
    } |> ignore)

  assertEqual message e.Message

[<Test>]
let ``TryWith without Exception`` (): unit =
  let res = Do.voption {
    try
      let! a = Some 4
      let! b = ValueSome 5
      return a + b
    with e ->
      return 1
  }
  assertEqual (ValueSome 9) res

[<Test>]
let ``TryWith catch`` (): unit =
  let res = Do.voption {
    try
      let! a = Some 4
      let! b = ValueSome 5
      failwith "Exception!"
      return a + b
    with e ->
      return 1
  }
  assertEqual (ValueSome 1) res

[<Test>]
let ``TryWith reraise`` (): unit =
  let message = "Exception!"
  let e = Assert.Throws<MyException>(fun () ->
    Do.voption {
    try
      let! a = Some 4
      let! b = ValueSome 5
      raise (MyException(message))
      return a + b
    with e ->
      return e.Reraise()
    } |> ignore)

  assertEqual message e.Message

[<Test>]
let ``TryFinally without Exception1`` (): unit =
  let mutable executed = false
  let res = Do.voption {
    try
      do! Some ()
    finally
      executed <- true
    return executed
  }
  assertEqual (ValueSome true) res

[<Test>]
let ``TryFinally without Exception2`` (): unit =
  let mutable executed = false
  let res = Do.voption {
    try
      do! Some ()
      return executed
    finally
      executed <- true
  }
  assertEqual (ValueSome false) res

[<Test>]
let ``TryFinally with Exception`` (): unit =
  let message = "Exception!"
  let mutable count = 0
  let res = Assert.Throws<MyException>(fun () ->
    Do.voption {
      try
        do! Some ()
        raise (MyException message)
        count <- count + 1
        do! None
      finally
        count <- count + 10
      return count
    } |> ignore)
  assertEqual message res.Message
  assertEqual 10 count

[<Test>]
let ``Use`` (): unit =
  let mutable executed = false
  let res = Do.voption {
    use _d =
      { new System.IDisposable with
        member _.Dispose() = executed <- true }

    return 42
  }

  assertEqual (ValueSome 42) res
  Assert.True executed

[<Test>]
let ``Use!`` (): unit =
  let mutable executed = false
  let res = Do.voption {
    use! _d =
      { new System.IDisposable with
        member _.Dispose() = executed <- true }
      |> ValueSome

    return 42
  }

  assertEqual (ValueSome 42) res
  Assert.True executed