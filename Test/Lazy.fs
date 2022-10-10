module Test.Lazy


open NUnit.Framework

// [<SetUp>]
// let Setup () = ()

[<Test>]
let ``Return unit`` (): unit =
  let res = Do.lazily { return () }
  assertEqual () res.Value

[<Test>]
let ``Executed lazily`` (): unit =
  let mutable count = 0
  let res = Do.lazily {
    count <- count + 1
    do! lazy ()
    count <- count + 1
    do! lazy ()
    count <- count + 1
    return count
  }

  assertEqual 0 count
  let resValue = res.Force()
  assertEqual count resValue
  assertEqual 3 count

[<Test>]
let ``ReturnFrom lazy()`` (): unit =
  let res = Do.lazily { return! lazy () }
  assertEqual () res.Value

[<Test>]
let ``Return int`` (): unit =
  let x = 42
  let res = Do.lazily { return x }
  assertEqual x res.Value

[<Test>]
let ``Lazy executes just once`` (): unit =
  let mutable count = 0
  let lz = lazy (count <- count + 1)
  
  (Do.lazily {
    do! lz
    do! lz
    do! lz
  }).Force()

  assertEqual 1 count

[<Test>]
let ``Do!`` (): unit =
  let mutable executed = false
  let lz = lazy (executed <- true)
  let res = Do.lazily { do! lz }
  Assert.False(lz.IsValueCreated)
  Assert.False(executed)
  assertEqual () res.Value
  Assert.True(lz.IsValueCreated)
  Assert.True(executed)

[<Test>]
let ``Bind`` (): unit =
  let res = Do.lazily {
    let! a = lazy 2
    let! b = lazy 3
    return a + b
  }
  assertEqual 5 res.Value

[<Test>]
let ``Bind for cheking delayed`` (): unit =
  let mutable count = 0
  let lazyIncr () = lazy (count <- count + 1)
  let res = Do.lazily {
    assertEqual 0 count
    do! lazyIncr()
    assertEqual 1 count
    do! lazyIncr()
    assertEqual 2 count
    do! lazyIncr()
    assertEqual 3 count
    return count
  }

  assertEqual 0 count

  assertEqual 3 res.Value

[<Test>]
let ``While false`` (): unit =
  let res = Do.lazily {
    while false do ()
    return ()
  }
  assertEqual () res.Value

[<Test>]
let ``While count`` (): unit =
  let res = Do.lazily {
    let mutable count = 10
    let lx = lazy 1
    while count > 0 do
      let! x = lx
      count <- count - x
    return count
  }
  assertEqual 0 res.Value

[<Test>]
let ``For`` (): unit =
  let res = Do.lazily {
    let mutable count = 0
    for i in 0..10 do
      count <- count + i
    return count
  }
  assertEqual 55 res.Value

[<Test>]
let ``Exception`` (): unit =
  let message = "Exception!"
  let e = Assert.Throws<MyException>(fun () ->
    Do.lazily {
      let! a = lazy 4
      let! b = lazy 5
      raise (MyException(message))
      return a + b
    } |> fun x -> x.Value |> ignore)

  assertEqual message e.Message

[<Test>]
let ``TryWith without Exception`` (): unit =
  let res = Do.lazily {
    try
      let! a = lazy 4
      let! b = lazy 5
      return a + b
    with e ->
      return 1
  }
  assertEqual 9 res.Value

[<Test>]
let ``TryWith catch`` (): unit =
  let res = Do.lazily {
    try
      let! a = lazy 4
      let! b = lazy 5
      failwith "Exception!"
      return a + b
    with e ->
      return 1
  }
  assertEqual 1 res.Value

[<Test>]
let ``TryWith reraise`` (): unit =
  let message = "Exception!"
  let e = Assert.Throws<MyException>(fun () ->
    Do.lazily {
    try
      let! a = lazy 4
      let! b = lazy 5
      raise (MyException(message))
      return a + b
    with e ->
      return e.Reraise()
    } |> fun x -> x.Value |> ignore)

  assertEqual message e.Message

[<Test>]
let ``TryFinally without Exception1`` (): unit =
  let mutable executed = false
  let res = Do.lazily {
    try
      do! lazy ()
    finally
      executed <- true
    return executed
  }
  assertEqual false executed
  assertEqual true res.Value

[<Test>]
let ``TryFinally without Exception2`` (): unit =
  let mutable executed = false
  let res = Do.lazily {
    try
      do! lazy ()
      return executed
    finally
      executed <- true
  }
  Assert.False executed
  assertEqual false res.Value
  Assert.True executed

[<Test>]
let ``TryFinally with Exception`` (): unit =
  let message = "Exception!"
  let mutable executed = false
  let mutable count = 0
  let res = Assert.Throws<MyException>(fun () ->
    Do.lazily {
      try
        let! _ = lazy 123
        raise (MyException message)
        count <- count + 1
        do! lazy (executed <- false)
      finally
        count <- count + 10
      return count
    } |> fun x -> x.Value |> ignore)
  Assert.False executed
  assertEqual message res.Message
  assertEqual 10 count

[<Test>]
let ``Use`` (): unit =
  let mutable executed = false
  let res = Do.lazily {
    use _d =
      { new System.IDisposable with
        member _.Dispose() = executed <- true }

    return 42
  }

  Assert.False executed
  assertEqual 42 res.Value
  Assert.True executed

[<Test>]
let ``Use!`` (): unit =
  let mutable executed = false
  let res = Do.lazily {
    use! _d = lazy (
      { new System.IDisposable with
        member _.Dispose() = executed <- true }
    )

    return 42
  }

  Assert.False executed
  assertEqual 42 res.Value
  Assert.True executed