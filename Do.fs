// The MIT License (MIT)
// Do.fs
// Copyright (c) 2022 wraikny
// All rights reserved.
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
// Do.fs contains somde code from dotnet/fsharp.
// https://github.com/dotnet/fsharp/blob/v12.0.5-beta.22427.1/tests/benchmarks/CompiledCodeBenchmarks/TaskPerf/TaskPerf/option.fs
//   The MIT License (MIT)
//   Copyright (c) Microsoft Corporation.
//   All rights reserved.
//   Permission is hereby granted, free of charge, to any person obtaining a copy
//   of this software and associated documentation files (the "Software"), to deal
//   in the Software without restriction, including without limitation the rights
//   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//   copies of the Software, and to permit persons to whom the Software is
//   furnished to do so, subject to the following conditions:
//   The above copyright notice and this permission notice shall be included in all
//   copies or substantial portions of the Software.
//   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//   SOFTWARE.

[<AutoOpen>]
module DoFs

[<RequireQualifiedAccess>]
module Builders =
  open System

  type Code<'a> = unit -> 'a

  type BuilderBase() =
    member inline _.Delay([<InlineIfLambda>] f: unit -> Code<'t>): Code<'t> =
      fun () -> (f())()

    member inline _.TryWith([<InlineIfLambda>] body: Code<'t>, [<InlineIfLambda>] catch: exn -> Code<'t>): Code<'t> =
      fun () ->
        try body()
        with exn -> (catch exn)()

    member inline _.TryFinally([<InlineIfLambda>] body: Code<'t>, [<InlineIfLambda>] compensation: unit -> unit): Code<'t> =
      fun () ->
        try body ()
        finally compensation ()

    member inline this.Using(disp: #IDisposable, [<InlineIfLambda>] body: #IDisposable -> Code<'t>): Code<'t> =
      // A using statement is just a try/finally with the finally block disposing if non-null.
      this.TryFinally(
        (fun () -> (body disp)()),
        (fun () -> if not (isNull (box disp)) then disp.Dispose()))

  type OptionCode<'t> = Code<'t voption>

  type OptionBuilderBase() =
    inherit BuilderBase()

    member inline _.Zero(): OptionCode<unit> = fun () -> ValueSome()

    member inline _.Bind(res1: 't1 option, [<InlineIfLambda>] task2: ('t1 -> OptionCode<'t2>)): OptionCode<'t2> =
      fun () ->
        match res1 with
        | None -> ValueNone
        | Some v -> (task2 v)()

    member inline _.Bind(res1: 't1 voption, [<InlineIfLambda>] task2: ('t1 -> OptionCode<'t2>)): OptionCode<'t2> =
      fun () ->
        match res1 with
        | ValueNone -> ValueNone
        | ValueSome v -> (task2 v)()

    member inline _.Bind(res1: Nullable<'t1>, [<InlineIfLambda>] task2: ('t1 -> OptionCode<'t2>)): OptionCode<'t2> =
      fun () ->
        if res1.HasValue then (task2 res1.Value)()
        else ValueNone

    member inline _.Bind(res1: Result<'t1, _>, [<InlineIfLambda>] task2: ('t1 -> OptionCode<'t2>)): OptionCode<'t2> =
      fun () ->
        match res1 with
        | Error _ -> ValueNone
        | Ok v -> (task2 v)()

    member inline _.Bind(res1: 't1, [<InlineIfLambda>] task2: ('t1 -> OptionCode<'t2>)): OptionCode<'t2> when 't : null =
      fun () ->
        match res1 with
        | null -> ValueNone
        | v -> (task2 v)()

    member inline this.Combine([<InlineIfLambda>] task1: OptionCode<unit>, [<InlineIfLambda>] task2: OptionCode<'t>) : OptionCode<'t> =
      this.Bind(task1(), fun () -> task2)

    member inline _.While([<InlineIfLambda>] condition : unit -> bool, [<InlineIfLambda>] body : OptionCode<unit>) : OptionCode<unit> =
      fun () ->
        let mutable proceed = true
        while proceed && condition() do
          match body () with
          | ValueNone -> proceed <- false
          | ValueSome () -> ()
        if proceed then ValueSome () else ValueNone

    member inline this.For(sequence: seq<'tElement>, [<InlineIfLambda>] body: 'tElement -> OptionCode<unit>): OptionCode<unit> =
      this.Using (sequence.GetEnumerator(),
        (fun e -> this.While((fun () -> e.MoveNext()), fun () -> (body e.Current)())))

    member inline _.Return (value: 't): OptionCode<'t> =
      fun () -> ValueSome value

    member inline _.ReturnFrom (source: 't option): OptionCode<'t> =
      fun () ->
        match source with
        | Some x -> ValueSome x
        | None -> ValueNone

    member inline _.ReturnFrom (source: voption<'t>): OptionCode<'t> =
      (fun () -> source)

  type OptionBuilder() =
    inherit OptionBuilderBase()

    member inline _.Run([<InlineIfLambda>] code: OptionCode<'t>): 't option =
      match code () with
      | ValueSome v -> Some v
      | ValueNone -> None

  type ValueOptionBuilder() =
    inherit OptionBuilderBase()

    member inline _.Run([<InlineIfLambda>] code: OptionCode<'t>): 't voption =
      code()

  type ResultCode<'t,'e> = Code<Result<'t, 'e>>

  type ResultBuilder() =
    inherit BuilderBase()

    member inline _.Zero(): ResultCode<unit, 'e> = fun () -> Ok()

    member inline _.Bind(res1: Result<'t1, 'e>, [<InlineIfLambda>] task2: ('t1 -> ResultCode<'t2, 'e>)): ResultCode<'t2, 'e> =
      fun () ->
        match res1 with
        | Error e -> Error e
        | Ok v -> (task2 v)()

    member inline this.Combine([<InlineIfLambda>] task1: ResultCode<unit, 'e>, [<InlineIfLambda>] task2: ResultCode<'t, 'e>) : ResultCode<'t, 'e> =
      this.Bind(task1(), fun () -> task2)

    member inline _.While([<InlineIfLambda>] condition : unit -> bool, [<InlineIfLambda>] body : ResultCode<unit, 'e>) : ResultCode<unit, 'e> =
      fun () ->
        let mutable error = ValueNone
        while error.IsNone && condition() do
          match body () with
          | Error e -> error <- ValueSome e
          | Ok () -> ()
        match error with
        | ValueNone -> Ok()
        | ValueSome e -> Error e

    member inline this.For(sequence: seq<'tElement>, [<InlineIfLambda>] body: 'tElement -> ResultCode<unit, 'e>): ResultCode<unit, 'e> =
      this.Using (sequence.GetEnumerator(),
        (fun e -> this.While((fun () -> e.MoveNext()), (fun () -> (body e.Current)()))))

    member inline _.Return (value: 't): ResultCode<'t, 'e> =
      fun () -> Ok value

    member inline _.ReturnFrom (source: Result<'t, 'e>): ResultCode<'t, 'e> =
      fun () -> source

    member inline _.Run([<InlineIfLambda>] code: ResultCode<'t, 'e>): Result<'t, 'e> =
      code ()

  type LazyCode<'t> = Code<Lazy<'t>>

  type LazyBuilder() =
    member inline _.Delay([<InlineIfLambda>] f: unit -> LazyCode<'t>): LazyCode<'t> =
      fun () -> lazy ((f())()).Value

    member inline _.Zero(): LazyCode<unit> = fun () -> (Lazy<_>.CreateFromValue())

    member inline _.Bind(res1: Lazy<'t1>, [<InlineIfLambda>] task2: ('t1 -> LazyCode<'t2>)): LazyCode<'t2> =
      fun () -> lazy (task2(res1.Value)()).Value

    member inline this.Combine([<InlineIfLambda>] task1: LazyCode<unit>, [<InlineIfLambda>] task2: LazyCode<'t>) : LazyCode<'t> =
      this.Bind(task1(), fun () -> task2)

    member inline _.While([<InlineIfLambda>] condition : unit -> bool, [<InlineIfLambda>] body : LazyCode<unit>) : LazyCode<unit> =
      fun () ->
        lazy
          while condition() do
            body().Force()

    member inline _.TryWith([<InlineIfLambda>] body: LazyCode<'t>, [<InlineIfLambda>] catch: exn -> LazyCode<'t>): LazyCode<'t> =
      fun () ->
        lazy
          try body().Force()
          with exn -> ((catch exn)()).Force()

    member inline _.TryFinally([<InlineIfLambda>] body: LazyCode<'t>, [<InlineIfLambda>] compensation: unit -> unit): LazyCode<'t> =
      fun () ->
        lazy
          try body().Force()
          finally compensation ()

    member inline this.Using(disp: #IDisposable, [<InlineIfLambda>] body: #IDisposable -> LazyCode<'t>): LazyCode<'t> = 
      // A using statement is just a try/finally with the finally block disposing if non-null.
      this.TryFinally(
        (fun () -> lazy ((body disp)()).Value),
        (fun () -> if not (isNull (box disp)) then disp.Dispose()))

    member inline this.For(sequence: seq<'tElement>, [<InlineIfLambda>] body: 'tElement -> LazyCode<unit>): LazyCode<unit> =
      this.Using (sequence.GetEnumerator(),
        (fun e -> this.While((fun () -> e.MoveNext()), (fun () -> lazy ((body e.Current)()).Value))))

    member inline _.Return (value: 't): LazyCode<'t> = fun () -> lazy value

    member inline _.ReturnFrom (source: Lazy<'t>): LazyCode<'t> = fun () -> source

    member inline _.Run([<InlineIfLambda>] code: LazyCode<'t>): Lazy<'t> =
      code ()

[<RequireQualifiedAccess>]
module Do =
  let option = Builders.OptionBuilder()

  let voption = Builders.ValueOptionBuilder()

  let result = Builders.ResultBuilder()

  let lazily = Builders.LazyBuilder()
