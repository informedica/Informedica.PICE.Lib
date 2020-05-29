
#r "System.Data.Linq"
#load "../../../.paket/load/net45/main.group.fsx"

open System
open System.IO

module Text =
        (*

        Copyright (c) Microsoft Corporation.

        Description:

            String manipulation helper functions.

        Author:

            William Blum (WiBlum) created 9/27/2012

        Revision history:
            Repackaged into FSharpLu on 2/18/2015

        *)


        /// File path comparer
        type CaseInsensitiveComparer() =
            interface System.Collections.Generic.IComparer<string> with
                member __.Compare(f1, f2) =
                    System.String.Compare(f1, f2, System.StringComparison.OrdinalIgnoreCase)

        /// Extension methods for String
        type System.String with

            /// Extend the string replace function to allow for StringComparison options to be specified
            member this.Replace (oldString:string, newString:string, comparisonType:System.StringComparison): string =
                let index = this.IndexOf(oldString, comparisonType)

                if index >= 0 then
                    this.Remove(index, oldString.Length).Replace(oldString, newString, comparisonType).Insert(index, newString)
                else
                    this

        /// Returns true if text starts with the specified prefix
        let startsWith prefix (text:System.String) =
            text.StartsWith prefix

        /// Returns true if text ends with the specified suffix
        let endWith prefix (text:System.String) =
            text.EndsWith prefix

        /// Remove count characters from the end of the specified string
        let chop count (text:System.String) =
            text.Remove(text.Length-count)

        /// Remove leading and trailing occurrences of a set of characters
        let trim chars (text:System.String) =
            text.Trim chars

        /// Remove trailing occurrences of a set of characters
        let trimEnd chars (text:System.String) =
            text.TrimEnd chars

        /// Remove leading occurrences of a set of characters
        let trimStart chars (text:System.String) =
            text.TrimStart chars

        /// Skip count number of characters from the specified string
        let skip (text:string) count =
            text.Substring(count, text.Length-count)

        /// Remove a prefix from the specified string
        let skipPrefix prefix (text:string) =
            if text.StartsWith prefix then
                skip text prefix.Length
            else
                text

        /// Remove a suffix from the specified string
        let removeSuffix suffix (text:string) =
            if text.EndsWith suffix then
                chop suffix.Length text
            else
                text

        /// Remove a prefix from the specified string case insensitively
        let skipPrefixCaseInsensitive prefix (text:string) =
            if text.StartsWith(prefix, System.StringComparison.OrdinalIgnoreCase) then
                skip text prefix.Length
            else
                text

        /// Remove a suffix case insensitively (used on file paths)
        let removeSuffixCaseInsensitive suffix (text:string) =
            if text.EndsWith(suffix, System.StringComparison.OrdinalIgnoreCase) then
                chop suffix.Length text
            else
                text

        /// Remove part following the the first occurrence of a given string
        let removeAfter marker (text:string) =
            let markPosition = text.IndexOf(marker, System.StringComparison.OrdinalIgnoreCase)
            if markPosition >= 0 then
                text.Remove markPosition
            else
                text

        /// Return the right n-most characters from the string
        /// where n is smaller than the length of the string.
        let right n (text:string) =
            text.Substring(text.Length-n,n)

        /// Split a string based on the specified array of character separators
        let split charSeparators (text:string) =
            text.Split charSeparators

        /// Split a string on a string separator
        let splitOnString (stringSeparators:string[]) (text:string) =
            text.Split(stringSeparators, System.StringSplitOptions.RemoveEmptyEntries)

        /// Split a string based on the specified array of character separators
        let splitNoEmptyEntries (charSeparators:char[]) (text:string) =
            text.Split(charSeparators, System.StringSplitOptions.RemoveEmptyEntries)

        /// Strip trailing and prefix character
        let stripQuotes =
            skipPrefix "\""
            >> removeSuffix "\""

        /// Split a string at the first occurrence of a character
        let splitOnce (charSep:char) (text:string) =
            let pos = text.IndexOf(charSep)
            if pos < 0 then
                invalidArg "text" "Separator not present in the string"
            else
                text.Substring(0, pos), text.Substring(pos+1)

        /// Join a sequence of strings
        let join separator (values:seq<string>) =
            System.String.Join(separator,values)

        /// Truncate a string to a maximum number of characters
        let truncate max (text:string) =
            let length = text.Length
            if length <= max then
                text
            else
                text.Substring(0,max)

        /// longest common prefix of two strings
        let longestCommonPrefixLength (s1:string) (s2:string) =
            let chop = Seq.map2 (<>) s1 s2
            match Seq.tryFindIndex id chop with
            | None -> min (s1.Length) (s2.Length)
            | Some i -> i

        /// Indent lines in a text
        let indent count =
            let prefix = System.String(' ', count)
            splitOnString [|System.Environment.NewLine|]
            >> Seq.map (fun line -> prefix + line)
            >> join System.Environment.NewLine

        /// Encode a string to Base64
        let encodeToBase64 (toEncode:string) =
            toEncode |> System.Text.ASCIIEncoding.UTF8.GetBytes |> System.Convert.ToBase64String

        /// Decode a Base64 encoded string
        let decodeFromBase64 (base64Encoded:byte[]) =
            let decodedString = System.Text.Encoding.UTF8.GetString(base64Encoded)
            System.Convert.FromBase64String(decodedString)

        /////// Implementation of Knuth–Morris–Pratt on Stream

        /// Used by kmpTryFindBytesInStream below to compute the backtrack array
        let computeKmpBacktrack (searchBytes: uint8[]) =
            let backtrack = Array.zeroCreate (searchBytes.Length+1)
            let rec back b j =
                if j >= 0 && b <> searchBytes.[j] then
                    back b backtrack.[j]
                else
                    j

            let rec compute i j =
                if j < searchBytes.Length then
                    if searchBytes.[j] = searchBytes.[i] then
                        backtrack.[j] <- backtrack.[i]
                        compute (i+1) (j+1)
                    else
                        backtrack.[j] <- i
                        let k = back searchBytes.[j] backtrack.[i]
                        compute (k+1) (j+1)
                else
                    backtrack.[j] <- i

            backtrack.[0] <- -1
            compute 0 1
            backtrack

        /// Options for fmdFindBytesInStream
        type FindOptions =
            /// Return after finding the first occurence of bytes in the stream
            | FindFirst
            /// Return after finding all occurences of bytes in the stream
            | FindAll

        /// Use the Knuth–Morris–Pratt algorithm to search for first or all occurrences of a byte sequence in a stream
        /// returns a list of positions of the occurence of bytes in the stream, or None if the bytes could not be found
        let kmpFindBytesInStream (findOptions:FindOptions) (stream:System.IO.Stream) (searchBytes:uint8[]) =
            let backtrack = computeKmpBacktrack searchBytes
            let mutable k = 0
            let mutable byteRead = stream.ReadByte()
            let mutable results = []
            while byteRead <> -1 && (List.isEmpty results || findOptions = FindAll) do
                if searchBytes.[k] = (byteRead |> uint8) then
                    k <- k + 1
                    if k = searchBytes.Length then
                        results <- (stream.Position - (k|>int64)) :: results
                        k <- backtrack.[k]
                    byteRead <- stream.ReadByte()
                else
                    k <- backtrack.[k]
                    if k < 0 then
                        k <- k + 1
                        byteRead <- stream.ReadByte()

            List.rev results

        /// Use the Knuth–Morris–Pratt algorithm to search for the occurrence of a byte sequence in a stream
        /// returns the position of the first occurrence of the bytes in the stream, or None if the bytes could not be found
        let kmpTryFindFirstBytesInStream (stream:System.IO.Stream) (searchBytes:uint8[]) =
            List.tryHead (kmpFindBytesInStream FindFirst stream searchBytes)

        /// Search for the occurrence of a byte sequence in a file without loading the entire file into memory to do it
        let fileContainsBytes (filePath:string) (searchBytes:uint8[]) =
            use fileStream = System.IO.File.Open (filePath, System.IO.FileMode.Open, System.IO.FileAccess.Read, System.IO.FileShare.Read)
            (kmpTryFindFirstBytesInStream fileStream searchBytes).IsSome


module Option =
    (*

    Copyright (c) Microsoft Corporation.

    Description:

        Option type helpers

    Author:

        William Blum (WiBlum) created 9/27/2012

    Revision history:
        Repackaged into FSharpLu on 2/18/2015

    *)


    /// Branching combinator for function returning value of type 'a option
    let public (|-) f g =
        match f with
        | None -> g()
        | v -> v

    /// Dual branching combinator for function returning value of type 'a option
    let public (|+) f g =
        match f with
        | None -> None
        | Some v -> g v

    /// Convert a result to option type according to a boolean success code
    let public ofCurriedPair result success =
        if success then Some result else None

    /// Convert a success*result pair to a Some type
    let public ofPair (success, result) =
        ofCurriedPair result success

    /// Inner-join an element with a function returning an option type.
    /// Apply a function to an element and return a pair of the element and the function result
    /// or None if the function returns nothing.
    let public mapjoin e f =
        match f e with
        | None -> None
        | Some a -> Some(e, a)

    /// Convert a nullable type to a Some type
    let public ofNullable (a:System.Nullable<'T>) =
        if a.HasValue then
            Some(a.Value)
        else
            None

    /// Pattern-matching on option (equivalent to F# match)
    let _match none some =
        function
        | None -> none()
        | Some v -> some v

    /// Pattern-matching on option (equivalent to F# match)
    let mapOrDefault f defaultValue =
        function
        | None -> defaultValue
        | Some v -> f v

    /// Provide default value for a None optional argument
    let public orDefault defaultValue x =
        defaultArg x defaultValue

    /// Return the value of a some type or raise an exception if it is None
    let public orRaise anException =
        function
        | None -> raise anException
        | Some v -> v

    /// Return the value of a some type or if it is None execute the given default operation
    let public orDo (defaultOrFailure:unit -> 'T) =
        function
        | None -> defaultOrFailure()
        | Some v -> v

    // Try f then g. (f >-> g) x evaluates to match f x with None -> g x | v -> v
    let (>->) f g x =
        match f x with
        | None -> g x
        | v -> v

    /// Convert string to option type where null and empty string become None.
    let ofString s =
        if System.String.IsNullOrEmpty s then
            None
        else
            Some s

    /// Convert .net object to option type
    let fromObject x =
        if obj.ReferenceEquals(x, null) then None else Some x

    /// Make sure an option value does not host a null by replacing Some null by None.
    let fromNull x =
        Option.bind fromObject x

    /// Convert a .net Nullable object to option type
    let fromNullable (o:System.Nullable<'t>) =
        if o.HasValue then
            Some o.Value
        else
            None

    /// The Maybe monad
    type MaybeMonad() =
        member __.Return(v) = Some v
        member __.ReturnFrom(v) = v
        member __.Bind(p, f) = match p with None -> None | Some v -> f v
        member __.Delay(f) = f
        member __.Combine(first, fallback) = if Option.isSome first then first else fallback()
        member __.Run(f) = f()

    let public maybe = MaybeMonad()


module Async =

    open System
    open System.Threading
    open System.Threading.Tasks

    /// Extension methods to work with .Net Tasks
    type System.Threading.Tasks.Task with
        member x.AsAsync
            with get () = Async.AwaitTask(x)

    /// Extension methods to work with generic .Net Task<T>
    type System.Threading.Tasks.Task<'T> with
        member x.AsAsync
            with get () = Async.AwaitTask(x)

    /// Reraise an exception from a `catch` block of an async exception handler
    /// while preserving all the original exception stack trace.
    /// Requires .NET 4.5 (ExceptionDispatchInfo)
    let inline reraise e =
        System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(e).Throw()
        raise <| System.InvalidProgramException() // Unreachable, used only to match any generic return type

    let inline RunCatchSynchronously workflow =
        workflow
        |> Async.Catch
        |> Async.RunSynchronously
        |> function
        | Choice1Of2 result -> result
        | Choice2Of2 exn -> reraise exn

    /// Bind operator for Async computation
    let bind f asyncOp =
        async.Bind(asyncOp, f)

    /// Map operator for Async computation
    let map f asyncOp =
        bind (f >> async.Return) asyncOp

    /// Start multiple async-workflows concurrently
    /// and return a new async that waits for the first async to return.
    /// After one async has returned all the remaining ones are cancelled,
    /// and the workflow may return before the cancellations of the remaining workflows complete.
    let Compete workflows =
        async {
            use loosers = new System.Threading.CancellationTokenSource()

            // The call to loosers.Cancel in the finally block is not sufficient.
            // Doing it from OnCancel() guarantees that the children
            // workflows are properly terminated if the parent workflow is cancelled.
            use! c = Async.OnCancel(fun() -> loosers.Cancel())

            try
                let! winningTask =
                    workflows
                    |> Seq.map (fun w -> Async.StartAsTask(w, cancellationToken=loosers.Token))
                    |> Task.WhenAny
                    |> Async.AwaitTask
                return winningTask.Result
            finally
                // cancell all other tasks
                loosers.Cancel()
        }

    /// Async.Compete between an Async<'A> and a Task<'B>.
    ///
    /// Create an asynchronous workflow that concurrently runs an async workflow and
    /// a .Net Task and returns a discriminating union of three possible outcomes depending on
    /// which computation finished first or if they finished simultaneously.
    /// When one of the computation wins, the other 'loosing' computation
    /// is cancelled synchronoulsy.
    let CompeteWithTask<'A, 'B> (workflow:Async<'A>) (taskBuilder:CancellationTokenSource -> Task<'B>) =
        async {
            use looser = new System.Threading.CancellationTokenSource()

            // The call to looser.Cancel in the finally block is not sufficient.
            // Doing it from OnCancel guarantees that the children
            // task/workflow are properly terminated if the parent workflow is cancelled.
            use! c = Async.OnCancel(fun() -> looser.Cancel())

            let t1 = Async.StartAsTask(workflow, cancellationToken=looser.Token)
            let t2 = taskBuilder looser
            try
                let! competition = Tasks.Task.WhenAny [| t1:> Task; t2:> Task |] |> Async.AwaitTask
                ()
            finally
                looser.Cancel()

            // Wait for the looser task cancellation to complete (a TaskCanceledException exception will be triggered when this happens)
            do! async {
                try
                    if not t1.IsCompleted then
                        let! _ = Async.AwaitTask t1 in ()
                    elif not t2.IsCompleted then
                        let! _ = Async.AwaitTask t2 in ()
                with
                :? System.AggregateException as e ->
                    if e.InnerExceptions |> Seq.exists (function e -> e :? TaskCanceledException) then
                        raise e
            }
            return
                match t1.IsCompleted && not t1.IsCanceled, t2.IsCompleted && not t2.IsCanceled with
                | true, false -> Choice1Of3 (t1.Result)
                | false, true -> Choice2Of3 t2.Result
                | true, true -> Choice3Of3 (t1.Result, t2.Result)
                | false, false -> invalidOp "Both competing tasks failed to complete."
        }

    /// Return an object that when disposed automatically calls the object's Release() method
    let inline private releaseOnDispose (eventObject: ^T) =
        { new System.IDisposable with
            member __.Dispose() =
                (^T : (member Release : unit -> ^R) (eventObject)) |> ignore
        }

    /// Create an asynchronous workflow that concurrently runs an async workflow and
    /// tries to acquire a given .Net threading object (e.g. SlimSemaphore, ...).
    /// It returns a discriminating union representing the task that finished first. The other one
    /// (either the workflow or the threading object) is properly terminated and disposed.
    let inline CompeteWithThreadingObject<'A, ^R, ^T when ^T: (member Release : unit -> ^R)
                                                      and ^T: (member WaitAsync : int -> CancellationToken -> Task<bool>)>
                (workflow:Async<'A>) (threadingObject: ^T) =
        async {
            let! r = CompeteWithTask workflow (fun source -> (^T: (member WaitAsync : int -> CancellationToken -> Task<bool>)(threadingObject, -1, source.Token)))
            return match r with
                   | Choice1Of3 x -> Choice1Of3 x
                   | Choice2Of3 true -> Choice2Of3 (releaseOnDispose threadingObject)
                   | Choice3Of3 (x,true) -> Choice3Of3 (x, releaseOnDispose threadingObject)
                   | Choice2Of3 false
                   | Choice3Of3 (_,false) -> assert false; invalidOp "Both competing tasks failed to complete."
        }


    /// Execute an asynchronous computation until it succeeds or the specified timeout expires.
    let retry (timeout:TimeSpan, retryDelay:TimeSpan, f:unit -> Async<'T>) =
        let beginPollTime = DateTime.UtcNow
        let endPollTime = beginPollTime + timeout

        let rec loop () =
            async {
                try
                    return! f()
                with
                | e when DateTime.UtcNow <= endPollTime ->
//                    Trace.info "Exception in retry loop (will retry in %ds): %s" (int retryDelay.TotalSeconds) e.Message
                    do! Async.Sleep(int retryDelay.TotalMilliseconds)
                    return! loop()
            }
        loop ()

    /// Perform an asynchronous computation until either it succeedes, an unexpected exception occurs, or the specified timeout expires.
    /// Until the timeout expires, any exception thrown meeting the exception filter condition will not be thrown.
    /// After the timeout expires any exception will be thrown, regardless of whether the filtering condition is met.
    let retryOnSpecificFailures (timeout:TimeSpan, retryDelay:TimeSpan, f:unit -> Async<'T>, exceptionFilter: Exception -> bool) =
        let beginPollTime = DateTime.UtcNow
        let endPollTime = beginPollTime + timeout
        let rec loop () =
            async {
                try
                    return! f()
                with
                | e when exceptionFilter e && DateTime.UtcNow <= endPollTime ->
                    do! Async.Sleep(int retryDelay.TotalMilliseconds)
                    return! loop()
            }
        loop ()

    /// Execute an asynchronous computation until it returns something, an unexpected exception occurs or the specified timeout expires.
    let retryUntilSome (timeout:TimeSpan) (retryDelay:TimeSpan) (f:unit -> Async<'T option>) =
        let retryDelay = if retryDelay > timeout then timeout else retryDelay

        let beginPollTime = DateTime.UtcNow
        let endPollTime = beginPollTime + timeout
        let rec loop () =
            async {
                let! r = f()
                match r with
                | Some v ->
                    return v
                | None when DateTime.UtcNow <= endPollTime ->
                    do! Async.Sleep(int retryDelay.TotalMilliseconds)
                    return! loop()
                | None ->
                    return raise <| System.TimeoutException()
            }
        loop ()

    /// Execute an asynchronous computation until it returns something. Returns None if an unexpected exception occurs or the specified timeout expires.
    let retryUntilSomeOrTimeout (timeout:TimeSpan) (retryDelay:TimeSpan) (f:unit -> Async<'T option>) =
        let retryDelay = if retryDelay > timeout then timeout else retryDelay

        let beginPollTime = DateTime.UtcNow
        let endPollTime = beginPollTime + timeout
        let rec loop () =
            async {
                let! r = f()
                match r with
                | None when DateTime.UtcNow <= endPollTime ->
                    do! Async.Sleep(int retryDelay.TotalMilliseconds)
                    return! loop()
                | Some _ | None ->
                    return r
            }
        loop ()

    /// Execute an asynchronous computation until it returns true,
    // an unexpected exception occurs or the specified timeout expires.
    let retryUntilTrueOrTimeout (timeout:TimeSpan) (retryDelay:TimeSpan) f =
        async {
            let! result =
                retryUntilSomeOrTimeout timeout retryDelay
                    (fun () ->
                        async {
                            let! r = f ()
                            match r with
                            | true -> return Some ()
                            | false -> return None
                        })

            return result.IsSome
        }

    /////////////
    ///// Operation on sequences of Async (seq<Async<t>)

    /// Returns an async expression that sequentially asynchronously evaluates a given sequence of asynchronous computation
    /// and returns the list of results from the evaluated synchronous computation.
    /// (i.e. it's the counterpart of Async.Parallel from F# standard library for sequential evaluation.)
    let sequentialCombine (asyncSeq:seq<Async<'t>>) : Async<list<'t>> =
        let rec mapAux prefix (s:System.Collections.Generic.IEnumerator<_>) =
            async {
                if s.MoveNext() then
                    let! y = s.Current
                    return! mapAux (prefix@[y]) s
                else
                    return prefix
            }
        mapAux [] (asyncSeq.GetEnumerator())

    /// Create an async computation that evaluates a sequence of async and return a list of the corresponding results
    /// Alternative implementations of sequentialCombine that relies on cons instead of list concatenation
    let sequential (s:seq<Async<'t>>) : Async<list<'t>> =
        Seq.foldBack
            (fun aHead aQueue -> async.Bind(aHead, fun head -> async.Bind(aQueue, fun queue -> async.Return(head::queue))))
            s
            (async.Return [])

    /// Returns an async expression that evaluates in parallel a given sequence of asynchronous computation
    /// and returns the list of results from the evaluated synchronous computation.
    /// (i.e. same as Async.Parallel but returns a list instead of an array)
    let parallelCombine (asyncSeq:seq<Async<_>>) : Async<list<'t>> =
        async {
            let! array = Async.Parallel asyncSeq
            return Array.toList array
        }

    /// Apply a function to each element of a sequence of asyncs
    let seqMap (f:'t -> 'u) (asyncSeq:seq<Async<'t>>) =
        Seq.map (map f) asyncSeq

    /// Returns an async expression that sequentially asynchronously evaluates a given sequence of asynchronous computation of unit type
    let seqIter (asyncSeq:seq<Async<_>>) =
        async {
            for a in asyncSeq do
                do! a
        }



    module Synchronization =

        /// Interface for a pool-based synchronization object
        type IPool =
            interface
                abstract InternalSemaphore : SemaphoreSlim
                abstract AcquireAsync : int option -> Async<System.IDisposable>
                abstract TryAcquireAsync : int option -> Async<System.IDisposable option>
            end

        /// Synchronization object used to limit the total
        /// number of requests that can be granted concurrently.
        /// Usage:
        ///   let pool = new Pool(5)
        ///   ...
        ///   async {
        ////    use! token = pool.AcquireAsync()
        ///        ...
        ///   }
        type Pool(size:int) =
            let semaphore = new SemaphoreSlim(initialCount=size, maxCount=size)

            interface IDisposable with
                member __.Dispose() =
                    semaphore.Dispose()

            interface IPool with
                /// Returns the internal semaphore object
                member __.InternalSemaphore with get() = semaphore

                /// Wait until a token from the pool becomes available and acquire it
                /// Return an object that automatically releases the token to the pool when disposed.
                member __.AcquireAsync(?timeout) =
                    async {
                        let! token = Async.CancellationToken
                        let! ok = semaphore.WaitAsync(defaultArg timeout -1, token) |> Async.AwaitTask
                        if ok then
                            return releaseOnDispose semaphore
                        else
                            return failwith "Could not acquire a token from the pool within allocated time"
                    }

                /// Try acquiring a token from the pool.
                /// On success returns an object that automatically releases the token
                /// once disposed. Returns None on failure to acquire the token.
                member __.TryAcquireAsync(?timeout) =
                    async {
                        let! token = Async.CancellationToken
                        let! entered = semaphore.WaitAsync(defaultArg timeout 0, token) |> Async.AwaitTask
                        if entered then
                            return releaseOnDispose semaphore |> Some
                        else
                            return None
                    }

        /// Nested Async pool. Behaves like pool but acquires a resource from a parent pool
        /// before acquring the token from this pool.
        type NestedPool(size:int, parent:IPool) =
            let pool = new Pool(size=size) :> IPool

            interface IDisposable with
                member __.Dispose() =
                    (pool :?> IDisposable).Dispose()

            interface IPool with
                /// Returns the internal semaphore object
                member __.InternalSemaphore with get() = pool.InternalSemaphore

                /// Wait until a token from the parent pool and this pool become available and acquire them.
                /// Return an object that automatically releases the tokens when disposed.
                member __.AcquireAsync(?timeout) =
                    async {
                        let! parent = parent.AcquireAsync(timeout)
                        let! this = pool.AcquireAsync(timeout)
                        return
                            { new System.IDisposable with
                                 member __.Dispose() =
                                    this.Dispose()
                                    parent.Dispose()
                             }
                    }

                /// Try acquiring a token from the parent pool and this pool.
                /// On success returns an object that automatically releases the tokens
                /// once disposed. Returns None on failure to acquire a token from the parent
                /// or from this pool.
                member __.TryAcquireAsync(?timeout) =
                    async {
                        let! parent = parent.TryAcquireAsync(timeout)
                        match parent with
                        | None -> return None
                        | Some parentToken ->
                            let! thisToken = pool.TryAcquireAsync(timeout)
                            match thisToken with
                            | None -> parentToken.Dispose()
                                      return None
                            | Some token ->
                                return Some
                                    { new System.IDisposable with
                                         member x.Dispose() =
                                            token.Dispose()
                                            parentToken.Dispose()
                                     }
                    }


        /// Single-use event object that can be waited on asynchronoulsy
        type SingleUseEvent() =
            let semaphore = new SemaphoreSlim(initialCount=0, maxCount=1)

            interface IDisposable with
                member __.Dispose() =
                    semaphore.Dispose()

            // Signal the event
            member __.Fire() =
                semaphore.Release() |> ignore

            // Wait for the event to occur
            member __.WaitAsync(?timeout) =
                async {
                    let! token = Async.CancellationToken
                    let! ok = semaphore.WaitAsync((defaultArg timeout -1), token) |> Async.AwaitTask
                    if ok then
                        semaphore.Release() |> ignore
                        return ()
                    else
                        return failwith "Wait on SingleUseEvent timed-out."
                }

         /// Asynchronous critical section
         type CriticalSection() =
            inherit Pool(1)
            member x.CriticalBlock(f:unit->'A, ?blockName) =
                let description = match blockName with None -> "" | Some name -> " (" + name + ")"
                async {
                    //printfn "Entering critical section%s" description
                    use! block = (x:> IPool).AcquireAsync None
                    //printfn "Critical section entered%s" description
                    let ret = f()
                    //printfn "Leaving critical section%s" description
                    return ret
                }

            member x.CriticalAsyncBlock(task:Async<'A>, ?blockName) =
                let description = match blockName with None -> "" | Some name -> " (" + name + ")"
                async {
                    use! block = (x:> IPool).AcquireAsync None
                    return! task
                }

    /// Asynchronous file copy
    let copyFile source target =
        async {
            use sourceStream = System.IO.File.Open(source, System.IO.FileMode.Open, System.IO.FileAccess.Read, System.IO.FileShare.Read)
            use targetStream = System.IO.File.Open(target, System.IO.FileMode.Create, System.IO.FileAccess.Write)
            let task = sourceStream.CopyToAsync(targetStream)
            return! task |> Async.AwaitTask
        }


module File =

    open System.IO

    /// Path combine infix operator
    let (++) x y = System.IO.Path.Combine(x,y)

    /// Ensure specified file exists
    let public getExistingFile path =
        if System.IO.File.Exists path then
            Some path
        else
            None

    /// Ensure specified directory exists
    let public getExistingDir path =
        if System.IO.Directory.Exists path then
            Some path
        else
            None

    /// Return the size in bytes of a file
    let fileLength filePath =
        let f = System.IO.FileInfo(filePath)
        f.Length

    /// Append a line to a text file
    let public appendLine filepath line =
        System.IO.File.AppendAllLines(filepath, [line])

    /// Append lines to a text file
    let public appendLines filepath (lines:seq<string>) =
        System.IO.File.AppendAllLines(filepath, lines)

    /// Write lines to a text file
    let public writeLines filepath (lines:seq<string>) =
        System.IO.File.WriteAllLines(filepath, lines)

    /// Create an empty file
    let public createEmptyFile filepath =
        (
            use file = System.IO.File.Create(filepath)
            ()
        )

    /// Write to a text file atomically while allowing concurrent reads.
    /// **Atomicity is guaranteed only if file content is < 8kb**
    ///
    // NOTE: An implementation based on System.IO.File.Replace
    // would not guarantee atomicity for files residing on SMB shares!
    // (http://msdn.microsoft.com/en-us/library/windows/desktop/aa365512(v=vs.85).aspx)
    let public atomaticWriteAllLines filePath lines =
        /// Replace content of an existing file atomically using the
        /// whitespace padding hack.
        let replaceExistingContentWithPaddingHack () =
            // CAUTION: Atomicity breaks if bufferSize < |content to be written|
            let BufferSize = 8192

            // Should not use FileMode.Create otherwise the file will be empty until the next flush.
            use fs = new FileStream(filePath, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.Read, BufferSize, FileOptions.WriteThrough)
            use streamWriter = new StreamWriter(fs)
            streamWriter.AutoFlush <- false
            let newContent = Text.join streamWriter.NewLine lines

            // If new content is smaller than previous content
            // then we pad the content with spaces to
            // prevent concurrent readers to see inconsistent content
            // after we flushed and before the file is closed.
            let oldLength = fs.Length |> int64
            streamWriter.Write(newContent)
            let newLength = newContent.Length |> int64
            let diff = oldLength - newLength
            if diff > 0L then
                streamWriter.Write(streamWriter.NewLine)
                streamWriter.Write(Array.create(diff |> int) ' ')
            streamWriter.Flush()
            // Trim the extra padding
            fs.SetLength(newLength)
            if newLength > int64 BufferSize then
                failwithf "File too big to guarantee atomicity: %d bytes. Maximum supported size is %d" newLength BufferSize

        // Write content to a temp file in the target directory
        let writeToTempFile () =
            let targetDir = System.IO.Path.GetDirectoryName(filePath)
            let tempFileName = System.Guid.NewGuid().ToString().Replace("-","")
            let tempFile = targetDir ++ tempFileName
            System.IO.File.WriteAllLines(tempFile, lines |> Seq.toArray)
            tempFile

        // Logic differs depending on whether the file exists.
        if System.IO.File.Exists filePath then
            replaceExistingContentWithPaddingHack ()
        else
            // If the file does not exists then the previous logic does not work:
            // creating the file stream will leave the file empty until the file is flushed!
            // Instead we write to a separate file and then atomically rename it.
            let tempFile = writeToTempFile()
            try
                System.IO.File.Move(tempFile, filePath)
            with
                :? System.IO.IOException ->
                    if System.IO.File.Exists filePath then
                        // the target file has just been created by
                        // another process: let the other process win
                        System.IO.File.Delete tempFile
                    else
                        reraise()

    /// Read a text file atomically while allowing concurrent writes.
    /// Atomicity is guaranteed only if file content is < 8kb
    let public atomaticReadAllText filePath =
        let bufferSize = 8192 // CAUTION: if < content to be written then atomicity breaks
        use fs = new FileStream(filePath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite, bufferSize, FileOptions.SequentialScan)
        use streamReader = new StreamReader(fs)
        streamReader.ReadToEnd()

    /// Read a text file atomically while allowing concurrent writes.
    /// Returns result as array of lines
    let public atomaticReadLines filePath =
        atomaticReadAllText filePath |> Text.splitOnString [|"\r\n"|]

    /// Parse any batch file containing variable definitions of the following format
    ///   rem Some comment
    ///   set VAR=somevalue
    ///   set VAR2=some other value
    let public parseBatchSetFile file =
        file
        |> atomaticReadLines
        |> Seq.filter (Text.startsWith "::" >> not)
        |> Seq.filter (Text.startsWith "rem" >> not)
        |> Seq.filter (Text.startsWith "REM" >> not)
        |> Seq.filter (System.String.IsNullOrWhiteSpace >> not)
        |> Seq.map (Text.skipPrefixCaseInsensitive "set " >> Text.splitOnce '=')
        |> Seq.toList

    /// Serialize a sequence of key/value pairs to batch file
    let public serializeToBatch filepath keypairs =
        keypairs |> Seq.map (fun (k,v) -> sprintf "set %s=%O" k v)
        |> atomaticWriteAllLines filepath

    /// Wait until the specified file exists on disk.
    /// Note: the immediate parent directory must already exist
    let waitUntilExists filepath =
        async {
            if System.IO.File.Exists filepath then
//                log.write "File %s already exists" filepath
                return filepath
            else
                let parentDir = System.IO.Path.GetDirectoryName filepath
                let fileName = System.IO.Path.GetFileName filepath
                use w = new System.IO.FileSystemWatcher(parentDir, fileName, IncludeSubdirectories = false, EnableRaisingEvents = true)

                let waitForFileAsync =
                    Async.Compete
                        [
                            async {
                                let! v = Async.AwaitEvent w.Created
                                return v.FullPath
                            }
                            async {
                                let! v = Async.AwaitEvent w.Renamed
                                return v.FullPath
                            }
                        ]

                // (a) Potential race condition if the file is created here,
                // taken care of by (b).

                let! waitForFile = Async.StartChild(waitForFileAsync)

                /// (b) Check again to handle race condition (a)
                if System.IO.File.Exists filepath then
                    return filepath
                else
//                    log.write "awaiting for %s" filepath
                    return! waitForFile
        }

    /// Deletes the directory if it exists.
    let deleteDirIfExists dir =
        if Directory.Exists(dir) then
            Directory.Delete(dir, true)

    /// Check if the target directory exists, if not, create it.
    let createDirIfNotExists dir =
        if not <| Directory.Exists(dir) then
            Directory.CreateDirectory(dir) |> ignore

    /// Create the target directory exists, deleting it first if it already exists.
    let recreateDir dir =
        if Directory.Exists(dir) then
            Directory.Delete(dir, true)
        Directory.CreateDirectory(dir) |> ignore

    /// Directory copy (ported from MSDN https://msdn.microsoft.com/en-us/library/system.io.directoryinfo.aspx)
    /// Usage:
    ///
    ///   CopyDirectory (DirectoryInfo(sourceDirectory)) (DirectoryInfo(targetDirectory))
    ///
    let copyDirectory sourcePath targetPath =
        let rec aux (source:DirectoryInfo) (target:DirectoryInfo) =
            if System.String.Compare(source.FullName, target.FullName, true) <> 0 then
                createDirIfNotExists target.FullName

                // Copy each file into it's new directory.
                source.GetFiles()
                |> Seq.iter
                    (fun fi ->
                        // printf @"Copying %s\%s" target.FullName fi.Name
                        fi.CopyTo(Path.Combine(target.ToString(), fi.Name), true) |> ignore)

                // Copy each subdirectory using recursion.
                source.GetDirectories()
                |> Seq.iter(fun diSourceSubDir ->
                                let nextTargetSubDir = target.CreateSubdirectory(diSourceSubDir.Name)
                                aux diSourceSubDir nextTargetSubDir)

        in aux (DirectoryInfo(sourcePath)) (DirectoryInfo(targetPath))


    /// A string that represents a valid file path.
    type FilePath = string

    /// Validates that the argument is a valid path on the current platform.
    /// The validation does not require the file to exist.
    let validateFilePath (pathAsString:string) =
        // Use FileInfo to check that the path is valid.
        // FileInfo performs validity checks on the path for the current platform, but
        // does not check for the existence of the file.
        if pathAsString.IndexOfAny(Path.GetInvalidPathChars()) >= 0 then
            None
        else
            Some pathAsString

    let contains textToSearchFor (text:string) =
        text.IndexOf(textToSearchFor, System.StringComparison.InvariantCultureIgnoreCase) > -1

    let findMatchingLines (file:string) textToSearchFor =
        System.IO.File.ReadAllLines(file)
        |> Seq.filter (contains textToSearchFor)

    /// Settings are expected to be in the form "key=value"
    /// Return exactly one setting.
    let getIniValue (file:string) textToSearchFor =
        let line =
            findMatchingLines file textToSearchFor
            |> Seq.exactlyOne
        let splits = line.Split('=')
        splits.[1]

    /// This is standard Windows HRESULT for file already exists
    module private Constants =
        let [<Literal>] FILE_ALREADY_EXISTS = 0x80070050

    /// Create a new file if it does not already exist and atomically write to it using the specified FileStream function.
    /// If the file already exists then do nothing and return None.
    let asyncTryCreateFile filePath (f: System.IO.FileStream -> Async<'a>) =
        async {
            if System.IO.File.Exists(filePath) then
                return None
            else
                let fs =
                    try
                        Some (new System.IO.FileStream(filePath, FileMode.CreateNew, FileAccess.ReadWrite, FileShare.None))
                    with
                    | :? System.IO.IOException as ex when ex.HResult = Constants.FILE_ALREADY_EXISTS -> None

                match fs with
                | Some fileStream ->
                    use stream = fileStream
                    let! result = f stream
                    return Some result
                | None ->
                    return None
        }


module Configuration =
    open System.Configuration
    open System.Reflection
    open System.Runtime.CompilerServices

    /// Read a configuration value.
    /// Default implementation: read from the application configuration manager
    let mutable public tryGetConfigValue =
        fun (key:string) ->
            match System.Configuration.ConfigurationManager.AppSettings.Get(key) with
            | null -> None
            | v -> Some v

    /// Read a configuration value
    let public getConfigValue key =
        tryGetConfigValue key |> Option.orDo (fun () -> invalidOp (sprintf "Configuration key %s missing from config file" key))

    /// Set a configuration value
    let mutable public setConfigValue =
        fun key value ->
            System.Configuration.ConfigurationManager.AppSettings.Set(key, value)

    /// Get an array type value from the configuration file
    let public getConfigArray name =
        getConfigValue name
        |> Text.splitNoEmptyEntries [|';'; '\t'; '\n'; '\r'|]
        |> Array.map (Text.trim [|' '|])

    /// Use a user-specified .config file
    let public loadCustomConfig filePath =
        let configFileMap = ExeConfigurationFileMap(ExeConfigFilename = filePath)
        let config = ConfigurationManager.OpenMappedExeConfiguration(configFileMap, ConfigurationUserLevel.None)
        if isNull config.AppSettings || isNull config.AppSettings.Settings then
            invalidOp (sprintf "Settings missing from config file: %s" filePath)
        let settings = config.AppSettings.Settings
        tryGetConfigValue <- fun (key:string) ->
                                match settings.[key] with
                                | null -> None
                                | v -> Some v.Value

        setConfigValue <- fun (key:string) value ->
                                    if not <| isNull settings.[key] then
                                        settings.Remove(key)
                                    settings.Add(key, value)
        config

    /// Try loading a custom config file
    let public tryLoadConfigFile configFile =
        File.getExistingFile configFile
        |> Option.map loadCustomConfig

    /// Load configuration from a custom app.config file
    let public loadConfigFile configFile =
        tryLoadConfigFile configFile
        |> Option.orDo (fun () -> invalidOp (sprintf "Config file missing: %s " configFile))

    /// Try loading the configuration file next to the calling assembly
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let tryLoadConfigFileNextToAssembly () =
        let callingAssembly = Assembly.GetCallingAssembly()
        let path = sprintf "%s.config" callingAssembly.Location
        path
        |> File.getExistingFile
        |> Option.map loadCustomConfig
        |> ignore

