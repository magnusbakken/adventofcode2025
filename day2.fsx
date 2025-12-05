type Range = {
    Start: int64;
    End: int64;
}

let parseRange (s: string) =
    let parts = s.Split("-")
    { Start = int64 parts.[0]; End = int64 parts.[1] }

let parseFile (filePath: string) =
    System.IO.File.ReadAllText filePath
    |> fun s -> s.Split([|','|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map parseRange

let generateRange range = [range.Start .. range.End]

let isInvalidId n =
    let s = n.ToString()
    s.Length % 2 = 0 && s.Substring(0, s.Length / 2) = s.Substring(s.Length / 2)

let numberChunks (s: string) chunkSize =
    [ for i in 0 .. chunkSize .. s.Length - chunkSize -> s.Substring(i, chunkSize) ]

let allNumberChunks (s: string) =
    [
        for size in 1 .. s.Length / 2 -> if s.Length % size = 0 then numberChunks s size else []
    ] |> List.filter (fun l -> l.Length > 0)

let listItemsAreEqual l =
    match l with
    | [] -> true
    | [_] -> true
    | head :: tail -> List.forall (fun elem -> elem = head) tail

let isInvalidId2 n =
    List.exists listItemsAreEqual (allNumberChunks (n.ToString()))

let sumInvalidIds isInvalid  =
    parseFile "day2.txt"
        |> Array.toList
        |> List.collect generateRange
        |> List.filter isInvalid
        |> List.sum

printfn "%A" (sumInvalidIds isInvalidId)
printfn "%A" (sumInvalidIds isInvalidId2)