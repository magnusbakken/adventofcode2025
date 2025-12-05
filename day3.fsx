let parseFile =
    System.IO.File.ReadAllLines "day3.txt"
    |> Array.map Seq.toList
    |> Array.toList

let chooseNFromList l (n: int) =
    let rec chooseNFromListRec l n acc =
        match n, l with
        | 0, _ -> [List.rev acc]
        | _, [] -> []
        | _, head :: tail ->
            let withHead = chooseNFromListRec tail (n - 1) (head :: acc)
            let withoutHead = chooseNFromListRec tail n acc
            withHead @ withoutHead
    chooseNFromListRec l n []

let findHighestBatteriesInJoltage (n: int) (joltage: list<char>) =
    let rec pick k l =
        match k, l with
        | 0, _ -> []
        | _, [] -> []
        | _, _ ->
            let m = List.length l
            if k >= m then l
            else
                let window = m - k + 1
                let prefix = List.take window l
                let chosen = List.max prefix
                let idx = prefix |> List.findIndex ((=) chosen)
                let suffix = l |> List.skip (idx + 1)
                chosen :: (pick (k - 1) suffix)
    pick n joltage

let findHighest (n: int) (joltage: list<char>) =
    let filteredJoltage = findHighestBatteriesInJoltage n joltage
    chooseNFromList filteredJoltage n
        |> List.map (fun items -> System.String.Concat items)
        |> List.map int64
        |> List.max

let result1 =
    parseFile
    |> List.map (findHighest 2)
    |> List.sum

let result2 =
    parseFile
    |> List.map (findHighest 12)
    |> List.sum

printfn "%A" result1
printfn "%A" result2