#!/usr/bin/env fsharpi

open System.IO

type Move =
    | Left of amount: int
    | Right of amount: int

let parseMove (move: string) =
    if move.StartsWith("L") then
        Left(move.Substring(1) |> int)
    else
        Right(move.Substring(1) |> int)

let turn (position: int) move =
    match move with
    | Left amount -> (position - amount) % 100
    | Right amount -> (position + amount) % 100

let turn2 (position: int) move =
    match move with
    | Left amount ->
        let temp = (position - amount) % 100
        let coerced = if temp < 0 then 100 + temp else temp
        (coerced, position - amount)
    | Right amount -> (position + amount) % 100, position + amount

let rec countZeroes position (moves: seq<Move>) =
    match Seq.toList moves with
    | [] -> 0
    | x :: xs ->
        let next = turn position x
        let extra = if next = 0 then 1 else 0
        extra + countZeroes next xs

let rec countTimesThroughZero position (moves: seq<Move>) =
    match Seq.toList moves with
    | [] -> 0
    | x :: xs ->
        let next, raw = turn2 position x
        let extra = if (position <> 0 && raw < 0) || raw = 0 then 1 else 0
        let total = abs (raw / 100) + extra
        total + countTimesThroughZero next xs

let input = File.ReadAllLines "day1.txt" |> Array.map parseMove

printfn "%A" (countZeroes 50 input)
printfn "%A" (countTimesThroughZero 50 input)
