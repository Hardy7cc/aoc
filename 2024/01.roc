app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    aoc: "https://github.com/lukewilliamboswell/aoc-template/releases/download/0.2.0/tlS1ZkwSKSB87_3poSOXcwHyySe0WxWOWQbPmp7rxBw.tar.br",
}

import pf.Stdin
import pf.Stdout
import pf.Utc
import aoc.AoC {
    stdin: Stdin.readToEnd,
    stdout: Stdout.write,
    time: \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch,
}

main =
    AoC.solve {
        year: 2024,
        day: 1,
        title: "Historian Hysteria",
        part1,
        part2,
    }

exampleInput =
    """
    3   4
    4   3
    2   5
    1   3
    3   9
    3   3
    """

expect
    res = part1 exampleInput
    res == Ok "11"

expect
    res = part2 exampleInput
    res == Ok "31"

## Implement your part1 and part2 solutions here
part1 : Str -> Result Str _
part1 = \input ->
    val =
        input
        |> Str.split "\n"
        |> parseIntoLists
    dbg val

    sorted = {
        num1: List.sortAsc val.num1,
        num2: List.sortAsc val.num2,
    }

    dbg sorted

    distances = List.map2 sorted.num1 sorted.num2 \first, second ->
        Num.absDiff first second
    dbg distances

    res = List.sum distances
    Ok "$(res |> Num.toStr)"

parseIntoLists = \input ->
    input
    |> List.walk { num1: [], num2: [] } \state, line ->
        ele =
            Str.split line " "
            |> List.keepOks Str.toU64
        when ele is
            [first, second] ->
                {
                    num1: List.append state.num1 first,
                    num2: List.append state.num2 second,
                }

            [] -> state
            _ -> crash "Parsing failed"

part2 : Str -> Result Str _
part2 = \input ->
    val =
        input
        |> Str.split "\n"
        |> parseIntoLists
    dbg val

    similarityList =
        val.num1
        |> List.map \first ->
            factor = List.countIf val.num2 \second -> first == second
            first * Num.intCast factor

    dbg similarityList

    res = List.sum similarityList
    dbg res

    # roc nightly pre-release, built from commit c95fdd6 on Mi 13 Nov 2024 11:26:07 UTC
    # thread '<unnamed>' panicked at crates/compiler/mono/src/ir.rs:6191:56:
    # called `Option::unwrap()` on a `None` value
    # Ok "$(res |> Num.toStr)"
    Err res
