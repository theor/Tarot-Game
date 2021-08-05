module Tarot.Ext
module Array =

    /// Returns true if i is a valid index in the array a.
    /// Otherwise returns false.
    let hasIndex i (a:'a array) =
        a.Length > 0
        && 0 <= i
        && i < a.Length

    /// Swap two values in an array, modifying the array (hence Unsafe).
    /// i is the location of one value to swap.
    /// j is the location of the other value to swap.
    let swapUnsafe i j (a:'a array) =
        if a|> hasIndex i && a|> hasIndex j && i <> j then
            let temp = a.[j]
            a.[j] <- a.[i]
            a.[i] <- temp
module Seq =

    /// Shuffle a sequence with a given RNG.
    /// Uses the provided random number generator and the Durstenfeld shuffle algorithm.
    /// getRandomIndexUpTo is a function which generates a random number between 0 and the given number (both inclusive).
    /// s is the sequence to be shuffled.
    /// Returns a new sequence which is shuffled.
    /// NOTE: does not work on infinite sequences as the sequence is enumerated
    let shuffle (getRandomIndexUpTo:int -> int) (s:'a seq) =
        let arr = Array.ofSeq s
        let lastIndex = (Array.length arr) - 1
        for maxIndex = lastIndex downto 1 do
            let randomJ = getRandomIndexUpTo maxIndex
            Array.swapUnsafe maxIndex randomJ arr
        arr |> Seq.ofArray

    /// Shuffle a sequence with a seeded pseudo-RNG.
    /// Uses a seeded psuedo random number generator and the Durstenfeld shuffle algorithm.
    /// seed is the seed value.
    /// s is the sequence to be shuffled.
    /// Returns a new sequence which is shuffled.
    /// NOTE: does not work on infinite sequences as the sequence is enumerated
    let shuffleSeeded seed s =
        let rngSeeded = new System.Random (seed)
        let getInt max = rngSeeded.Next (max + 1) // because it's exclusive max, shuffle expects inclusive
        s |> shuffle getInt

