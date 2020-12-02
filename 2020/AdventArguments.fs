namespace codingforbeer.AdventOfCode

open Argu

type AdventArguments = 
    | [<AltCommandLine("-d")>] Day of int
with 
    interface IArgParserTemplate with
        member arg.Usage = 
            match arg with 
            | Day _ -> "Which advent day?"