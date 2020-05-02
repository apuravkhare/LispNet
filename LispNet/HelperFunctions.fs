module HelperFunctions

open System.Collections.Generic

let memoize f =
    let cache = Dictionary<_,_>()
    fun x ->
        if cache.ContainsKey(x) then cache.[x]
        else
            let res = f(x)
            cache.[x] <- res
            res