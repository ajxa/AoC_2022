pairs <- readLines("Day4/input.txt") |> strsplit(",") |> lapply(\(x) strsplit(x,"-"))

pairs <- lapply(pairs, \(pair){
    lapply(pair, \(x) IRanges::IRanges(as.numeric(x)[1]:as.numeric(x)[2]))
})

lapply(pairs, \(x) IRanges::`%within%`(x[[1]], x[[2]]) | IRanges::`%within%`(x[[2]], x[[1]])) |> 
    unlist() |> which() |> length()

lapply(pairs, \(x)  IRanges::`%over%`(x[[1]], x[[2]])) |> unlist() |> which() |> length()