input <- read_file_raw("Day6/input.txt") |> rawToChar(multiple = T) 

ngram = 4 # change for part 2

packet = quanteda::char_ngrams(input, n = ngram, concatenator = "") |> 
    lapply(\(x) length(unique(charToRaw(x))) == ngram) |> unlist()

which(packet)[[1]] + ngram - 1