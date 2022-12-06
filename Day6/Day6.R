input <- readLines("Day6/input.txt") |> quanteda::tokenize_character() |> 
    unlist() |> paste0(collapse = " ") |> quanteda::tokens()

ngram = 14
packet <- char_ngrams(input[[1]], n = ngram, concatenator = " ") |> 
    strsplit( " ") |> 
    lapply(\(x) all(!duplicated(x))) |> unlist() 

min(which(packet)) + ngram - 1