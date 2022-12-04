rucksack <- readLines("Day3/input.txt")
rucksack <- lapply(rucksack, quanteda::tokenize_character) |> unlist(recursive = FALSE)
item_map <- setNames(1:52, c(letters, LETTERS))

rucksack_split <- lapply(rucksack, \(x){ 
    list(x[1:(length(x)/2)], x[(length(x)/2+1):length(x)])
    })

lapply(rucksack_split, \(x) item_map[Reduce(intersect, x)]) |> unlist() |> sum()

rucksack_group <- split(rucksack, rep(1:100, each = 3))

lapply(rucksack_group, \(x) item_map[Reduce(intersect, x)]) |> unlist() |> sum()