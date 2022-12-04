input <- readLines("Day2/input.txt") |> strsplit(" ")

score_map <- expand.grid(p1 = 1:3, p2 = 1:3)

score_map$result <- ifelse(score_map[[2]]-score_map[[1]] == 1 | score_map[[2]]-score_map[[1]] == -2, 6, 
                           ifelse(score_map[[1]] == score_map[[2]], 3, 0))

part1_map <- setNames(rep(1:3, times =2), c("A","B","C","X","Y","Z"))

result1 <- sapply(input, \(x) as.numeric(part1_map[x])) |> t() |> as.data.frame() |> 
    setNames(c("p1","p2")) |> merge(y=score_map, c("p1","p2"), all.x=TRUE)

sum(result1$p2, result1$result)

part2_map <- setNames(c(1:3,0,3,6), c("A","B","C","X","Y","Z"))

result2 <- sapply(input, \(x) as.numeric(part2_map[x])) |> t() |> as.data.frame() |> 
    setNames(c("p1","result")) |> merge(y=score_map, c("p1","result"), all.x=TRUE)

sum(result2$p2, result2$result)