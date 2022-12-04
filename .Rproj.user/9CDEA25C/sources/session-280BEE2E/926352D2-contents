out <- readLines("Day1/input.txt") |> paste0(collapse = " ") |> strsplit("  ") |>
    lapply(strsplit, split = " ") |> unlist(recursive = FALSE) |>
    lapply(\(x) sum(as.numeric(x))) |> unlist() |> sort(decreasing = T)

out[1]
sum(out[1:3])