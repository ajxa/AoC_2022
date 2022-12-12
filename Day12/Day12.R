library(tidyverse)

map <- readLines("Day12/input.txt") %>% str_split("") %>% 
    do.call(rbind, .)

start <- which(map=="S", arr.ind=T)
end <- which(map=="E", arr.ind=T)
map[start]<-"a"
map[end]<-"z"

all_coords <- expand.grid(x = seq_len(nrow(map)) , y =seq_len(ncol(map))) %>%
    
    split(seq_len(nrow(.))) %>%
    
    lapply(\(x) setNames( c(x[["x"]], x[["y"]]), c("x","y")))

squares <- matrix(1:length(map), nrow = nrow(map), ncol = ncol(map))

destination_squares <- function(step){
    
    possible_moves <- rbind(c(step["x"]-1, step["y"]), # left
                            c(step["x"]+1, step["y"]), # right
                            c(step["x"], step["y"]-1), # down
                            c(step["x"], step["y"]+1)) # up
    
    possible_moves[,"x"] <- ifelse(possible_moves[,"x"] < 1, 1, possible_moves[,"x"])
    possible_moves[,"x"] <- ifelse(possible_moves[,"x"] > nrow(map), nrow(map), possible_moves[,"x"])
    
    possible_moves[,"y"] <- ifelse(possible_moves[,"y"] < 1, 1, possible_moves[,"y"])
    possible_moves[,"y"] <- ifelse(possible_moves[,"y"] > ncol(map), ncol(map), possible_moves[,"y"])
    
    self <- which(possible_moves[,"x"] == step[["x"]] & possible_moves[,"y"] == step[["y"]])
    
    if(length(self) >= 1) possible_moves <- possible_moves[-self,]
    
    step_letter <- which(letters == map[step["x"], step["y"]])
    
    possible_move_letters <- apply(possible_moves, 1, \(x) which(letters == map[x["x"], x["y"]]))
    
    possible_moves <- possible_moves[possible_move_letters <= (step_letter + 1),, drop=FALSE]
    
    if(nrow(possible_moves)){
        
        return(
            data.frame(
                "from" = squares[step["x"], step["y"]],
                "to"  =apply(possible_moves, 1,\(x) squares[x["x"],x["y"]])
                )
        )
        
    } else return(NULL)
    
}

network_edges <- lapply(all_coords, destination_squares) %>% do.call(rbind, .)

graph <- as.matrix(network_edges) %>% igraph::graph_from_edgelist(directed = TRUE)


start_vertex <- sapply(all_coords,\(x) all(x == start)) %>% which()
end_vertex <- sapply(all_coords,\(x) all(x == end)) %>% which()

igraph::distances(graph = graph,
                  v = start_vertex,
                  to = end_vertex,
                  mode = "out")
# Part 2 ----

all_coords <- do.call(rbind, all_coords)

a_positions <- which(map == "a", arr.ind = T)

a_vertices  <- apply(a_positions, 1, \(x){
    
    which(all_coords[,"x"] == x["row"] & all_coords[,"y"] == x["col"])
    
})

distances(graph = graph, v = a_vertices, to = end_vertex, mode="out") %>%
    min()
