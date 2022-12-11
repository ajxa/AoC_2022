library(tidyverse)

input <- readr::read_file("Day8/input.txt") %>%  
    stringr::str_split("\n") %>% 
    unlist() %>%
    stringr::str_split("") %>% 
    sapply(as.numeric) %>% 
    t()


# Part 1 ----

inner_coords <- expand.grid(x = 2:(nrow(input)-1), 
                            y = 2:(ncol(input)-1)) %>%
    split(1:nrow(.)) %>% 
    purrr::map(unlist)


tree_visible <- function(tree, patch){
    
    tree_height <- patch[tree[["x"]], tree[["y"]]]

    trees_around <- list(
        
        above = patch[1:(tree["x"]-1),tree["y"]],
        
        below = patch[(tree["x"]+1):nrow(patch),tree["y"]],
        
        left = patch[tree["x"],1:(tree["y"]-1)],
        
        right = patch[tree["x"],(tree["y"]+1):ncol(patch)]
                  
        )
    
    
    purrr::map_lgl(trees_around, ~ all(.x < tree_height)) %>% any()
    
}

edge_trees <- function(patch){
    
    out <- list(top_bottom = patch[c(1,nrow(patch)),],
                left_right = patch[,c(1, ncol(patch))])
    
    out$left_right <- out$left_right[-c(1,nrow(out$left_right)),]
    
    
    length(unlist(out))
    
}

purrr::map_lgl(inner_coords, ~tree_visible(tree = .x, patch = input)) %>% 
    sum(., edge_trees(patch = input)) 


# Part 2 ----

tree_scenic_score <- function(tree, patch){
    
    tree_height <- patch[tree[["x"]], tree[["y"]]]

    trees_around <- list(
        
        above = patch[(tree["x"]-1):1, tree["y"]],
        
        below = patch[(tree["x"]+1):nrow(patch), tree["y"]],
        
        left = patch[tree["x"], (tree["y"]-1):1],
        
        right = patch[tree["x"], (tree["y"]+1):ncol(patch)]
        
    )
    
  score <-  purrr::map(trees_around, ~{
        
        same_or_taller  <- .x >= tree_height
    
        if(any(same_or_taller)) which(same_or_taller)[1] else length(same_or_taller)
    
    })
    
  return(purrr::reduce(score, `*`))
  
}

purrr::map_dbl(inner_coords, ~tree_scenic_score(tree = .x, patch = input)) %>%
    max() 
