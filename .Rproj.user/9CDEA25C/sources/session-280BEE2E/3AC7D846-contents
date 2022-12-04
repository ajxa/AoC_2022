library(tidyverse)

guide <- read_csv("data/day2.csv")

guide <- as.data.frame(str_split(guide$scores, " ", n = 2,simplify = T))

names(guide) <- c("p1","p2")

head(guide)


guide$p1 <- ifelse(guide$p1 == "A","rock", 
       ifelse(guide$p1 == "B", "paper", "scissors"))

guide$p2 <- ifelse(guide$p2 == "X","rock", 
       ifelse(guide$p2 == "Y", "paper", "scissors"))


guide$shape_selected <- ifelse(guide$p2 == "rock", 1, 
                               ifelse(guide$p2 == "paper", 2, 3))

# 0 = lost;3 = draw;6 = won


# my = guide$p2[4]
# 
# opp = guide$p1[4]


get_scores <- function(my, opp){
    
    if(my == opp) return(3)
    
    if(my == "rock" & opp != "paper"){
        
        return(6)
        
    }else if(my == "paper" & opp != "scissors"){
        
        return(6)
        
    } else if(my == "scissors" & opp != "rock") return(6) else return(0)
    
}

guide$game_score <- purrr::map2(guide$p2, guide$p1, ~get_scores(my = .x, opp = .y)) %>%
    unlist(use.names = F)


guide <- guide %>%
    mutate(round_score = shape_selected + game_score)


# Part 2

guide <- read_csv("data/day2.csv")

guide <- as.data.frame(str_split(guide$scores, " ", n = 2,simplify = T))

names(guide) <- c("p1","game_score")

head(guide)


guide$p1 <- ifelse(guide$p1 == "A","rock", 
                   ifelse(guide$p1 == "B", "paper", "scissors"))

guide$game_score <- ifelse(guide$game_score == "X",0, 
                   ifelse(guide$game_score == "Y", 3, 6))


guide$shape_selected <- ifelse(guide$p2 == "rock", 1, 
                               ifelse(guide$p2 == "paper", 2, 3))


outcome = guide$game_score[1]
opp = guide$p1[1]

get_shape(outcome = outcome, opp = opp)

get_shape <- function(outcome, opp){
    
    if(outcome == 3) return(opp)
    
    if(outcome == 6 & opp == "rock"){
        
        return("paper")
        
    }else if(outcome == 6 & opp == "paper"){
        
        return("scissors")
        
    }else if(outcome == 6 & opp == "scissors"){
        
        return("rock")
        
    }else if(outcome == 0 & opp == "rock"){
        
        return("scissors")
        
    }else if(outcome == 0 & opp == "paper"){
        
        return("rock")
        
    }else if(outcome == 0 & opp == "scissors"){ 
        
        return("paper")
        
    } 
    
}


guide$shape_selected <- purrr::map2(guide$game_score, guide$p1, ~get_shape(outcome = .x, opp = .y)) %>%
    unlist()


guide$shape_selected_score <- ifelse(guide$shape_selected == "rock", 1, 
                               ifelse(guide$shape_selected == "paper", 2, 3))



sum(guide$game_score + guide$shape_selected_score)


