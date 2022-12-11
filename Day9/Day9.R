library(dplyr)

motions <- readLines("Day9/test.txt") %>% str_split(" ", simplify = T) %>% 
    as.data.frame() %>%
    rename(direction = V1, step = V2) %>%
    mutate(across(step, as.numeric))

motions$step[grep("L|D", motions$direction)] <- motions$step[grep("L|D", motions$direction)] * -1

# Assuming the head and the tail both start at the same position, overlapping:

head <- list(c(0, 0))

head_movements <- function(head_coord, direction, step){
    
   previous_coord <- head_coord[[length(head_coord)]]
   
   if(direction == "L" | direction == "R"){
      new_coord <- list(c(previous_coord[1] + step, previous_coord[2]))
   }
    
   if(direction == "U" | direction == "D"){
       new_coord <- list(c(previous_coord[1], previous_coord[2] + step))
   }
  
   
   c(head_coord, new_coord)
   
   return(c(head_coord, new_coord))

}

for(i in 1:nrow(motions)){
    
   head = head_movements(head_coord = head, 
                         direction = motions$direction[[i]], 
                         step = motions$step[[i]])
}

#   HEAD      TAIL
#   0,0       0,0     same y
#   4,0       3,0     same y
#   4,4       4,3    
#   1,4       2,4
#   1,3       2,4
#   5,3       4,3
#   5,2       4,3
#   0,2       1,2
#   2,2       1,2

motions <- lapply(head, \(x) data.frame(head_x = x[[1]], head_y = x[[2]])) %>% 
    bind_rows() %>%
    slice(-1) %>%
    cbind(motions, .)
    

motions$head_x - 1



input <- read.table("Day9/test.txt",sep=" ")

hpos <- tpos <- matrix(0,ncol=2)

for(i in 1:nrow(input)){
    d <- switch(input[i,1],"R"=c(1,0),
                "U"=c(0,1),"L"=c(-1,0),"D"=c(0,-1))
    for(j in 1:input[i,2]){
        hpos <- rbind(hpos,hpos[nrow(hpos),]+d)
    }
}

for(i in 1:nrow(hpos)){
    dif <- hpos[i,]-tail(tpos,1)
    
    if(any(abs(dif)>1)){
        if(dif[1]>=1&dif[2]==0) m <- c(1,0)
        if(dif[1]>=1&dif[2]>=1) m <- c(1,1)
        if(dif[1]>=1&dif[2]<=-1) m <- c(1,-1)
        if(dif[1]<=-1&dif[2]==0) m <- c(-1,0)
        if(dif[1]<=-1&dif[2]<=-1) m <- c(-1,-1)
        if(dif[1]<=-1&dif[2]>=1) m <- c(-1,1)
        if(dif[1]==0&dif[2]>=1) m <- c(0,1)
        if(dif[1]==0&dif[2]<=-1) m <- c(0,-1)
        tpos <- rbind(tpos, tail(tpos,1)+m)
    }
}
single <- tpos[!duplicated(tpos),]
nrow(single)


#Now tpos is position of knot 2
for(k in 3:10){
    hpos <- tpos
    tpos <- matrix(0,ncol=2)
    for(i in 1:nrow(hpos)){
        dif <- hpos[i,]-tail(tpos,1)
        if(any(abs(dif)>1)){
            if(dif[1]>=1&dif[2]==0) m <- c(1,0)
            if(dif[1]>=1&dif[2]>=1) m <- c(1,1)
            if(dif[1]>=1&dif[2]<=-1) m <- c(1,-1)
            if(dif[1]<=-1&dif[2]==0) m <- c(-1,0)
            if(dif[1]<=-1&dif[2]<=-1) m <- c(-1,-1)
            if(dif[1]<=-1&dif[2]>=1) m <- c(-1,1)
            if(dif[1]==0&dif[2]>=1) m <- c(0,1)
            if(dif[1]==0&dif[2]<=-1) m <- c(0,-1)
            tpos <- rbind(tpos, tail(tpos,1)+m)
        }
    }
}

single <- tpos[!duplicated(tpos),]
nrow(single)

