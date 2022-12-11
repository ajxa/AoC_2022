library(tidyverse)

input <- read_lines("Day11/input.txt") %>% str_remove_all("^\\s+|:$")
    
input <- input[which(nchar(input) != 0)] %>% split(ceiling(seq_along(.)/6))

parse_monkeys <- function(monkey, part2 = FALSE){
    
   items <- str_remove(monkey[[2]], "Starting items: ") %>% 
        str_split(",") %>% unlist() %>% as.numeric()
   
   if(part2){
       
       operation <- monkey[[3]] %>% 
           str_remove_all("Operation: new = ") %>%
           str_replace_all(pattern = "old", replacement = "x") %>%
           paste0("function(x){",.,"}")
       
       operation <- eval(parse(text = operation))
       
   }else{
       
       operation <- monkey[[3]] %>% 
           str_remove_all("Operation: new = ") %>%
           str_replace_all(pattern = "old", replacement = "x") %>%
           paste0("function(x){","floor((",.,")/3) }")
       
       operation <- eval(parse(text = operation))
    
   }
   
   test <- monkey[4:6] %>% 
       str_extract_all("\\d+") %>% unlist() %>% as.numeric()
   
   divisiable_by <- test[[1]]
   
   test <- paste0("function(x) if(!(x %%", 
                  test[[1]],")) ", test[[2]],' else ', test[[3]])
   
   test <- eval(parse(text = test))
  
   return( list(items = items, 
                operation = operation, 
                test = test,
                divisable_by = divisiable_by,
                inspected=0)
           )
}
 
# Part 1 ----

monkeys <- lapply(input, parse_monkeys, part2 = FALSE)

total_rounds <- 20

for(round in seq_len(total_rounds)){
    
    for(monkey in seq_along(monkeys)){
        
        for(item in seq_along(monkeys[[monkey]]$items)){
            
            worry <- monkeys[[monkey]]$operation(monkeys[[monkey]]$items[item])
            
            next_m <- monkeys[[monkey]]$test(worry) + 1
            
            monkeys[[next_m]]$items <- c(monkeys[[next_m]]$items, worry)
            
            monkeys[[monkey]]$inspected <- monkeys[[monkey]]$inspected + 1
        }
        
        monkeys[[monkey]]$items <- c()
    }
    
}

sapply(monkeys,\(x) x$inspected) %>% sort(decreasing = T) %>% head(n = 2) %>% prod()

# Part 2 ----
library(bit64) # to store in the large numerics

monkeys <- lapply(input, parse_monkeys, part2 = TRUE)

total_rounds <- 10000 

worry_divis <- sapply(monkeys,\(x) x$divisable) %>% 
    paste0(collapse = "*") %>% parse(text = .) %>% eval(.)

for(round in seq_len(total_rounds)){
    
    for(monkey in seq_along(monkeys)){
        
        if(length(monkeys[[monkey]]$items)){
            
            for(item in seq_along(monkeys[[monkey]]$items)){
                
                worry <- monkeys[[monkey]]$operation(monkeys[[monkey]]$items[item]) %% worry_divis
                
                next_m <- monkeys[[monkey]]$test(worry)+1
                
                monkeys[[next_m]]$items <- c(monkeys[[next_m]]$items,worry)
                
                monkeys[[monkey]]$inspected <- monkeys[[monkey]]$inspected+1
            }
            
            monkeys[[monkey]]$items <- c()
        }}
    
    if(!round%%100) cat(round,"\r")
}

sapply(monkeys,\(x) x$inspected) %>% sort(decreasing = T) %>% head(n = 2) %>% prod()

