`%>%` <- magrittr::`%>%`
input <- readLines("Day5/input.txt")

creates <- input[1:8] %>%  gsub("\\s{4}", "[!]", x = .) %>%  
    gsub("\\[|\\]|\\s+", "", x = .) %>%
    quanteda::tokenize_character() %>% 
    purrr::map(~ifelse(.x == "!", NA, .x)) %>% as.data.frame() %>% 
    split(seq(nrow(.))) %>% purrr::map(~as.character(na.omit(unlist(.x, use.names = F))))
  
instructions <- grep("^move", input, value = T) %>% strsplit(" ") %>% 
    purrr::map(~ setNames(as.numeric(c(.x[[2]], .x[[4]], .x[[6]])),
                          c("amount", "from", "to")))

rearranged <- creates

for (i in 1:length(instructions)) {
    
    set <- instructions[[i]]
    
    if(set[["amount"]] > 1){
        
        # move_quantity <- set[["amount"]]:1 # part 1
        move_quantity <- 1:set[["amount"]] # part 2
        
        }else move_quantity <- set[["amount"]]
    
    rearranged[[set["to"]]] <- c(rearranged[[set[["from"]]]][move_quantity], 
                                 rearranged[[set["to"]]])
    
    rearranged[[set["from"]]] <- rearranged[[set["from"]]][-move_quantity]
}

rearranged %>% purrr::map_chr(~purrr::pluck(.x, 1)) %>% paste0(collapse = "")
