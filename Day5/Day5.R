instructions <- readLines("Day5/input.txt")

instructions <- instructions[11:length(instructions)] |> strsplit(" ") |> 
    lapply(\(x) setNames(as.numeric(c(x[[2]], x[[4]], x[[6]])),
                         c("amount", "from", "to")))

creates <- list('1'="RCH",
                '2'="FSLHJB",
                '3'="QTJHDMR",
                '4'="JBZHRGS",
                '5'="BCDTZFPR",
                '6'="GCHT",
                '7'="LWPBZVNS",
                '8'="CGQJR",
                '9'="SFPHRTDL")

creates <- lapply(creates, \(x) quanteda::tokenize_character(x)) |> unlist(recursive = F)

rearranged <- creates

for (i in 1:length(instructions)) {
    
    set = instructions[[i]]
    
    
    if(set[["amount"]] > 1){
        
        # move_quantity = set[["amount"]]:1 # part 1
        move_quantity = 1:set[["amount"]] # part 2
        
        }else move_quantity = set[["amount"]]
    
    rearranged[[set["to"]]] = c(rearranged[[set[["from"]]]][move_quantity], 
                                rearranged[[set["to"]]])
    
    rearranged[[set["from"]]]  = rearranged[[set["from"]]][-move_quantity]
}

sapply(rearranged, \(x) x[[1]]) |> paste0(collapse = "")