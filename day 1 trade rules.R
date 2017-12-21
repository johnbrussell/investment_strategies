# Decides if the strategy requires a day-1 trade
init_trade <- function(strategy)
{
    if(strategy == 1 || strategy == 2 || strategy == 3 || strategy == 4 || 
       strategy == 5 || strategy == 6 || strategy == 7 || strategy == 8)
        return(TRUE)
}

# Decides if we invest everything or just what is specified
everything <- function(strategy)
{
    if(strategy == 1 || strategy == 2 || strategy == 3 || strategy == 4 || 
       strategy == 5 || strategy == 6 || strategy == 7 || strategy == 8)
        return(TRUE)
}