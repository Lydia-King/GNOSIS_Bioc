setup <- function(){

    tags <- shiny::tags
    validate <- shiny::validate
    levels = base::levels
    unique = base::unique
    sapply = base::sapply
    hide = shinyjs::hide
    show = shinyjs::show
}

tags <- shiny::tags
validate <- shiny::validate
levels = base::levels
unique = base::unique
sapply = base::sapply
hide = shinyjs::hide
show = shinyjs::show

## Functions
# Function to get complete cases
completeFun <- function(data, desiredCols) {
    completeVec <- complete.cases(data[, desiredCols])
    return(data[completeVec, ])
}

# Get Mode
getmode <- function(v) {uniqv <- unique(v)
uniqv[which.max(tabulate(match(v, uniqv)))]}

# experiment with the multiplier to find the perfect position
give.n <- function(x){return(c(y = median(x)*1.05, label = length(x)))}


