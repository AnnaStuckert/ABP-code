get_clean_data <- function(data){

data_clean <- select(data, -matches("_complete"))

return(data_clean)
}
