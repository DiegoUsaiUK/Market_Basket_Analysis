### Loading the Packages
library(tidyverse)            
library(knitr)
library(Matrix)
library(recommenderlab)

## Data

# read retail data from part 2
retail <- readRDS("retail.rds")
# 517,354

# Create `past_orders_matrix` containing the history of past orders. This is a is a user-item sparse matrix. 
# This is needed in the `server.R` file for all the calculations. 
past_orders_matrix <- retail %>%
    # Select only needed variables
    select(InvoiceNo, Description) %>% 
    # Add a column of 1s
    mutate(value = 1) %>%
    # Spread into user-item format
    spread(Description, value, fill = 0) %>%
    select(-InvoiceNo) %>% 
    # Convert to matrix
    as.matrix() %>% 
    # Convert to class "dgCMatrix"
    as("dgCMatrix")

# I save the file for use in the app
saveRDS(past_orders_matrix, 
        file = "past_orders_matrix.rds")

# Create `item_list` list of all the products available to purchase. 
# This will feed in the `ui.R` part of the app to make the producs list available for selection.
item_list <- retail %>% 
    select(Description) %>% 
    unique()

# I save the file for use in the app
saveRDS(item_list, 
        file = "item_list.rds")


## Improved Collaborative Filtering

# First, I re-create the made-up order using the same 6 randomply selected products.
customer_order <- c("GREEN REGENCY TEACUP AND SAUCER",
                    "SET OF 3 BUTTERFLY COOKIE CUTTERS",
                    "JAM MAKING SET WITH JARS",
                    "SET OF TEA COFFEE SUGAR TINS PANTRY",
                    "SET OF 4 PANTRY JELLY MOULDS")

# Next, I use `item_list` to put the `new_order` in a user_item matrix format.
new_order <- item_list %>%
    # Add a 'value' column with 1's for customer order items
    mutate(value = as.numeric(Description %in% customer_order)) %>%
    # Spread into sparse matrix format
    spread(key = Description, value = value) %>%
    # Change to a matrix
    as.matrix() %>% 
    # Convert to class "dgCMatrix"
    as("dgCMatrix")

# Then, I add the `new_order` to the `past_orders_matrix` as its first order.
all_orders_dgc <- t(rbind(new_order,past_orders_matrix))

# Now, I set a number of parameters required by the Improved CF to work:

# Set range of items to calculate predictions for - here I select them all
items_to_predict <- 1:nrow(all_orders_dgc)
# Set current user to 1, which corresponds to new_order
users <- c(1)
# Set prediction indices
prediction_indices <- as.matrix(expand.grid(items_to_predict, users = users))


# Load algorithm implementations and similarity calculations
source("cf_algorithm.R")
source("similarity_measures.R")

# Finally I can run the IBCF model and test the runtime with the Improved CF 
start <- Sys.time()
recomm <- predict_cf(all_orders_dgc, prediction_indices,
                     "ibcf", FALSE, cal_cos, 3, FALSE, 4000, 2000)
end <- Sys.time()
cat('runtime', end - start)
## runtime 0.630003
    
## Let's now run the IBCF model and test the runtime with recommenderlab to compare performances
# Convert `all_orders` to class "binaryRatingMatrix"
all_orders_brm <- as(all_orders, "binaryRatingMatrix")

# Run run IBCF model on recommenderlab
start <- Sys.time()
recomm <- Recommender(all_orders_brm, 
method = "IBCF",  
param = list(k = 5))
end <- Sys.time()
cat('runtime', end - start)
## runtime 12.75939
