# test
library(shiny)
library(tidyverse)
library(Matrix)


# ui.R

item_list <- readRDS("item_list.rds")

ui <- fluidPage(
    
    # App title ----
    headerPanel("Product Recommender for Online Retailer"),
    
    fluidRow(
        
        # Input selection
        column(6, 
               # INPUT
               h3("Select Items and Complete Transaction for Your Suggestions"),    
               wellPanel(
                   selectInput("input_item1", "Item #1", choices = c("", item_list)),
                   selectInput("input_item2", "Item #2", choices = c("", item_list)),
                   selectInput("input_item3", "Item #3", choices = c("", item_list)),
                   selectInput("input_item4", "Item #4", choices = c("", item_list)),
                   selectInput("input_item5", "Item #5", choices = c("", item_list)),
                   actionButton("submit", "Complete Your Purchase")
               )
        ),
        
        # Output table
        column(6,
               h3("Other Items you Might Be Interested in"),     
               tableOutput("item_recom")
        )
    ),
    
    # COMMENTS    
    fluidRow(                                    
        column(12,
               p("For a detailed description of this project, please visit my", 
                 a("Website.", href="http://rpubs.com/DiegoUsai", target="_blank"),
                 "For the full code, please visit my", 
                 a("GitHub page", href = "https://github.com/DiegoUsaiUK/Market_Basket_Analysis", target="_blank"))
        )
    )
)


# server.R

# Load algorithm implementations and similarity calculations
source("cf_algorithm.R")
source("similarity_measures.R")
past_orders_matrix <- readRDS("past_orders_matrix.rds")

server <- function(input,output) {
    
    output$item_recom <- renderTable({
        # react to submit button
        input$submit
        # gather input in string
        customer_order <- 
            isolate(
                
                unique(c(input$input_item1, input$input_item2, input$input_item3, 
                         input$input_item4, input$input_item5))
            )
        
        
        # put in a matrix format
        new_order <- item_list %>%
            # Add a 'value' column with 1's for customer order items
            mutate(value = as.numeric(Description %in% customer_order)) %>%
            # Spread into sparse matrix format
            spread(key = Description, value = value) %>%
            # Change to a matrix
            as.matrix() %>% 
            # Convert to class "dgCMatrix"
            as("dgCMatrix")
        
        # Add new order to retail matrix - binding 2 matrices
        all_orders_dgc <- t(rbind(new_order,past_orders_matrix))
        
        # Set items to predict range
        items_to_predict <- which(all_orders_dgc[ ,1] == 0)
        # items_to_predict <- 1:nrow(all_orders_dgc)
        # Set user to 1
        users <- c(1)
        # Set prediction indices
        prediction_indices <- as.matrix(expand.grid(items_to_predict, users = users))
        
        # Run IBCF model
        recomm <- predict_cf(all_orders_dgc, prediction_indices, 
                             "ibcf", FALSE, cal_cos, 3, FALSE, 4000, 2000)
        
        # Put recommended products into a dataframe
        recomm[,users] %>% 
            as.data.frame() %>% 
            rownames_to_column('NOTE that not all combinations of products return suggestions') %>% 
            filter(.>0) %>% 
            select('NOTE that not all combinations of products return suggestions')
        
    })
}

shinyApp(ui = ui, server = server)


