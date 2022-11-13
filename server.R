library(shiny)
library(dplyr)
library(ggplot2)
library(tibble)
library(reshape)
library(purrr)
library(tidyr)
library(forcats)

function(input, output) {
  unit_choice <- reactive({
    plans %>% filter(unit == input$unit)
  })
  
  # just display the raw data
  output$data <- renderDataTable(plans)
  
  # barplot of annual cost by plan
  output$plans <- renderPlot({
    ggplot(unit_choice(), aes(
      x = reorder(plan, cost),
      y = 26 * cost,
      # cost is bi-weekly
      fill = plan
    )) + geom_col() +
      xlab("") +
      theme_bw(base_size = 20) +
      ylab("Annual Cost") +
      theme(legend.position = "none")
  })
  
  # plot oop cost vs medical cost
  output$lines <- renderPlot({
    cost_fun <- function(x, d, c, o) {
      case_when((0 <= x) & (x < d) ~ x,
                (d <= d + c * (x - d)) &
                  (d + c * (x - d) < o) ~ d + c * (x - d),
                TRUE ~ o)
    }
    
    asymptotes <- pmap(
      list(
        d = unit_choice()$deductible,
        c = unit_choice()$coins,
        o = unit_choice()$oop_max
      ),
      asymptote_point
    ) %>% unlist()
    
    x <- c(0, 50000, unit_choice()$deductible, asymptotes) |>
      unique() |>
      rep(each = nrow(unit_choice())) |>
      sort()
    
    data <-
      do.call("rbind",
              replicate(length(x) / nrow(unit_choice()),
                        unit_choice(), simplify = F)) %>%
      mutate(`Medical Costs` = as.numeric(x)) %>%
      mutate(`OOP Cost` = as.numeric(pmap(
        list(
          x = as.numeric(x),
          d = as.numeric(deductible),
          c = as.numeric(coins),
          o = as.numeric(oop_max)
        ),
        cost_fun
      )))
    
    
    ggplot(data, aes(x = `Medical Costs`, y = `OOP Cost`, colour = plan)) +
      geom_line() +
      facet_wrap(~ network) +
      theme_bw(base_size = 20) +
      theme(legend.position = "bottom", legend.title = element_blank())
  })
  
  # estimate annual oop cost based on selections
  output$txt <- renderTable({
    df <-
      melt(as.data.frame(reactiveValuesToList(input)), id = "unit") %>%
      separate(variable, c("input_type", "location", "network")) %>%
      mutate(network = fct_recode(network,
                                  "In-Network" = "in",
                                  "OON" = "oon")) %>%
      # deal with urgent care oon copay
      full_join(unit_choice())
    
    
    
    deductible <- df %>%
      filter(input_type == "cost") %>%
      mutate(copay = 
               (location =="primary" && !is.na(copay_primary) ||
                   location =="specialist" && !is.na(copay_specialist) ||
                   location =="urgent" && !is.na(copay_urgent) ||
                   location =="er" && !is.na(copay_er))
                   )
 
      
      # group_by(network) %>%
      # summarise(cost = sum(value))
    # unit_choice()
  })
  
  
  
  
  
}