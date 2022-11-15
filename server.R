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
    plans |> filter(unit == input$unit)
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
    ) |> unlist()
    
    x <- c(0, 50000, unit_choice()$deductible, asymptotes) |>
      unique() |>
      rep(each = nrow(unit_choice())) |>
      sort()
    
    data <-
      do.call("rbind",
              replicate(length(x) / nrow(unit_choice()),
                        unit_choice(), simplify = F)) |>
      mutate(`Medical Costs` = as.numeric(x)) |>
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
  output$deductible <- renderTable({
      melt(as.data.frame(reactiveValuesToList(input)), id = "unit") |>
      separate(variable, c("input_type", "location", "network")) |>
      mutate(network = fct_recode(network,
                                  "In-Network" = "in",
                                  "OON" = "oon")) |>
      full_join(unit_choice(), by = c("unit", "network", "location")) #|>
      # filter(input_type == "cost") |>
      # filter(is.na(copay)) |>
      # group_by(network, plan, deductible, coins, oop_max) |>
      # summarise(total = sum(value),) |>
      # mutate(total = you_pay(total, deductible, coins, oop_max))
  })
  
  # output$copay <- renderTable({
  #   melt(as.data.frame(reactiveValuesToList(input)), id = "unit") |>
  #     separate(variable, c("input_type", "location", "network")) |>
  #     mutate(network = fct_recode(network,
  #                                 "In-Network" = "in",
  #                                 "OON" = "oon")) |>
  #     full_join(unit_choice(), by = c("unit", "network")) |>
  #     filter(input_type == "n") |>
  #     mutate(
  #       copay = copay_eligible(
  #         location,
  #         copay_primary,
  #         copay_specialist,
  #         copay_urgent,
  #         copay_er,
  #         copay_preventative
  #       )
  #     ) |>
  #   filter(copay) |>
  #   mutate(
  #     copay_cost = calc_copay(
  #       location,
  #       value,
  #       copay_primary,
  #       copay_specialist,
  #       copay_urgent,
  #       copay_er,
  #       copay_preventative
  #     )
  #   ) |>
  #   group_by(plan) |>
  #   summarise(total = sum(copay_cost))
      
  # })
  
  
  
  
  
  
}