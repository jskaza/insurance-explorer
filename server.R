library(shiny)
library(dplyr)
library(ggplot2)
library(tibble)
library(reshape)
library(purrr)
library(tidyr)
library(forcats)
library(plotly)

function(input, output) {
  unit_choice <- reactive({
    plans |> filter(unit == input$unit)
  })
  
  # just display the raw data
  output$data <- renderDataTable(plans)
  
  # barplot of annual cost by plan
  output$plans <- renderPlot({
    unit_choice() |>
      group_by(plan) |>
      filter(row_number() == 1) |>
      ungroup() |>
      ggplot(aes(
        x = reorder(plan, cost),
        y = 26 * cost,
        # cost in is bi-weekly
        fill = plan
      )) + geom_col() +
      xlab("") +
      theme_bw(base_size = 20) +
      ylab("Annual Premium") +
      theme(legend.position = "none")
  })
  
  # plot oop cost vs medical cost
  output$lines <- renderPlot({
    df <- unit_choice() |>
      group_by(plan, network) |>
      filter(service == "ip") |>
      ungroup()
    
    print(df)
    
    asymptotes <- pmap(list(
      d = df$deductible,
      c = df$coins,
      o = df$oop_max
    ),
    asymptote_point) |> unlist()
    
    x <- c(0, 50000, df$deductible, asymptotes) |>
      unique() |>
      rep(each = nrow(df)) |>
      sort()
    
    data <-
      do.call("rbind",
              replicate(length(x) / nrow(df),
                        df, simplify = F)) |>
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
      facet_wrap( ~ network) +
      theme_bw(base_size = 20) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      labs(caption = "*assuming no copays")
  })
  
  
  
  
  
  # estimate annual oop cost based on selections
  output$oop <- renderDataTable({
    premium <-
      melt(as.data.frame(reactiveValuesToList(input)), id = "unit")  |>
      separate(variable, c("input_type", "service", "network")) |>
      mutate(network = fct_recode(network,
                                  "In-Network" = "in",
                                  "OON" = "oon")) |>
      full_join(unit_choice(), by = c("unit", "network", "service")) |>
      group_by(plan) |>
      summarise(premium = 26 * first(cost))
    
    oop_max <- unit_choice() |>
      group_by(plan, network) |>
      summarise(oop_max = first(oop_max))
    
    copay <-
      melt(as.data.frame(reactiveValuesToList(input)), id = "unit")  |>
      separate(variable, c("input_type", "service", "network")) |>
      mutate(network = fct_recode(network,
                                  "In-Network" = "in",
                                  "OON" = "oon")) |>
      full_join(unit_choice(), by = c("unit", "network", "service")) |>
      group_by(plan, network) |>
      filter(!is.na(copay) & input_type == "n") |>
      mutate(total = value * copay) |>
      summarise(copay = sum(total))
    
    misc <-
      melt(as.data.frame(reactiveValuesToList(input)), id = "unit")  |>
      separate(variable, c("input_type", "service", "network")) |>
      mutate(network = fct_recode(network,
                                  "In-Network" = "in",
                                  "OON" = "oon")) |>
      full_join(unit_choice(), by = c("unit", "network", "service")) |>
      group_by(plan, network) |>
      filter(!is.na(extra) & input_type == "cost") |>
      mutate(total = value * extra) |>
      summarise(misc = sum(total))
    
    coins <-
      melt(as.data.frame(reactiveValuesToList(input)), id = "unit")  |>
      separate(variable, c("input_type", "service", "network")) |>
      mutate(network = fct_recode(network,
                                  "In-Network" = "in",
                                  "OON" = "oon")) |>
      full_join(unit_choice(), by = c("unit", "network", "service")) |>
      group_by(plan, network) |>
      filter(!is.na(coins) & input_type == "cost") |>
      summarise(
        deductible = first(deductible),
        coins = first(coins),
        oop_max = first(oop_max),
        total = sum(value)
      ) |>
      group_by(plan, network) |>
      mutate(you_pay = you_pay(total, deductible, coins)) |>
      summarise(coins = sum(you_pay))
    
    reduce(list(oop_max, copay, misc, coins), left_join) |>
      rowwise() |>
      mutate(total = sum(copay, misc, coins, na.rm = TRUE)) |>
      mutate(total = ifelse(total > oop_max, oop_max, total)) |>
      group_by(plan) |>
      summarise(oop = sum(total)) |>
      left_join(premium) |>
      mutate(total = oop + premium) |>
      arrange(total)
    
  })
  
}
