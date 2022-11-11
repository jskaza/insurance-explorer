library(shiny)
library(dplyr)
library(ggplot2)
library(tibble)
library(reshape)
library(purrr)

# Define UI for application that draws a histogram
ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    h3("Unit"),
    radioButtons("unit",
                 NULL,
                 c("Individual",
                   # "Employee + Spouse",
                   # "Employee + Child(ren)",
                   "Family")),
    
    h3("Preventative Care Costs"),
    sliderInput(
      "preventative_in",
      "In-Network:",
      min = 0,
      max = 500000,
      value = 0
    ),
    
    sliderInput(
      "preventative_out",
      "OON:",
      min = 0,
      max = 500000,
      value = 0
    ),
    
    
    h3("Primary Care"),
    sliderInput(
      "pcp_in_cost",
      "In-Network Costs:",
      min = 0,
      max = 500000,
      value = 0
    ),
    sliderInput(
      "pcp_in_n",
      "In-Network Visits:",
      min = 0,
      max = 50,
      value = 0
    ),
    sliderInput(
      "pcp_out",
      "OON Cost:",
      min = 0,
      max = 500000,
      value = 0
    ),
    
    h3("Specialists"),
    sliderInput(
      "specialist_in_cost",
      "In-Network Costs:",
      min = 0,
      max = 500000,
      value = 0
    ),
    sliderInput(
      "specialist_in_n",
      "In-Network Visits:",
      min = 0,
      max = 50,
      value = 0
    ),
    sliderInput(
      "specialist_out",
      "OON Costs:",
      min = 0,
      max = 500000,
      value = 0
    ),
    
    h3("Virtual Care Costs"),
    sliderInput(
      "virtual_in",
      "In-Network:",
      min = 0,
      max = 500000,
      value = 0
    ),
    sliderInput(
      "virtual_out",
      "OON:",
      min = 0,
      max = 500000,
      value = 0
    ),
    
    h3("Urgent Care"),
    sliderInput(
      "urgent_in_cost",
      "In-Network Costs:",
      min = 0,
      max = 500000,
      value = 0
    ),
    sliderInput(
      "urgent_in_n",
      "In-Network Visits:",
      min = 0,
      max = 50,
      value = 0
    ),
    sliderInput(
      "urgent_out",
      "OON Costs:",
      min = 0,
      max = 500000,
      value = 0
    ),
    
    h3("Emergency Room"),
    sliderInput(
      "er_in_cost",
      "In-Network Costs:",
      min = 0,
      max = 500000,
      value = 0
    ),
    sliderInput(
      "er_in_n",
      "In-Network Visits:",
      min = 0,
      max = 50,
      value = 0
    ),
    sliderInput(
      "er_out",
      "OON Costs:",
      min = 0,
      max = 500000,
      value = 0
    ),
    
    h3("Diagnostic Procedure Costs"),
    sliderInput(
      "diagnostics",
      NULL,
      min = 0,
      max = 500000,
      value = 0
    ),
    
    h3("MRI/MRA, CT/PET Costs"),
    sliderInput(
      "scans_in",
      "In-Network:",
      min = 0,
      max = 500000,
      value = 0
    ),
    sliderInput(
      "scans_out",
      "OON:",
      min = 0,
      max = 500000,
      value = 0
    ),
    
    h3("Inpatient Stay Costs"),
    sliderInput(
      "ip_in",
      "In-Network:",
      min = 0,
      max = 500000,
      value = 0
    ),
    sliderInput(
      "ip_out",
      "OON:",
      min = 0,
      max = 500000,
      value = 0
    ),
    
    h3("Outpatient Surgery Costs"),
    sliderInput(
      "op_in",
      "In-Network:",
      min = 0,
      max = 500000,
      value = 0
    ),
    sliderInput(
      "op_out",
      "OON:",
      min = 0,
      max = 500000,
      value = 0
    )
    
    
    
  ),
  mainPanel(plotOutput("plans"),
            plotOutput("lines"))
))


server <- function(input, output) {
  plans <- read.csv("data.csv", header = T)
  
  unit_choice <- reactive({
    plans %>% filter(unit == input$unit)
  })
  
  output$plans <- renderPlot({
    ggplot(unit_choice(), aes(x = reorder(plan, cost), y = 26 * cost)) + geom_col() + xlab("") + theme_bw(base_size = 20) + ylab("Annual Cost")
  })
  
  
  
  output$lines <- renderPlot({
    cost_fun <- function(x, d, c, oop_max) {
      case_when((0 <= x) & (x < d) ~ x,
                (d <= d + c * (x - d)) &
                  (d + c * (x - d) < oop_max) ~ d + c * (x - d),
                TRUE ~ oop_max)
    }
    # need to find the asymptote by using max oop and coins
    
    asymptote_point <- function(d, c, o) {
      return(d + (o - d) / c)
    }
    
    asymptotes <- pmap(
      list(
        d = unit_choice()$deductible,
        c = unit_choice()$coins,
        o = unit_choice()$oop_max
      ),
      asymptote_point
    ) %>% unlist()
    
    x <-
      c(0, 50000, unit_choice()$deductible, asymptotes) |>
      unique() |>
      rep(each = nrow(unit_choice())) |>
      sort()
    
    
    data <-
      do.call("rbind",
              replicate(length(x) / nrow(unit_choice()), unit_choice(), simplify = F)) %>%
      mutate(`Medical Costs` = as.numeric(x)) %>%
      mutate(`OOP Cost` = as.numeric(pmap(
        list(
          x = as.numeric(x),
          d = as.numeric(deductible),
          c = as.numeric(coins),
          oop_max = as.numeric(oop_max)
        ),
        cost_fun
      )))
    
    print(data)
    
    ggplot(data, aes(x = `Medical Costs`, y = `OOP Cost`, colour = plan)) + geom_line() + facet_wrap(~
                                                                                                       network) + theme_bw(base_size = 20) + theme(legend.position =
                                                                                                                                                     "bottom", legend.title =
                                                                                                                                                     element_blank())
    
    
    
    # ggplot(y, aes(x = x, y = value, colour = variable)) + geom_line()
  })
  
  
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
