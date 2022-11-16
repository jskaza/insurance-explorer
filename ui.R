library(shiny)

fluidPage(sidebarLayout(
  sidebarPanel(
    radioButtons("unit",
                 "Plan Type",
                 unique(plans$unit)),
    
    h4("Preventative Care Costs"),
    # sliderInput(
    #   "cost_preventative_in",
    #   "In-Network",
    #   min = 0,
    #   max = 5000,
    #   value = 0
    # ),
    
    sliderInput(
      "cost_preventative_oon",
      "OON",
      min = 0,
      max = 10000,
      value = 0
    ),
    
    
    h4("Primary Care"),
    sliderInput(
      "cost_primary_in",
      "In-Network Costs",
      min = 0,
      max = 10000,
      value = 0
    ),
    sliderInput(
      "n_primary_in",
      "In-Network Visits",
      min = 0,
      max = 50,
      value = 0
    ),
    sliderInput(
      "cost_primary_oon",
      "OON Cost",
      min = 0,
      max = 10000,
      value = 0
    ),
    
    h4("Specialists"),
    sliderInput(
      "cost_specialist_in",
      "In-Network Costs",
      min = 0,
      max = 10000,
      value = 0
    ),
    sliderInput(
      "n_specialist_in",
      "In-Network Visits",
      min = 0,
      max = 50,
      value = 0
    ),
    sliderInput(
      "cost_specialist_oon",
      "OON Costs",
      min = 0,
      max = 10000,
      value = 0
    ),
    
    h4("Virtual Care Costs"),
    sliderInput(
      "cost_virtual_in",
      "In-Network",
      min = 0,
      max = 10000,
      value = 0
    ),
    sliderInput(
      "cost_virtual_oon",
      "OON",
      min = 0,
      max = 10000,
      value = 0
    ),
    
    h4("Urgent Care"),
    sliderInput(
      "cost_urgent_in",
      "In-Network Costs",
      min = 0,
      max = 10000,
      value = 0
    ),
    sliderInput(
      "n_urgent_in",
      "In-Network Visits",
      min = 0,
      max = 50,
      value = 0
    ),
    sliderInput(
      "cost_urgent_oon",
      "OON Costs",
      min = 0,
      max = 10000,
      value = 0
    ),
    
    h4("Emergency Room"),
    sliderInput(
      "cost_er_in",
      "In-Network Costs",
      min = 0,
      max = 10000,
      value = 0
    ),
    sliderInput(
      "n_er_in",
      "In-Network Visits",
      min = 0,
      max = 50,
      value = 0
    ),
    sliderInput(
      "cost_er_oon",
      "OON Costs",
      min = 0,
      max = 10000,
      value = 0
    ),
    
    h4("Diagnostic Procedure Costs"),
    sliderInput(
      "cost_diagnostics_in",
      NULL,
      min = 0,
      max = 10000,
      value = 0
    ),
    sliderInput(
      "cost_diagnostics_oon",
      NULL,
      min = 0,
      max = 10000,
      value = 0
    ),
    
    h4("MRI/MRA, CT/PET Costs"),
    sliderInput(
      "cost_scans_in",
      "In-Network",
      min = 0,
      max = 10000,
      value = 0
    ),
    sliderInput(
      "cost_scans_oon",
      "OON",
      min = 0,
      max = 10000,
      value = 0
    ),
    
    h4("Inpatient Stay Costs"),
    sliderInput(
      "cost_ip_in",
      "In-Network",
      min = 0,
      max = 10000,
      value = 0
    ),
    sliderInput(
      "cost_ip_oon",
      "OON",
      min = 0,
      max = 10000,
      value = 0
    ),
    
    h4("Outpatient Surgery Costs"),
    sliderInput(
      "cost_op_in",
      "In-Network",
      min = 0,
      max = 10000,
      value = 0
    ),
    sliderInput(
      "cost_op_oon",
      "OON",
      min = 0,
      max = 10000,
      value = 0
    )
  ),
  mainPanel(tabsetPanel(
    tabPanel(
      "Cost Comparison",
      dataTableOutput("oop")
    ),
    tabPanel("Charts",
             plotOutput("plans"),
             plotOutput("lines")),
    tabPanel("Raw Data", dataTableOutput("data"))
  ))
))
