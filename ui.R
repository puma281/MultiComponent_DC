constant_1 <- read.csv("data/antconst.csv")
constant <- constant_1[,1:5]
ui<- fluidPage(
  titlePanel("MultiComponent Distillation"),
  numericInput(
    "criteria_count",
    label = "Number of Components (min =2,max = 3)",
    2,
    min = 2,
    max = 5
  ),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.criteria_count == '2'",
        fluidRow(
          column(6,
                 selectInput(
                   "var_2_1",
                   label = h3("Component 1"),
                   choices = constant[, 1],
                   selected = constant[28,1]
                 ),
                 selectInput(
                   "var_2_2",
                   label = h3("Component 2"),
                   choices = constant[, 1],
                   selected = constant[32,1]
                 ),
                 
                 # Only show this panel if Custom is selected
                 conditionalPanel(
                   condition = "input.breaks == 'custom'",
                   sliderInput("breakCount", "Break Count", min=1, max=1000, value=10)
                 )
                 
          ),
          
          column(6,
                 numericInput("per_2_1", 
                              h3("feed percentage"), 
                              value = 50),
                 numericInput("per_2_2", 
                              h3("feed percentage"), 
                              value = 50)),
          
        )
      ),
      conditionalPanel(
        condition = "input.criteria_count == '3'",
        fluidRow(
          column(6,
                 selectInput(
                   "var_3_1",
                   label = h3("Component 1"),
                   choices = constant[, 1],
                   selected = constant[28,1]
                 ),
                 selectInput(
                   "var_3_2",
                   label = h3("Component 2"),
                   choices = constant[, 1],
                   selected = constant[32,1]
                 ),
                 selectInput(
                   "var_3_3",
                   label = h3("Component 3"),
                   choices = constant[, 1],
                   selected = constant[36,1]
                 ),
                 
                 # Only show this panel if Custom is selected
                 conditionalPanel(
                   condition = "input.breaks == 'custom'",
                   sliderInput("breakCount", "Break Count", min=1, max=1000, value=10)
                 )
                 
          ),
          
          column(6,
                 numericInput("per_3_1", 
                              h3("feed percentage"), 
                              value = 33),
                 numericInput("per_3_2", 
                              h3("feed percentage"), 
                              value = 33),
                 numericInput("per_3_3", 
                              h3("feed percentage"), 
                              value = 34)),
          
        )
      ),
      conditionalPanel(
        condition = "input.criteria_count == '4'",
        fluidRow(
          column(6,
                 selectInput(
                   "var_4_1",
                   label = h3("Component 1"),
                   choices = constant[, 1],
                   selected = constant[28,1]
                 ),
                 selectInput(
                   "var_4_2",
                   label = h3("Component 2"),
                   choices = constant[, 1],
                   selected = constant[32,1]
                 ),
                 selectInput(
                   "var_4_3",
                   label = h3("Component 3"),
                   choices = constant[, 1],
                   selected = constant[36,1]
                 ),
                 selectInput(
                   "var_4_4",
                   label = h3("Component 4"),
                   choices = constant[, 1],
                   selected = constant[48,1]
                 ),
                 
                 # Only show this panel if Custom is selected
                 conditionalPanel(
                   condition = "input.breaks == 'custom'",
                   sliderInput("breakCount", "Break Count", min=1, max=1000, value=10)
                 )
                 
          ),
          
          column(6,
                 numericInput("per_4_1", 
                              h3("feed percentage"), 
                              value = 25),
                 numericInput("per_4_2", 
                              h3("feed percentage"), 
                              value = 25),
                 numericInput("per_4_3", 
                              h3("feed percentage"), 
                              value = 25),
                 numericInput("per_4_4", 
                              h3("feed percentage"), 
                              value = 25)),
          
        )
      ),
      conditionalPanel(
        condition = "input.criteria_count == '5'",
        fluidRow(
          column(6,
                 selectInput(
                   "var_5_1",
                   label = h3("Component 1"),
                   choices = constant[, 1],
                   selected = constant[28,1]
                 ),
                 selectInput(
                   "var_5_2",
                   label = h3("Component 2"),
                   choices = constant[, 1],
                   selected = constant[32,1]
                 ),
                 selectInput(
                   "var_5_3",
                   label = h3("Component 3"),
                   choices = constant[, 1],
                   selected = constant[36,1]
                 ),
                 selectInput(
                   "var_5_4",
                   label = h3("Component 4"),
                   choices = constant[, 1],
                   selected = constant[48,1]
                 ),
                 selectInput(
                   "var_5_5",
                   label = h3("Component 5"),
                   choices = constant[, 1],
                   selected = constant[52,1]
                 ),
                 
                 # Only show this panel if Custom is selected
                 conditionalPanel(
                   condition = "input.breaks == 'custom'",
                   sliderInput("breakCount", "Break Count", min=1, max=1000, value=10)
                 )
                 
          ),
          
          column(6,
                 numericInput("per_5_1", 
                              h3("feed percentage"), 
                              value = 20),
                 numericInput("per_5_2", 
                              h3("feed percentage"), 
                              value = 20),
                 numericInput("per_5_3", 
                              h3("feed percentage"), 
                              value = 20),
                 numericInput("per_5_4", 
                              h3("feed percentage"), 
                              value = 20),
                 numericInput("per_5_5", 
                              h3("feed percentage"), 
                              value = 20)
          ),
          
        )
      )),
    mainPanel(h2("Distillation"),
              fluidRow(
                h4("Select two adjacent components as LK and HK from the below table in which Boiling points are in ascending order"),
                column(3,
                       h4("select LK component from the order below"),
                       selectInput("sel_1",
                                   h3("Light Key"),
                                   choices = list("Component 1(Orderd)" = 1, "Component 2(Ordered)" = 2,
                                                  "Component 3(Orderd)" = 3,"Component 4(Orderd)" = 4,"Component 5(Orderd)" = 5), selected = 1)),
                column(3,
                       h4("select HK component orderd below"),
                       selectInput("sel_2",
                                   h3("Heavy Key"),
                                   choices = list("Component 1(Orderd)" = 1, "Component 2(Orderd)" = 2,
                                                  "Component 3(Orderd)" = 3,"Component 4(Orderd)" = 4,"Component 5(Orderd)" = 5), selected = 2)),
                
              ),
              h3("Data Table of the constants and Boiling Points in ascending order"),
              tableOutput("tablu"),
              fluidRow(
                column(3,
              h4("LK- Light key Component"),
              
              numericInput("rec_1", 
                           h3("Distillate Recovery"), 
                           value = 95)),
              column(3,
              h4("HK- High Key Component"),
              numericInput("rec_2", 
                           h3("Bottom Recovery"), 
                           value = 95)),
              column(3,
                     h4("Input feed Pressure"),
              numericInput("press", 
                           h3(" Pressure(in atm)"), 
                           value = 1)),
              column(3,
                     h4("Quality of feed"),
                     numericInput("qual", 
                                  h3(" q(feed quality)"), 
                                  value = 0))
              ),
              h4("Relative volatility of LK obtained by average of top and bottom volatilities:"),
              h4(textOutput("rel_volt")),
              h4("Minimum No. of Plates requiried obtained by Fenske Equation:"),
              h4(textOutput("min_platty")),
              h4("Minimum reflux ratio obtained by underwood equation:"),
              h4(textOutput("ref_ratio")),
              numericInput("org_reflex", 
                           h3(" Ratio of Actual Reflux Ratio to Reflex Ratio"), 
                           value = 1.5),
              h4("Actual No.of plates obtained by the Gilliland Correlation:"),
              h2(textOutput("act_plat")),
              
              )
              
    )
  )


