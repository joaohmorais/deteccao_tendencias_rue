library(shiny)
library(shinyWidgets)
library(shinyjs)
library(plotly)

source("globals.R")
source("gam_tab.R")
source("../aux_deteccao_tendencias_rue.R")


fluidPage(
  
    titlePanel("GAM modeling"),

    sidebarLayout(
        sidebarPanel(
          shinyjs::useShinyjs(),
            h3("Fonte de dados"),
            selectInput(
              "serie",
              label = "Série",
              choices = series_rue,
              selected = series_rue[1]
            ),
            
            
            fluidRow(
              column(9,
                     h3("GAM")
                     ),
              column(3#,
                     # div(style = "height:16px"),
                     # actionButton(
                     #   "gam_novo",
                     #   "Novo",
                     #   icon = icon("plus"),
                     #   class = "btn-success",
                     #   width = "100%"
                     # )
                     )
            ),
            #uiOutput("gam_models_ui"),
            gam_tab_ui
        ),

        
        mainPanel(
          uiOutput("model_plot_list_ui"),
          plotlyOutput("serie_plot"),
          fluidRow(
            column(6,
                   verbatimTextOutput("gam_list_out")
                   ),
            column(6,
                   plotOutput("dia_semana_plot")
                   )
          )
          
        )
    )
)
