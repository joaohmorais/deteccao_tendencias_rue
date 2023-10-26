gam_tab_ui <- tagList(
  wellPanel(
    sliderInput(
      paste0("periodo"), label = "Período", 
      min = datas_rue[1],
      max = datas_rue[2], 
      value = c(datas_rue[1], datas_rue[2]), 
      timeFormat = "%d/%m/%Y"
    ),
    fluidRow(
      h4(strong("Efeito temporal (~tempo)")),
      column(6,
             selectInput(
               paste0("weekly_bs"), 
               label = "Spline", 
               choices = c("thin plate (tp)" = "tp",
                           "cubic regression (cr)" = "cr",
                           "duchon splines (ds)" = "ds",
                           "cubic regression with shrinkage (cs)" = "cs",
                           "cyclic cubic regression" = "cc",
                           "b-spline (bs)" = "bs",
                           "p-spline (ps)" = "ps",
                           "cyclic p-spline (cp)" = "cp",
                           "random effects (re)" = "re"
               ), selected = "ps"
             )
      ),
      column(6,
             numericInput(
               paste0("weekly_k"),
               label = "k",
               value = 52,
               min = 1
             )
      )
    ),
    fluidRow(
      h4(strong("Efeito dia da semana (~dia_semana)")),
      column(6,
             selectInput(
               paste0("daily_bs"), 
               label = "Spline", 
               choices = c("thin plate (tp)" = "tp",
                           "cubic regression (cr)" = "cr",
                           "duchon splines (ds)" = "ds",
                           "cubic regression with shrinkage (cs)" = "cs",
                           "cyclic cubic regression" = "cc",
                           "b-spline (bs)" = "bs",
                           "p-spline (ps)" = "ps",
                           "cyclic p-spline (cp)" = "cp",
                           "random effects (re)" = "re"
               ), selected = "cr"
             )
      ),
      column(6,
             numericInput(
               paste0("daily_k"),
               label = "k",
               value = 7,
               min = 1
             )
      )
    ),
    selectInput(paste0("family"), "Família", 
                choices = c("gaussian", "poisson", "nb", "quasipoisson", "tw", "ziP"), 
                selected = "nb"
    ),
    fluidRow(
      column(6),
      column(6,
             actionButton(paste0("fit_gam"), label = "Ajustar modelo", icon = icon("plus"), class = "btn-success", width = "100%")
      )
    )
  )
)

gam_tab <- function(id) {
  tag_list <- tagList(
    wellPanel(
      fluidRow(
        column(10,
               h4(paste0("GAM ", id))
        ),
        column(2,
               div(style="height:8px"),
               actionButton(paste0("delete_", id), label = "", icon = icon("trash"), class = "btn-danger", width = "100%")
        )
      ),
      sliderInput(
        paste0("periodo_", id), label = "Período", 
        min = datas_rue[1],
        max = datas_rue[2], 
        value = c(datas_rue[1], datas_rue[2]), 
        timeFormat = "%d/%m/%Y"
      ),
      fluidRow(
        h4(strong("Efeito temporal (~tempo)")),
        column(6,
               selectInput(
                 paste0("weekly_bs_", id), 
                 label = "Spline", 
                 choices = c("thin plate (tp)" = "tp",
                             "cubic regression (cr)" = "cr",
                             "duchon splines (ds)" = "ds",
                             "cubic regression with shrinkage (cs)" = "cs",
                             "cyclic cubic regression" = "cc",
                             "b-spline (bs)" = "bs",
                             "p-spline (ps)" = "ps",
                             "cyclic p-spline (cp)" = "cp",
                             "random effects (re)" = "re"
                 ), selected = "ps"
               )
        ),
        column(6,
               numericInput(
                 paste0("weekly_k_", id),
                 label = "k",
                 value = 52,
                 min = 1
               )
        )
      ),
      fluidRow(
        h4(strong("Efeito dia da semana (~dia_semana)")),
        column(6,
               selectInput(
                 paste0("daily_bs_", id), 
                 label = "Spline", 
                 choices = c("thin plate (tp)" = "tp",
                             "cubic regression (cr)" = "cr",
                             "duchon splines (ds)" = "ds",
                             "cubic regression with shrinkage (cs)" = "cs",
                             "cyclic cubic regression" = "cc",
                             "b-spline (bs)" = "bs",
                             "p-spline (ps)" = "ps",
                             "cyclic p-spline (cp)" = "cp",
                             "random effects (re)" = "re"
                 ), selected = "cr"
               )
        ),
        column(6,
               numericInput(
                 paste0("daily_k_", id),
                 label = "k",
                 value = 7,
                 min = 1
               )
        )
      ),
      selectInput(paste0("family_", id), "Família", 
                  choices = c("gaussian", "poisson", "nb", "quasipoisson", "tw", "ziP"), 
                  selected = "nb"
      ),
      fluidRow(
        column(6),
        column(6,
               actionButton(paste0("fit_gam_", id), label = "Ajustar modelo", icon = icon("arrow-down"), class = "btn-success", width = "100%")
        )
      )
    )
  )
}