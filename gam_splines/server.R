library(shiny)

function(input, output, session) {
  
  num_gam <- reactiveVal(0)
  
  gam_list <- reactiveVal(c())
  
  model_list <- reactiveVal(list())
  
  serie_df <- reactive({
    req(input$serie)
    
    con_rue <- dbConnect(duckdb(), DUCK_DB_RUE, read_only=T)
    
    serie_df <- con_rue %>% 
      dbGetQuery(paste0("select * from vw_series_dia where serie like '", input$serie,"' order by data_entrada")) %>% 
      as_tibble() %>% 
      rename(DATA = data_entrada)
    
    con_rue %>% dbDisconnect(shutdown=TRUE)
    
    serie_df
  })
  
  observeEvent(input$gam_novo, {
    num_gam(num_gam() + 1)
    
    if (num_gam() <= 20) {
      list <- c(gam_list(), num_gam())
      
      gam_list(list)  
    } else {
      showModal(modalDialog(
        title = "Número máximo de modelos atingido."
      ))
    }
    
    
    
  })
  
  
  # lapply(X = 1:20, function(i) {
  #   observeEvent(
  #     input[[paste0("delete_", i)]], {
  #       list <- gam_list()
  #       list <- list[list != i]
  #       gam_list(list)
  #     }
  #   )
  # })

  observeEvent(input$fit_gam, {
    req(input$periodo)
    
    periodo <- input$periodo
    
    shinyjs::disable("serie")
    
    m_list <- model_list()
    i <- length(m_list) + 1
    
    tryCatch({
      model <- serie_df() %>%
        filter(DATA >= periodo[1],
               DATA <= periodo[2]) %>%
        fit_gam(
          daily_k = input$daily_k,
          daily_bs = input$daily_bs,
          weekly_k = input$weekly_k,
          weekly_bs = input$weekly_bs,
          family = input$family
        )
      
      
      
      m_list[[i]] <- model
      
      model_list(m_list)
      
      updateCheckboxGroupInput(
        session,
        "model_plot_list", 
        selected = c(input$model_plot_list, i)
      )
      
    }, error = function(e) {
      showModal(modalDialog(
        title = paste0("Erro no ajuste do modelo GAM"),
        p(paste(e, collapse = "\n"))
      ))
    })
  })
    
  # lapply(X = 1:20, function(i) {
  #   observeEvent(
  #     input[[paste0("fit_gam_", i)]], {
  #       
  #       req(input[[paste0("periodo_", i)]])
  #       
  #       periodo <- input[[paste0("periodo_", i)]]
  #       
  #       shinyjs::disable("serie")
  #       
  #       m_list <- model_list()
  #       
  #       tryCatch({
  #         model <- serie_df() %>% 
  #           filter(DATA >= periodo[1],
  #                  DATA <= periodo[2]) %>% 
  #           fit_gam(
  #             daily_k = input[[paste0("daily_k_", i)]], 
  #             daily_bs = input[[paste0("daily_bs_", i)]], 
  #             weekly_k = input[[paste0("weekly_k_", i)]], 
  #             weekly_bs = input[[paste0("weekly_bs_", i)]], 
  #             family = input[[paste0("family_", i)]]
  #           )
  #         
  #         m_list[[i]] <- model
  #         
  #         model_list(m_list)  
  #       }, error = function(e) {
  #         showModal(modalDialog(
  #           title = paste0("Erro no ajuste do modelo GAM ", i),
  #           p(paste(e, collapse = "\n"))
  #         ))
  #       })
  #       
  #       
  #     }
  #   )
  # })
  
  output$gam_models_ui <- renderUI({
    lapply(gam_list(), gam_tab)
  })
  
  output$model_plot_list_ui <- renderUI({
    list <- model_list()
    
    if (length(list) > 0) {
      indexes <- list %>% lapply(function(x) {!is.null(x)}) %>% unlist() %>% which()
      
      indexes <- setNames(indexes, paste0("GAM ", indexes))
      
      checkboxGroupButtons(
        "model_plot_list", 
        choices = indexes,
        selected = input$model_plot_list
      )
      
    }
    
    
  })
  
  output$serie_plot <- renderPlotly({
    
    g <- serie_df() %>% 
      ggplot(aes(x=DATA, y=n)) + 
      geom_point(shape=1, alpha = 0.3) + 
      theme_minimal()
    
    if (!is.null(model_list()) & !is.null(input$model_plot_list)) {
      m <- model_list()

      mod_df <-
        tibble(
          id = input$model_plot_list,
          model = m[as.numeric(input$model_plot_list)],
          fit = map(model, get_gam_fitted_values),
          link = map_chr(model, function(m) {m$family$link})
        ) %>% 
        select(id, fit, link) %>% 
        unnest(fit) %>% 
        mutate(SMOOTH = case_when(
          link == "log" ~ exp(TREND + CONSTANT),
          TRUE ~ TREND + CONSTANT
        ))
        
        

      if (!is.null(mod)) {
        g <- g +
          geom_line(
            data = mod_df,
            aes(DATA, SMOOTH, group=id, color=id)
          )
      }

    }
    
    g %>% ggplotly()
    
  })
  
  output$gam_list_out <- renderPrint({
    
    req(input$model_plot_list)
    
    m <- model_list()
    print(m[as.numeric(input$model_plot_list)])
  })
  
}
