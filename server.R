shinyServer(function(input, output) {
    
    ##--Not in select fuction
    '%!in%' <- function(x, y)
        ! ('%in%'(x, y))
    
    ##--Round all function
    round_df <- function(df, digits) {
        nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
        
        df[,nums] <- round(df[,nums], digits = digits)
        
        (df)
    }
    
    ##----------------------------------------------------------------------------------------------
    ##--Data Load and Prep
    ##----------------------------------------------------------------------------------------------
    
    cat("\n")
    cat("Loading Data . . .")
    cat("\n")
    
    ##--Load Data
    population_data <- read.fst('world_population_shiny_data_prepared.fst')
    
    cat("\n")
    cat("Generating Options . . .")
    cat("\n")
    
    options <- population_data %>% 
        .$Country %>% 
        unique() %>% 
        as.character()
    
    ##----------------------------------------------------------------------------------------------
    ##--Create UI Components
    ##----------------------------------------------------------------------------------------------
    
    cat("\n")
    cat("Creating Select Country UI Component . . .")
    cat("\n")
    
    output$select_country <- renderUI({
        
        if(input$tabs == 'prediction'){
            
            pickerInput('select_country',
                        "Select Country:",
                        choices = options,
                        selected = 'All Countries',
                        options =
                            list(`actions-box` = TRUE,
                                 `live-search` = TRUE
                                ),
                        choicesOpt = list(
                            style = rep(("color: black;
                                         background: #E9E9E9;
                                         font-weight: bold;"),500)
                        ),
                        multiple = TRUE)
            
        }
        
    })
    
    cat("\n")
    cat("Creating Train Dates UI Component . . .")
    cat("\n")
    
    output$train_dates <- renderUI({
        
        if(input$tabs == 'prediction'){
            
            sliderInput('train_dates',
                        'Select Date Range to Train Model',
                        min = min(population_data$Year),
                        max = max(population_data$Year),
                        value = c(min(population_data$Year),max(population_data$Year)),
                        step = 1,
                        sep = '',
                        ticks = FALSE)
            
        }
        
    })
    
    cat("\n")
    cat("Creating Predict Until UI Component . . .")
    cat("\n")
    
    output$predict_until <- renderUI({
        
        if(input$tabs == 'prediction'){
            
            pickerInput('predict_until',
                        'Select Year to Predict Until',
                        choices = as.integer(2017:2100),
                        selected = 2050,
                        options = list(`actions-box` = TRUE,
                                       `live-search` = TRUE
                        ),
                        choicesOpt = list(
                            style = rep(("color: black;
                                         background: #E9E9E9;
                                         font-weight: bold;"),500)
                        ),
                        multiple = FALSE)
            
        }
        
    })
    
    output$map_year <- renderUI({
        
        pickerInput('map_year',
                    'Select Year to Display Population',
                    choices = population_data$Year,
                    selected = max(population_data$Year),
                    options = list(`actions-box` = TRUE,
                                   `live-search` = TRUE
                    ),
                    choicesOpt = list(
                        style = rep(("color: black;
                                     background: #E9E9E9;
                                     font-weight: bold;"),500)
                    ),
                    multiple = FALSE)
        
    })
    
    ##----------------------------------------------------------------------------------------------
    ##--Prediction
    ##----------------------------------------------------------------------------------------------
    
    cat("\n")
    cat("Configuring Prophet Model . . .")
    cat("\n")
    
    ##--Prophet Model Config
    prophet_configuration <- function(yfo,
                                      no_changepoints,
                                      changepoint_prior_scale,
                                      seasonality_prior_scale,
                                      changepoint_range){
        
        model <- prophet(
            yearly.seasonality = FALSE,
            weekly.seasonality = FALSE,
            daily.seasonality = FALSE,
            n.changepoints = no_changepoints,
            changepoint_prior_scale = changepoint_prior_scale,
            changepoint.range = changepoint_range,
            seasonality.prior.scale = seasonality_prior_scale
        )
        
        ##--Add Yearly Seasonality
        model <- add_seasonality(model,
                                 name = 'yearly',
                                 period = 1,
                                 fourier.order = yfo)
        
        ##--The model object once all modification is complete should be returned.
        return(model)
    }
    
    cat("\n")
    cat("Calling Prediction Function . . .")
    cat("\n")
    
    prophet_data <- reactive({
        
        cat("\n")
        cat("Assigning Dates . . .")
        cat("\n")
        
        ##--Get Dates From Inputs
        train_from_year <- as.integer(input$train_dates[1])
        train_until_year <- as.integer(input$train_dates[2])
        predict_until_year <- as.integer(input$predict_until)
        
        train_from <- as.Date(paste0(train_from_year, '-01-01'))
        train_until <- as.Date(paste0(train_until_year, '-01-01'))
        predict_until <- as.Date(paste0(predict_until_year, '-01-01'))
        
        cat("\n")
        cat("Creating Prophet Data . . .")
        cat("\n")
        
        ##--Prepare Training Data
        prophet_data <- population_data %>% 
            filter(Country %in% input$select_country) %>%
            mutate(ds = as.Date(paste0(Year, '-01-01'))) %>% 
            filter(ds >= train_from) %>% 
            filter(ds <= train_until) %>%
            group_by(ds) %>%
            summarise(y = sum(Value)) %>% 
            select(ds, y)
        
    })
    
    prophet_validate <- reactive({
        
        cat("\n")
        cat("Assigning Dates . . .")
        cat("\n")
        
        train_from_year <- as.integer(input$train_dates[1])
        train_until_year <- as.integer(input$train_dates[2])
        predict_until_year <- as.integer(input$predict_until)
        
        train_from <- as.Date(paste0(train_from_year, '-01-01'))
        train_until <- as.Date(paste0(train_until_year, '-01-01'))
        predict_until <- as.Date(paste0(predict_until_year, '-01-01'))
        
        cat("\n")
        cat("Creating Prophet Validate . . .")
        cat("\n")
        
        ##--Prepare Validation Data
        prophet_validate <-  population_data %>% 
            filter(Country %in% input$select_country) %>%
            mutate(ds = as.Date(paste0(Year, '-01-01'))) %>%  
            filter(ds > train_until) %>% 
            filter(ds <= predict_until) %>% 
            group_by(ds) %>% 
            summarise(y = sum(Value)) %>% 
            select(ds, y)
        
    })
    
    prophet_model <- reactive({
        
        yfo <- 30
        no_changepoints <- 50
        changepoint_prior_scale <- 0.1
        seasonality_prior_scale <- 10
        changepoint_range = 0.8
        
        cat("\n")
        cat("Building Prophet Model . . .")
        cat("\n")
        
        model <- prophet_configuration(yfo,
                                       no_changepoints,
                                       changepoint_prior_scale,
                                       seasonality_prior_scale,
                                       changepoint_range)
        
        model <- model %>%
            fit.prophet(prophet_data())
        
    })
    
    prophet_future_df <- reactive({
        
        train_until_year <- as.integer(input$train_dates[2])
        predict_until_year <- as.integer(input$predict_until)
        
        cat("\n")
        cat("Creating Periods . . .")
        cat("\n")
        
        periods = as.integer(predict_until_year - train_until_year)
        
        cat("\n")
        cat("Creating Future df . . .")
        cat("\n")
        
        future_df <- make_future_dataframe(prophet_model(),
                                           periods = periods,
                                           freq = 'year')
        
    })
    
    prophet_prediction <- reactive({
        cat("\n")
        cat("Generating Prediction . . .")
        cat("\n")
        
        # Generate predictions.
        prediction <- predict(prophet_model(), prophet_future_df())
        
        prediction <- prediction %>% 
            mutate(yhat = if_else(yhat < 0,0,yhat),
                   yhat_lower = if_else(yhat_lower < 0,0,yhat_lower),
                   yhat_upper = if_else(yhat_upper < 0,0,yhat_upper))
        
        
    })
    
    generate_plot <- eventReactive(input$plot, {
        
        steps <- 4
        
        withProgress(message = 'Creating Prediction Plot . . .', min = 0, max = steps, value = 1,  {
            
            incProgress(1/steps, detail = paste("Building Model . . ."))
            
            cat("\n")
            cat("Creating Plot Scales . . .")
            cat("\n")
            
            train_from_year <- as.integer(input$train_dates[1])
            predict_until_year <- as.integer(input$predict_until)
            
            num_years <- as.integer(predict_until_year - train_from_year)
            
            if(num_years > 30) {
                date_breaks = "10 year"; date_labels = "%Y"
            } else if(num_years > 20) {
                date_breaks = "5 year"; date_labels = "%Y"
            } else if(num_years > 10) {
                date_breaks = "2 year"; date_labels = "%Y"
            } else {
                date_breaks = "1 year"; date_labels = "%Y"
            }
            
            incProgress(2/steps, detail = paste("Predicting Selected Years . . ."))
            
            history <- prophet_model()$history %>%
                select(ds, y) %>%
                inner_join(prophet_prediction() %>%
                               select(ds, trend), by = "ds") %>%
                mutate(
                    residual = y - trend
                )
            
            history_sd = sd(history$residual)
            
            cat("\n")
            cat("Generating Plot . . .")
            cat("\n")
            
            incProgress(3/steps, detail = paste("Finalizing Plot . . ."))
            
            prediction_plot <- plot(prophet_model(), prophet_prediction()) +
                scale_x_datetime("", date_breaks = date_breaks, date_labels = date_labels) +
                labs(title = paste('Population Prediction Plot -', input$select_country),
                     y = paste("Population",'\n')) +
                theme(text = element_text(size = 16),
                      plot.background = element_rect(fill = "#ebebeb"))
            
            incProgress(3/steps, detail = paste("Done"))
            
        })
        
        ggplotly(prediction_plot)
        
    })
    
    cat("\n")
    cat("Creating Plot . . .")
    cat("\n")
    
    output$prediction_plot <- renderPlotly({
        
        generate_plot()
        
    })
    
    output$prediction_table <- DT::renderDataTable({
        
        kable_title <- paste('Population Prediction Table -', input$select_country)
        
        DT::datatable(prophet_prediction() %>%
                          mutate(Date = year(ds),
                                 Prediction = input$select_country) %>% 
                          select(Date,
                                 Prediction,
                                 Trend = trend) %>% 
                          arrange(desc(Date)),
                      options=list(pageLength = 25,
                                   search = list(regex = TRUE),
                                   autoWidth = TRUE,
                                   columnDefs = list(list(className = 'dt-center', targets = 0:3))
                      ),
                      caption = htmltools::tags$caption(kable_title,
                                                        style="color:black;font-size:20px")) %>% 
            formatCurrency(3, currency = "", interval = 3, mark = "`", digits = 0) 
        
    })
    
    output$pop_map <- renderLeaflet({
        
        map_data <- population_data %>% 
            filter(Year == input$map_year,
                   Country != 'All Countries')
        
        map <- map_data %>% 
            leaflet() %>% 
            addTiles() %>%
            addProviderTiles("OpenMapSurfer.Roads") %>%
            addCircleMarkers(lng = map_data$Longitude,
                             lat = map_data$Latitude,
                             weight = 2,
                             radius = sqrt(map_data$Value)/500,
                             color = map_data$pop_color,
                             fillColor = map_data$pop_color,
                             popup = paste('Country:', 
                                           toupper(map_data$Country),
                                           '<br>',
                                           'Year:',
                                           map_data$Year,
                                           '<br>',
                                           'Population:',
                                           format(map_data$Value, big.mark = "'"))) %>% 
            setView(lat = 0, lng = 0, zoom = 2.5) %>% 
            addLegend(title = 'Population',
                      labels = c('Less Than 5M',
                                 'Between 5M and 10M',
                                 'Between 10M and 20M',
                                 'Between 20M and 50M',
                                 'Between 50M and 100M',
                                 'Between 100M and 350M',
                                 'More than 1B'),
                      colors = c('red',
                                 'orange',
                                 'yellow',
                                 'lightblue',
                                 'darkblue',
                                 'lightgreen',
                                 'darkgreen'),
                      position = 'topright')
        
        map
        
    })
    
    output$tab_selection <- renderImage({
        
        return(list(
            src = 'tab_selection.jpg',
            contentType = "image/jpg",
            alt = "Tab Selection",
            height = 200
        ))
        
    }, deleteFile = FALSE)
    
    output$prediction <- renderImage({
        
        return(list(
            src = 'prediction.jpg',
            contentType = "image/jpg",
            alt = "Prediction",
            height = 400
        ))
        
    }, deleteFile = FALSE)
    
    output$prediction_data <- renderImage({
        
        return(list(
            src = 'prediction_table.jpg',
            contentType = "image/jpg",
            alt = "Prediction Table",
            height = 400
        ))
        
    }, deleteFile = FALSE)
    
    output$map_screenshot <- renderImage({
        
        return(list(
            src = 'map.jpg',
            contentType = "image/jpg",
            alt = "Map",
            height = 400
        ))
        
    }, deleteFile = FALSE)
    
})
