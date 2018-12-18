##############################################################################
#Project: optimisation - results pipeline
#Description: results visualiser for a/b testing
#Author: Remy Maguire
#Date: 24/09/2018
##############################################################################
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("proto")
#install.packages("tcltk2")
#install.packages("gsubfn")
#install.packages("RSQLite")
#install.packages("sqldf")
#install.packages("magrittr")
#install.packages("tidyr")
#install.packages("XML2")
#install.packages("eeptools")
#install.packages("data.table")
#install.packages("reshape")
#install.packages("stringi")
#install.packages("googlesheets")
#install.packages("rJava")
#install.packages("DBI")
#install.packages("RJDBC")
#install.packages("RPostgreSQL")
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("googleAnalyticsR")
#install.packages("Hmisc")
##install.packages('ggplot2')
#install.packages("highcharter")
#install.packages("plotly")
#install.packages("scales")
#install.packages("googleVis")
#install.packages("devtools")
#install.packages("splitstackshape")
#install.packages("zoo")
#install.packages("knitr")
#install.packages("V8")
#install.packages("shinyjs")
library(knitr)
library(devtools)
library(zoo)
library(Rcpp)
library(ggplot2)
library(googleAnalyticsR)
library(shiny)
library(shinyjs)
library(Hmisc)
library(shinydashboard)
library(highcharter)
library(plotly)
library(scales)
#library(dplyr)
library(plyr)
library(splitstackshape)
library(reshape2)
library(lubridate)
library(rsconnect)
library(aws.s3)
library(aws.signature)
library(data.table)
library(DT)
library(sqldf)
library(DBI)
library(RPostgreSQL)
library(formattable)
library(plotly)
options(sqldf.driver = "SQLite")


#setwd("C:/Users/remy.maguire/Google Drive/Projects/Product_Analytics_Dashboarding/Apps/Metrics_Dashboard")
#setwd("/home/remy/pbx_ab_testing_portal/results_pipeline/") # don't set this - this doesn't exist on the server

graphics.off()
pdf(NULL)

#data <- read.csv("custom_results_data_20180921.csv")

#x = aws.signature::locate_credentials()

#Sys.setenv(
#  #'AWS_SESSION_TOKEN' = x['session_token'],
 #          'AWS_SECRET_ACCESS_KEY' = x['secret'],
#           'AWS_ACCESS_KEY_ID' = x['key'],
#           'AWS_DEFAULT_REGION' = '')#


bucket_name<-"bi-photobox-analytics-eu"
aws_access_key = 'xxx'
aws_secret_key='xxx'
Sys.setenv('AWS_SECRET_ACCESS_KEY' =aws_secret_key,
           'AWS_ACCESS_KEY_ID' = aws_access_key
           ,'AWS_DEFAULT_REGION' ='eu-west-1'
)


########################################################################
######################################################################################################################
######################################################################################################################
ui <- fluidPage(
  
  titlePanel("PBX Optimisation Squad - A/B Testing Results"),
    mainPanel(
      tabsetPanel(
        tabPanel("Results: running & recent", 
                 
              fluidRow(
                   
                column(2,
                       wellPanel(
                         radioButtons("results_filter_country", "Country:",
                                      choices = c("All","GB","FR")),
                         radioButtons("results_filter_device", "Device:",
                                      choices = c("All","Mobile","Desktop","Tablet")))), 
                   column(10,
                 dataTableOutput("results_table"),
                  downloadButton("downloadData", "Download")))),
        tabPanel("Results: historical", 
                 
                 fluidRow(
                   
                   column(2,
                          wellPanel(
                            radioButtons("historical_results_filter_country", "Country:",
                                         choices = c("All","GB","FR")),
                            radioButtons("historical_results_filter_device", "Device:",
                                         choices = c("All","Mobile","Desktop","Tablet")))), 
                   column(10,
                          dataTableOutput("historical_results_table"),
                          downloadButton("historical_downloadData", "Download")))),
        tabPanel("Plots",
                 
                 fluidRow(
                   
                   column(3,
                          wellPanel(
                            uiOutput("running_experiment_dropdown"),
                            radioButtons("plots_filter_country", "Country:",
                                         choices = c("All","GB","FR")),
                            radioButtons("plots_filter_device", "Device:",
                                         choices = c("All","Mobile","Desktop","Tablet")),
                            radioButtons("metric", "Metric:",list("MPU (£)"="a", "CR (%)"="b","SPU (£)"="c")),
                            htmlOutput("status_box"))
                   ),
                   column(9,
                          fluidRow(
                            box(htmlOutput("conf_box")),
                            box(htmlOutput("lift_box"))),
                          fluidRow(
                          plotOutput("conf_plot", 
                                     height = "300px",
                                     width = "1000px"),
                          plotOutput("lift_plot", 
                                     height = "300px",
                                     width = "1000px"))
                         # ,
                        #  fluidRow(
                        #    box(htmlOutput("status_box")))
                   )
                 )
        )
    )
  )
)


######################################################################################################################
######################################################################################################################
#Server (data) code
server <- shinyServer(function(input, output){
  
  ######################################################################################################
  
  output$results_table<-renderDataTable({
    
    data_raw <- get_object("s3://bi-photobox-analytics-eu/remy/pbx - ab testing portal/results_data.csv", bucket = "bi-photobox-analytics-eu", check_region = FALSE)
    data <- fread(rawToChar(data_raw))
    
    data <- data[     data$device == input$results_filter_device
                    & data$country == input$results_filter_country
                    & ( data$end_date >= Sys.Date()-21 | data$completion_text == "Running"),
                    ]

    table <- datatable(data
                       #[,2:20]
                       , 
                       options = list(columnDefs = list(list(targets = c(0,20,21,22,24,25,26), visible = FALSE)),
                                      order = list(list(25, 'desc'),list(1, 'asc'),list(2, 'asc'), list(4, 'asc'),list(5,'asc'))
                                      ), 
              colnames = c('Index','Experiment Name', 'Variant ID','#Days Run','Country','Device','#Control Users','#Variant Users',
              'Control %CR','Variant %CR','%Delta CR','%Confidence CR',
              'Control £MPU','Variant £MPU','%Delta MPU','%Confidence MPU',
              'Control £SPU','Variant £SPU','%Delta SPU','%Confidence SPU','CR conf. flag','MPU conf. flag','SPU conf. flag','Status', 'Completion Flag','Start Date','End Date')
              )
    table %>%
    formatPercentage(c('control_cr','v_cr','delta_cr','v_conf_cr','delta_mpu','v_conf_mpu','delta_spu','v_conf_spu'), 2) %>%
    formatCurrency(c('control_mpu','v_mpu','control_spu' ,'v_spu'), '£')  %>%
      formatCurrency(c('control_n','v_n'),currency = "",interval = 3, mark = ",",digits = 0) %>% 
      formatStyle('v_conf_cr', 'cr_conf_flag',
                  backgroundColor = styleEqual(c(0, 1,-1), c('gainsboro', 'mediumseagreen','tomato'))) %>% 
      formatStyle('v_conf_mpu', 'mpu_conf_flag',
                  backgroundColor = styleEqual(c(0, 1,-1), c('gainsboro', 'mediumseagreen','tomato'))) %>% 
      formatStyle('v_conf_spu', 'spu_conf_flag',
                  backgroundColor = styleEqual(c(0, 1,-1), c('gainsboro', 'mediumseagreen','tomato'))) %>% 
      formatStyle('completion_text', 'completion_flag',
                  backgroundColor = styleEqual(c(0, 1), c('khaki', 'darkseagreen')))

    
  })
  
  ######################################################################################################
  
  output$historical_results_table<-renderDataTable({
    
    data_raw <- get_object("s3://bi-photobox-analytics-eu/remy/pbx - ab testing portal/results_data.csv", bucket = "bi-photobox-analytics-eu", check_region = FALSE)
    data <- fread(rawToChar(data_raw))
    
    data <- data[     data$device == input$historical_results_filter_device
                      & data$country == input$historical_results_filter_country
                      & data$completion_text == "Completed",
                      ]
    
    table <- datatable(data
                       #[,2:20]
                       , 
                       options = list(columnDefs = list(list(targets = c(0,20,21,22,24,25,26), visible = FALSE)),
                                      order = list(list(25, 'desc'),list(1, 'asc'),list(2, 'asc'), list(4, 'asc'),list(5,'asc'))
                       ), 
                       colnames = c('Index','Experiment Name', 'Variant ID','#Days Run','Country','Device','#Control Users','#Variant Users',
                                    'Control %CR','Variant %CR','%Delta CR','%Confidence CR',
                                    'Control £MPU','Variant £MPU','%Delta MPU','%Confidence MPU',
                                    'Control £SPU','Variant £SPU','%Delta SPU','%Confidence SPU','CR conf. flag','MPU conf. flag','SPU conf. flag','Status', 'Completion Flag','Start Date','End Date')
    )
    table %>%
      formatPercentage(c('control_cr','v_cr','delta_cr','v_conf_cr','delta_mpu','v_conf_mpu','delta_spu','v_conf_spu'), 2) %>%
      formatCurrency(c('control_mpu','v_mpu','control_spu' ,'v_spu'), '£')  %>%
      formatCurrency(c('control_n','v_n'),currency = "",interval = 3, mark = ",",digits = 0) %>% 
      formatStyle('v_conf_cr', 'cr_conf_flag',
                  backgroundColor = styleEqual(c(0, 1,-1), c('gainsboro', 'mediumseagreen','tomato'))) %>% 
      formatStyle('v_conf_mpu', 'mpu_conf_flag',
                  backgroundColor = styleEqual(c(0, 1,-1), c('gainsboro', 'mediumseagreen','tomato'))) %>% 
      formatStyle('v_conf_spu', 'spu_conf_flag',
                  backgroundColor = styleEqual(c(0, 1,-1), c('gainsboro', 'mediumseagreen','tomato'))) %>% 
      formatStyle('completion_text', 'completion_flag',
                  backgroundColor = styleEqual(c(0, 1), c('khaki', 'darkseagreen')))
    
    
  })
  
  ########################################################
  
  output$running_experiment_dropdown <-renderUI({
    
    data_raw <- get_object("s3://bi-photobox-analytics-eu/remy/pbx - ab testing portal/results_data.csv", bucket = "bi-photobox-analytics-eu", check_region = FALSE)
    data <- fread(rawToChar(data_raw))
    
    data <- data[order(-start_date),]
    
    selectInput("running_experiment_dropdown","Select experiment:",
                c(as.character(unique(data$experiment_name) )),
                multiple=FALSE, selected = FALSE)
  })
  
  ########################################################
  
  output$conf_plot <- renderPlot({
    
    
    data_raw <- get_object("s3://bi-photobox-analytics-eu/remy/pbx - ab testing portal/daily_results_data.csv", bucket = "bi-photobox-analytics-eu", check_region = FALSE)
    data <- fread(rawToChar(data_raw))  
    
    
    data <- data[   data$experiment_name == input$running_experiment_dropdown 
                                    & data$device == input$plots_filter_device
                                    & data$country == input$plots_filter_country,
                                    ]
    
    
    chart_data <- data
    
    
    if(input$metric=='a') {chart_data$metric <- chart_data$v_conf_mpu}
    if(input$metric=='b') {chart_data$metric <- chart_data$v_conf_cr}
    if(input$metric=='c') {chart_data$metric <- chart_data$v_conf_spu}
    
    
    title_text <- paste("CONFIDENCE PLOT | ","Experiment name: ",input$running_experiment_dropdown," | Country: ",input$plots_filter_country," | Device: ",input$plots_filter_device
                        #," | Variant: ",variant_id,sep = ""
                        )
  
    plot <- ggplot(chart_data, aes(days_run)) + 
      theme_bw() +
      geom_line(aes(y = metric, group = variant_id, colour = factor(variant_id))) + 
      ylab("") +
      xlab("Days run") +
      geom_hline(yintercept = 0.90,color = "deepskyblue3",size=1,linetype="dashed") + 
      geom_hline(yintercept = -0.90,color = "firebrick",size=1,linetype="dashed") + 
      labs(title = title_text) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10),labels = percent_format(), limits=c(0,1))+ 
      theme(plot.title = element_text(size =12),
            legend.title=element_text(size=12), 
            legend.text=element_text(size=12)) +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12)) +
      theme(plot.title = element_text(size=14)) +
      theme(axis.title.x = element_text(size=12),
            axis.text.x  = element_text(size=12)) +
      theme(legend.position="bottom") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10),limits = c(0,28))+
     labs(colour = "Contender variant") 
    plot
    
  })
  
  ########################################################
  
  output$lift_plot <- renderPlot({
    
    
    data_raw <- get_object("s3://bi-photobox-analytics-eu/remy/pbx - ab testing portal/daily_results_data.csv", bucket = "bi-photobox-analytics-eu", check_region = FALSE)
    data <- fread(rawToChar(data_raw))  
    
    
    data <- data[   data$experiment_name == input$running_experiment_dropdown 
                    & data$device == input$plots_filter_device
                    & data$country == input$plots_filter_country,
                    ]
    
    chart_data <- data
    
    
    if(input$metric=='a') {chart_data$metric <- (chart_data$v_mpu - chart_data$control_mpu)/ chart_data$control_mpu}
    if(input$metric=='b') {chart_data$metric <- (chart_data$v_cr - chart_data$control_cr)/ chart_data$control_cr}
    if(input$metric=='c') {chart_data$metric <- (chart_data$v_spu - chart_data$control_spu)/ chart_data$control_spu}
    
    
    title_text <- paste("LIFT PLOT | ","Experiment name: ",input$running_experiment_dropdown," | Country: ",input$plots_filter_country," | Device: ",input$plots_filter_device
                        #," | Variant: ",variant_id,sep = ""
    )
    
    plot <- ggplot(chart_data, aes(days_run)) + 
      theme_bw() +
      geom_line(aes(y = metric, group = variant_id, colour = factor(variant_id))) + 
      #scale_colour_manual(name = 'PC1 > 0', values = setNames(c('red','green'),c(T, F))) +
      #geom_segment(size = 1)
      ylab("") +
      xlab("Days run") +
      #  geom_hline(yintercept = 0.90,color = "seagreen",size=1,linetype="dashed") + 
      #  geom_hline(yintercept = -0.90,color = "firebrick",size=1,linetype="dashed") + 
      labs(title = title_text) +
      scale_y_continuous(labels = percent_format(),breaks = scales::pretty_breaks(n = 10))+ 
      geom_hline(yintercept = 0,color = "firebrick",size=1,linetype="dashed") + 
      theme(plot.title = element_text(size =12),
            legend.title=element_text(size=12), 
            legend.text=element_text(size=12)) +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12)) +
      theme(plot.title = element_text(size=14)) +
      theme(axis.title.x = element_text(size=12),
            axis.text.x  = element_text(size=12)) +
      theme(legend.position="bottom") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10),limits = c(0,28)) +
      labs(colour = "Contender variant") 
    #+
      #geom_line(aes(y = metric_cr))+
     #scale_color_manual(values=c("red","blue", "green"))
    plot
    
  })
  
  #########
  

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
    
    data_raw <- get_object("s3://bi-photobox-analytics-eu/remy/pbx - ab testing portal/results_data.csv", bucket = "bi-photobox-analytics-eu", check_region = FALSE)
    data <- fread(rawToChar(data_raw))  
      
    export <- data
    export$control_cr <- percent(export$control_cr)
    export$v_cr <- percent(export$v_cr)
    export$delta_cr <- percent(export$delta_cr)
    export$delta_mpu <- percent(export$delta_mpu)
    export$delta_spu <- percent(export$delta_spu)
    export$v_conf_cr <- percent(export$v_conf_cr)
    export$v_conf_mpu <- percent(export$v_conf_mpu)
    export$v_conf_spu <- percent(export$v_conf_spu)
    #export$control_mpu <- paste0("£",round(export$control_mpu, 2))
    #export$v_mpu <- paste0("£",round(export$v_mpu, 2))
    #export$control_spu <- paste0("£",round(export$control_spu, 2))
    #export$v_spu <- paste0("£",round(export$v_spu, 2))
    
    export$cr_conf_flag <- NULL
    export$mpu_conf_flag   <- NULL 
    export$spu_conf_flag <- NULL
    export$completion_flag <- NULL
    
      paste("experimentation_results_export.csv"
            , sep = "")
    },
    content = function(file) {
      utils::write.csv(export, file, row.names = FALSE)
    }
  )
  
  #########################################
  
  output$conf_box<-renderUI({
    
    data_raw <- get_object("s3://bi-photobox-analytics-eu/remy/pbx - ab testing portal/results_data.csv", bucket = "bi-photobox-analytics-eu", check_region = FALSE)
    data <- fread(rawToChar(data_raw))  
    
    
    data <- data[   data$experiment_name == input$running_experiment_dropdown 
                    & data$device == input$plots_filter_device
                    & data$country == input$plots_filter_country,
                    ]
    
   # data <- data[   data$experiment_name == 'shop-pdp-dropdown-config-jan18'
  #                  & data$device == 'All'
  #                  & data$country == 'All',
  #                  ]
    
    
    if(input$metric=='a') {data$metric <- data$v_conf_mpu
                           data$delta <- data$delta_mpu}
    if(input$metric=='b') {data$metric <- data$v_conf_cr
                           data$delta <- data$delta_cr}
    if(input$metric=='c') {data$metric <- data$v_conf_spu
                           data$delta <- data$delta_spu}
    
    delta <-  data$delta
    conf <-   data$metric
    
    metric <- round(data$metric,2)*100
    metric <- paste(metric, "%", sep="")
    
    if (conf >= 0.9 && delta > 0){ contents <-   
      paste("<p","style=",'"font-size: 24px;color:DimGrey; font-family: Helvetica ;font-weight: 1200 ;"' , ">","<strong>","Confidence: ","</strong>",
            "<span","style=",'"font-size: 24px;color:seagreen; font-family: Helvetica ;font-weight: 1200 ;"' , ">","<strong>",metric,"</strong>","</span>","</p>")
    }
    
    if (conf >= 0.9 && delta < 0){ contents <-   
      paste("<p","style=",'"font-size: 24px;color:DimGrey; font-family: Helvetica ;font-weight: 1200 ;"' , ">","<strong>","Confidence: ","</strong>",
            "<span","style=",'"font-size: 24px;color:tomato; font-family: Helvetica ;font-weight: 1200 ;"' , ">","<strong>",metric,"</strong>","</span>","</p>")
    }
    
    if (conf < 0.9){ contents <-   
      paste("<p","style=",'"font-size: 24px;color:DimGrey; font-family: Helvetica ;font-weight: 1200 ;"' , ">","<strong>","Confidence: ","</strong>",
            "<span","style=",'"font-size: 24px;color:DimGrey; font-family: Helvetica ;font-weight: 1200 ;"' , ">","<strong>",metric,"</strong>","</span>","</p>")
    }
    
    HTML(contents)
  })
  
  #########################################
  
  output$lift_box<-renderUI({
    
    data_raw <- get_object("s3://bi-photobox-analytics-eu/remy/pbx - ab testing portal/results_data.csv", bucket = "bi-photobox-analytics-eu", check_region = FALSE)
    data <- fread(rawToChar(data_raw))  
    
    
    data <- data[   data$experiment_name == input$running_experiment_dropdown 
                    & data$device == input$plots_filter_device
                    & data$country == input$plots_filter_country,
                    ]
    
    # data <- data[   data$experiment_name == 'shop-pdp-dropdown-config-jan18'
    #                  & data$device == 'All'
    #                  & data$country == 'All',
    #                  ]
    
    
    if(input$metric=='a') {data$metric <- data$delta_mpu
                           data$conf <- data$v_conf_mpu}
    if(input$metric=='b') {data$metric <- data$delta_cr
                           data$conf <- data$v_conf_cr}
    if(input$metric=='c') {data$metric <- data$delta_spu
                           data$conf <- data$v_conf_spu}
    
    delta <-  data$metric
    conf <-   data$conf
      
    metric <- round(data$metric,2)*100
    metric <- paste(metric, "%", sep="")
    
    
    if (conf >= 0.9 && delta > 0){ contents <-   
      paste("<p","style=",'"font-size: 24px;color:DimGrey; font-family: Helvetica ;font-weight: 1200 ;"' , ">","<strong>","Lift: ","</strong>",
            "<span","style=",'"font-size: 24px;color:seagreen; font-family: Helvetica ;font-weight: 1200 ;"' , ">","<strong>",metric,"</strong>","</span>","</p>")
    }
    
    if (conf >= 0.9 && delta < 0){ contents <-   
      paste("<p","style=",'"font-size: 24px;color:DimGrey; font-family: Helvetica ;font-weight: 1200 ;"' , ">","<strong>","Lift: ","</strong>",
            "<span","style=",'"font-size: 24px;color:tomato; font-family: Helvetica ;font-weight: 1200 ;"' , ">","<strong>",metric,"</strong>","</span>","</p>")
    }
    
    if (conf < 0.9){ contents <-   
      paste("<p","style=",'"font-size: 24px;color:DimGrey; font-family: Helvetica ;font-weight: 1200 ;"' , ">","<strong>","Lift: ","</strong>",
            "<span","style=",'"font-size: 24px;color:DimGrey; font-family: Helvetica ;font-weight: 1200 ;"' , ">","<strong>",metric,"</strong>","</span>","</p>")
    }
    
    HTML(contents)
    
  })
  
  #########################################
  
  output$status_box<-renderUI({
    
    data_raw <- get_object("s3://bi-photobox-analytics-eu/remy/pbx - ab testing portal/results_data.csv", bucket = "bi-photobox-analytics-eu", check_region = FALSE)
    data <- fread(rawToChar(data_raw))  
    
    
    data <- data[   data$experiment_name == input$running_experiment_dropdown 
                    & data$device == input$plots_filter_device
                    & data$country == input$plots_filter_country,
                    ]
    
     #data <- data[   data$experiment_name == 'shop-pdp-dropdown-config-jan18'
    #                  & data$device == 'All'
    #                  & data$country == 'All',
     #                 ]
    
    
    status <- unique(data$completion_text)
    
    if (status == "Completed"){ contents <-   
      paste("<p","style=",'"font-size: 24px;color:DimGrey; font-family: Helvetica ;font-weight: 1200 ;"' , ">","<strong>","Status: ","</strong>","</p>",
            "<p","style=",'"font-size: 24px;color:darkseagreen; font-family: Helvetica ;font-weight: 1200 ;"' , ">","<strong>",status,"</strong>","</p>")
    }
    
    if (status == "Running"){ contents <-   
        paste("<p","style=",'"font-size: 24px;color:DimGrey; font-family: Helvetica ;font-weight: 1200 ;"' , ">","<strong>","Status: ","</strong>","</p>",
              "<p","style=",'"font-size: 24px;color:gold; font-family: Helvetica ;font-weight: 1200 ;"' , ">","<strong>",status,"</strong>","</p>")
    }
    
    HTML(contents)
  
        
  })
  
  
  
  #########
  ######################################################################################################################
  ######################################################################################################################
})

shinyApp(ui, server)
######################################################################################################################
######################################################################################################################

