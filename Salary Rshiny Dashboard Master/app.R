
library(shiny)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(ggthemes)
library(maps)
library(leaflet)
library(shinythemes)
library(tidyverse)
library(rsconnect)
library(scales)

#df <- read.csv("data/Levels_Fyi_Salary_Data.csv")
source("Exploration.R")

companies <- sort(final_df$company, decreasing=FALSE)
titles <- c("All", sort(unique(final_df$title), decreasing=FALSE))
#df1$min_exp = as.integer(df1$min_exp)
#df1$max_exp = as.integer(df1$max_exp)

ui <- fluidPage(
    theme = shinytheme("sandstone"),
    titlePanel("Data Science Industry Salaries"), # App title
    tabsetPanel(
      id = "tab_being_displayed", # input ID for tap being displayed
      
      tabPanel("Overview", # Create Overview tab
                 sidebarLayout( # Layout for Overview tab
                   sidebarPanel(
                     h4("Data Science Industry Salaries Overview"),
                     p("As students in the School of Data Science at UNC Charlotte, we are always interested in what kind of data science opportunities are available, both in Charlotte and around the world."),
                     p("Explore the tabs in this app to compare the top companies employing data scientists and related roles."),
                     hr(),
                     radioButtons("overview_display", "What would you like to display?",
                                  choices = c("Number of Jobs", "Average Yearly Compensation")),
                     br(),
                     img(src = "logo.png", width="40%", height="40%", alt = "levels.fyi Logo")
                   ),
                   mainPanel(
                     plotOutput("overview", height="600px",width="600px")
                   )
                 )
        ),
        tabPanel("Charlotte Map", # Create Map tab
                 sidebarLayout( # Layout for Map tab
                   sidebarPanel(
                     h4("Map of Top Charlotte Companies Employing Data Scientists and Related Roles"),
                     hr(),
                     p("Select which companies to display from the dropdown menu below. You can also delete selections using the backspace key."),
                     selectInput(inputId="map_company", label="Select companies:", choices=companies, 
                                 selected=companies, multiple=TRUE),
                     p("Select which job titles to include from the dropdown menu below. Only companies employing this role will be displayed."),
                     selectInput(inputId="map_title", label="Select titles:", choices=titles, selected="All"),
                     p("The table to the right displays an overview of the selected companies and titles."),
                     br(),
                     img(src = "logo.png", width="40%", height="40%", alt = "levels.fyi Logo")
                   ),
                   mainPanel(
                     br(),
                     textOutput("shading"),
                     br(),
                     leafletOutput("map", height = "500px", width = "800px"),
                     br(),
                     tableOutput("view")
                   )
                 )
        ), 
        tabPanel("Job Salary", # Create company-level tab
                 sidebarLayout( # Layout for Company-level tab
                     sidebarPanel(
                         h4("Industry and Company Salaries"),
                         p("These graphics show an analysis of the distribution of salaries for a given job title."),
                        br(),
                        p("The thin black line shows the average salary industry wide for the given job role.
                           The blue line indicates what the average compensation is at the specified company compared to the distribution and industry."),
                         hr(),
                        
                        #Input for Job title
                         selectInput(inputId = "title", 
                                     label = "Select Job Title", 
                                     choices = df3$title, 
                                     selected = "Data Scientist", 
                                     multiple = FALSE),
                         
                         selectInput(inputId = "companies", 
                                     label = "Select Company", 
                                     choices = companydata$company, 
                                     selected = "Amazon", 
                                     multiple = FALSE, 
                                     selectize=TRUE),

                        #radioButtons(inputId = "salary_type", 
                        #             label = "What type of salary to view?", 
                        #             choices = c("Base Salary", "Combined Salary (including stock options and bonuses)"), 
                        #             selected = "Base Salary"),  
                        
                        checkboxInput(inputId = "hist", 
                                       label = "Show job salary over time", 
                                       value = FALSE),
                        br(),
                        img(src = "logo.png", width="40%", height="40%", alt = "levels.fyi Logo")
                        ),
                     mainPanel(
                         plotOutput(outputId = "histogram", height = "450px", width = "800px"),
                         
                         plotOutput(outputId = "jobsalary", height = "450px", width = "800px")
                         
                        )
                     
                    )
        ),
         tabPanel("Salary Across Positions",
                  sidebarLayout( # Layout for Company-level tab
                      sidebarPanel(
                        h4("Companies and the Salary Given for Each Role"),
                        p("In this visual we can examine how the different jobs stack up to one another in a company.
                          You can view how each company values the position based on salary given.
                          You can choose between viewing the median or mean salary."),
                        radioButtons(inputId = "avg_or_med",
                                     label = "View Median or Mean Salary",
                                     choices = c("Median", "Mean"),
                                     selected = "Median"),
                        selectInput(inputId = "company_jobs", 
                                      label = "Select Companies", 
                                      choices = companydata$company, 
                                      selected = "Amazon", 
                                      multiple = FALSE, 
                                      selectize=TRUE),
                          selectInput(inputId = "select_title", 
                                      label = "Select Job Title", 
                                      choices = df3$title, 
                                      selected = df3$title, 
                                      multiple = TRUE,
                                      selectize = TRUE)
                      ),
                      
                     mainPanel(
                         plotOutput(outputId = "salary")
                     )
                  )
                  
         ),
        tabPanel("About",
                br(),
                column(1),
                column(8, 
                h5('This app was developed by Rachael Dewey and Forrest Johnson.'),
                p("This is the term project for Visual Analytics Class (DSBA 5122) at UNC Charlotte within the Data Science and Business Analytics Program. The class was taught by Chase Romano."),
                br(),
                HTML('<a href="https://github.com/Forrestjohnson2000" style="color: #e36209">View Code on GitHub</a>')
                ),
                p("Data sourced from Kaggle.com:"),
                HTML('<a href="https://www.kaggle.com/jackogozaly/data-science-and-stem-salaries" style="color: #e36209">levels.fyi Data Science and STEM Salaries</a>'),
                column(3)
            )
    )
)

server <- function(input, output) {
  # ------------------------------ OVERVIEW Bar Chart -----------------------------
  
  data_overview <- reactive({
    if(input$overview_display == "Number of Jobs") {
      df_counts
    } else if(input$overview_display == "Average Yearly Compensation") {
      df_salaries
    }
  })
  
  gg_overview <- reactive({
    if(input$overview_display == "Number of Jobs") {
      gg <- df_counts %>% 
        ggplot(aes(x = reorder(company, count),
                   y = count)) +
        labs(title = "Data Science Industry Overview",
             subtitle = "Top Ten Companies by Number of Job Postings",
             y='Number of Jobs', x ='')
    } else if(input$overview_display == "Average Yearly Compensation") {
      gg <- df_salaries %>% 
        ggplot(aes(x = reorder(company, avg_salary),
                   y = avg_salary)) +
        labs(title = "Data Science Industry Overview",
             subtitle = "Top Ten Companies by Average Yearly Compensation",
             y='Average Yearly Compensation', x ='') +
        scale_y_continuous(labels = scales::dollar)
    }
    
    return(gg)
  })
  
  output$overview <- renderPlot({
    
    p <- gg_overview() +
      geom_bar(stat="identity",
               aes(color = company, fill = company),
               show.legend = FALSE) +
      theme_minimal() +
      theme(title = element_text(size = 17),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15, face = "bold")) +
      coord_flip()
    
    p
  })
  
  # ------------------------------------- MAP -------------------------------------
  
  df_map_company <- reactive({ # create reactive for company input
    final_df %>% filter(company %in% input$map_company)
  })
  
  
  df_groups <- reactive({ # create reactive for title input
    
    if(input$map_title == "All") { # if/else for "All" option
      df_map <- df_map_company()
    } else {
      df_map <- df_map_company() %>%
        filter(title == input$map_title)
    }
    
    df_map <- df_map %>% # create count and avg_salary columns
      group_by(company, avg_income, title, lat, long) %>%
      summarise(count=n(), avg_salary = mean(basesalary)) %>%
      ungroup()
    
    return(df_map)
  })
  
  selected_title <- reactive({ input$map_title }) # create reactive to use for text output
  
  output$shading <- renderText({
    text <- paste0("The shading of each point on the map is currently conveying the average salary of each selected Charlotte company",
                   " that employs the role: ",
                   selected_title(),". At least 2 selected companies must employ this role for shading to appear.")
  })
  
  mypal <- reactive({ # reactive for legend color scale
    dfg = df_groups()
    colorNumeric(palette = c("purple","orange"), domain = dfg$avg_income, reverse=TRUE)
  })
  
  output$map <- renderLeaflet({ # map output
    leaflet() %>%
      addTiles() %>%
      fitBounds(min(final_df$long), min(final_df$lat), 
                max(final_df$long), max(final_df$lat)) 
  })
  
  observe({
    
    req(input$tab_being_displayed == "Charlotte Map") # to start with points displayed
    
    df_map <- df_groups()
    
    labels <- sprintf( # create labels for popups
      "<strong>%s</strong><br/>$%g",
      df_map$company, df_map$avg_salary
    ) %>% lapply(htmltools::HTML)
    labels
    
    if(nrow(df_map) == 0){ # if no company selections, clear markers
      leafletProxy("map", data = df_map) %>%
        clearMarkers()
    }
    
    else{ # map details
      leafletProxy("map", data = df_map) %>%
        clearMarkers() %>%
        clearPopups() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addCircleMarkers(radius = 8, 
                         weight = 1, 
                         color = ~mypal()(df_map$avg_income),
                         stroke = FALSE, 
                         fillOpacity = 0.95,
                         label =labels,
                         labelOptions = labelOptions(noHide = F, offset=c(0,-12)))  %>%
        addLegend("bottomleft",
                  title = "Average Yearly Compensation per Company",
                  pal = mypal(),
                  values = df_map$avg_income,
                  layerId = "legend",
                  opacity = 0.90,
                  labFormat = labelFormat(prefix="$"))
    }
  })
  
  
  output$view <- renderTable({ # display data table
    head(df_groups()[c("company","title","count","avg_salary")],n=50)
  })
  
  ############ REactive Setup Section #####################################
    selected_company <- reactive({ input$companies })
    
    selected_job_title <- reactive({ input$title })
    
    job_titles <- reactive({ input$select_title })
    
    selected_company_jobs <- reactive({ input$company_jobs })
    

    #For choosing company
    df_company <- reactive({
        finaldata %>% filter(company == selected_company())#, title == selected_job_title())
    })
    
    #For choosing Title
    df_title <- reactive({
        sjt = selected_job_title()
        finaldata %>% filter(title == sjt)
    })
    
    #to group by title and company to get average salary across both
     df_title_company = reactive({
       finaldata %>% 
         group_by(title, company)
     })
      
     #To Choose between Mean and Median
       salary_makeup = reactive({
         df_tc = df_title_company() 
         if(input$avg_or_med == "Median"){
           df_tc = df_tc %>% 
             summarize(avg_sal = median(basesalary)) %>% 
             filter(title %in% job_titles(),
                    company %in% selected_company_jobs()) %>%
             summarize(ad = median(avg_sal))
         }
         else{
           df_tc = df_tc %>% 
             summarize(avg_sal = mean(basesalary)) %>% 
             filter(title %in% job_titles(),
                    company %in% selected_company_jobs()) %>%
             summarize(ad = mean(avg_sal))
         }
         return(df_tc)
       })
     
     #salary_type <- reactive({
     #  if(input$salary_type == "Base Salary") {
     #    salary = basesalary
    #   } else{
     #    salary = totalyearlycompensation
     #  }
     #  return(salary)
     #})
     
     
     ############## ~~~~~~~~~~~~~~Average Salary Across Jobs in each Company ~~~~~~~~~~~~~~~~~~~~~~################ 
     
     output$salary = renderPlot({
       sm = salary_makeup()
         
         
         p = ggplot(sm, aes(reorder(title, ad), fill = ad)) + 
          geom_col(aes(y = ad), show.legend = FALSE) +
           #facet_wrap(facets = company)
           #geom_col(aes(y = mean_job_sal$avg_sal), show.legend = FALSE) +
           scale_y_continuous(labels = scales::dollar) +
           coord_flip()
             
          p = p + labs(title = "Average salary across job titles",
                       y = "Average Salary",
                       x = "") +
            theme_bw() +
            theme(title = element_text(size = 17, family = "sans"),
                  #axis.text.x = element_text(size = 15),
                  axis.text.y = element_text(size = 15))
          p
      })
    
     
     ############## Histogram of Annual Salary ####################
     output$histogram = 
         renderPlot({
             
             dft = df_title()
             median_job_sal = finaldata %>% filter(title == selected_job_title(), company == selected_company()) %>% 
                 summarize(c_med = round(median(basesalary),0))
             
             #Remove outliers:
             IQR = IQR(dft$basesalary)
             Q1 = quantile(dft$basesalary, .25)
             Q3 = quantile(dft$basesalary, .75)
             adjusted_df = subset(dft, dft$basesalary > (Q1 - 5*IQR) & 
                                      dft$basesalary < (Q3 + 5*IQR))
             
             p = ggplot(adjusted_df, mapping = aes(x = basesalary)) + 
                 geom_histogram(aes(fill = title), 
                                #bins = 20, 
                                binwidth = 50000, 
                                show.legend = FALSE) +
                 scale_x_continuous(labels = scales::dollar)
                 
                 
            p = p + labs(title = "Distribution of salary offered by different jobs",
                      subtitle = input$title,
                      x='', y ='') +
                 theme_bw() +
                 theme(title = element_text(size = 17, family = "sans"),
                       axis.text.x = element_text(size = 15),
                       axis.text.y = element_text(size = 15)) +
                 geom_vline(aes(xintercept = median_job_sal$c_med), 
                            color = "blue", 
                            size = 1) +
                geom_vline(aes(xintercept = median(dft$basesalary)),
                           color = "black",
                           size = 0.3)
            ##Create adjustable y value:
            scale = ggplot_build(p)
            scale_y = scale[["layout"]][["panel_scales_y"]][[1]][["range"]][["range"]]
            
            
             p + annotate("text", size = 5, 
                          x = median_job_sal$c_med + 5000, 
                          y = scale_y[2],
                          hjust = 0, vjust = 1,
                          label = "Median salary \noffered at company") +
                 annotate("text", size = 5, 
                          x = median_job_sal$c_med + 5000, 
                          y = scale_y[2],
                          hjust = 0, vjust = 5,
                          label = scales::dollar(median_job_sal$c_med))
             
             #print(max(p[["data"]][[1]][["count"]]))
         })
     
     ########### Annual Salary Over Time ################################
     output$jobsalary = renderPlot({
         if(input$hist){
             dft1 = df_title()
             mean_job_sal = finaldata %>% filter(title == selected_job_title(), company == selected_company()) %>% 
               summarize(c_avg = round(mean(basesalary),0))
             
             p1 = ggplot(dft1, mapping = aes(timestamp, basesalary, color = title)) + 
                geom_smooth(alpha = .1, show.legend = FALSE) +
               scale_y_continuous(labels = scales::dollar) +
                labs(title = "Annual salary over time",
                    subtitle = input$title, x = "Year", y = "Average Annual Salary") +
                theme_bw() +
                theme(title = element_text(size = 17, family = "sans"),
                   axis.text.x = element_text(size = 15),
                   axis.text.y = element_text(size = 15))
             p1 #+ geom_smooth(aes(mean_job_sal$c_avg), show.legend = FALSE)
         }
         
     }) 
     

            
   
    
    #salary_makeup <- df %>%
    #    group_by(title) %>%
    #    summarise(company_sal_pct = 100 * mean(input$companies) / sum(title))
    
    #df_title <- reactive({
    #    df %>% filter(title == input$jobtitle, Year == selected_year())
    #})
    
    #output$barchart <- renderPlot({
    #    df <- df_company()
    #    
    #    p <- ggplot(data = df,
    #                aes(x = factor(Group, levels = rev(levels(Group))),
    #                    y = Percentage / 100)) +
    #        geom_bar(stat="identity",
    #                 aes(color = Group, fill = Group),
    #                 show.legend = FALSE) +
    #        labs(title = input$school,
    #             subtitle = "Percent of School's Student Population",
    #             x='', y ='') +
    #        scale_y_continuous(limits = c(0, 1),
    #                           breaks = seq(0,1,by = .10),
    #                          labels = scales::percent_format(accuracy = 1)) +
    #        theme_bw() +
    #        theme(title = element_text(size = 17),
    #              axis.text.x = element_text(size = 15),
    #              axis.text.y = element_text(size = 15, face = "bold")) +
    #       coord_flip() 
    #})
    
  

    #output$companies = renderPlot({
    #    
    #    ggplot(df1, aes(x = Input$companies, y = avg_exp)) + geom_col(position = "dodge2") +
    #        labs(x = "Company",y = "Average Years Experience Required") +
    #        theme(legend.position = "top") 
    #})
}

shinyApp(ui = ui, server = server)
