library(shiny)
library(ggplot2)
library(dplyr)


insurance <- read.csv(file.choose(),header = TRUE)


insurance$dob <- as.Date(insurance$dob, "%m/%d/%Y")
insurance$incident_date <- as.Date(insurance$incident_date, "%m/%d/%Y")
insurance$age <- insurance$incident_date - insurance$dob
insurance$age <- round(as.numeric(insurance$age) / 365.25, 0)
# Please note that you can create several other demographic variables using other dates

# ==========
# Isolate the date for convenience
insurance_dem <- insurance[,c(12, 14, 16, 18, 17, 19, 22, 3, 5, 6,11,21,4,10)]

# ==========
# Recode the data
insurance_dem[, c(1, 2, 3, 4, 8,10,11,12,13,14)] <- lapply(insurance_dem[, c(1, 2, 3, 4, 8, 10,11,12,13,14)], factor)

# ==========
# Label categorical variables
levels(insurance_dem$gender) <- c("Male", "Female")
levels(insurance_dem$ed_cat) <- c("No HS", "HS", "Some college", "College degree",
                                  "Post-undergraduate")
levels(insurance_dem$retire) <- c("Active", "Retired")
levels(insurance_dem$marital) <- c("Single", "Married")
levels(insurance_dem$claim_type) <- c("Wind/Hail", "Water Damage", 
                                      "Fire/Smoke", "Contamination", "Theft/Vandalism")
levels(insurance_dem$fraudulent) <- c("Legitimate", "Fraudulent")
levels(insurance_dem$townsize) <-c(">250000", "50000-249999", "10000-49999","2500-9999","<2500")
levels(insurance_dem$primary_res) <- c("No", "Yes")
levels(insurance_dem$uninhabit) <- c("No", "Yes")
levels(insurance_dem$deductible) <- c("100", "200","500", "1000", "2000", "3000")
# ==========
# Define UI of Shiny App
ui <- fluidPage(
  titlePanel("Demographic Segmentation based on the Insurance Companies Customer Base"),
  sidebarLayout(
    sidebarPanel(
      selectInput("continuous_var", "Key Performance Variable:", choices = colnames(insurance_dem[,c(5, 6, 7, 9,10,11,14)]), selected = colnames(insurance_dem[9])),
      selectInput("summary_stat", "Summary Statistic:", choices = c("Total", "Count")),
      selectInput("categorical_var", "Demographic Variable:", choices = colnames(insurance_dem[,c(1, 2, 3, 4, 8,10,11,12,13,14)]), selected= colnames(insurance_dem[8])),
      selectInput("categorical_varr", "Demographic Variable 2:", choices = colnames(insurance_dem[,c(1,2,3,4,8,10,11,12,14)]),selected = colnames(insurance_dem[2])),
      checkboxInput("show_colors", "Show Color Palette", value= TRUE),
      checkboxInput("show_labels", "Show Labels", value = TRUE),
      sliderInput("bins1", "Bin width:", min = 1, max = 100, value = 30),
      sliderInput("font_size", "Font Size:", min = 2, max = 20, value = 3.5, step= 0.5),
      actionButton("update", "Update Chart")
    ),
    
    mainPanel(
      plotOutput("chart"),
      plotOutput("histogram2"),
      plotOutput("jitterplot"),
      plotOutput("scatter"),
      plotOutput("barplot")
    )
  )
)


# Define server
server <- function(input, output) {
  palette <- brewer.pal(6, "Set2") 
  # Create pie chart
  output$chart <- renderPlot({
    continuous_var <- input$continuous_var
    summary_stat <- input$summary_stat
    categorical_var <- input$categorical_var
    show_labels <- input$show_labels
    show_colors <- input$show_colors
    
    summary_data <- insurance_dem %>%
      group_by(!!sym(categorical_var)) %>%
      summarize(value = switch(summary_stat,
                               "Total" = sum(!!sym(continuous_var)),
                               "Count" = n()))
    
    p <- ggplot(summary_data, aes(x = "", y = value, fill = !!sym(categorical_var))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme(legend.text = element_text(size = input$font_size + 4),
            axis.text = element_text(size = input$font_size + 4),plot.title = element_text(size = input$font_size + 6, face = "bold"),
            plot.subtitle = element_text (size= input$font_size + 4),
            plot.caption= element_text (size= input$font_size +4)
      ) + 
      labs(title = paste("Pie Chart of", continuous_var, "per", categorical_var), size = input$font_size)
    
    if (show_labels) {p
      p <- p + geom_text(aes(label = paste0(round(value, 2), ifelse(summary_stat == "Average" || summary_stat == "Count", "", ""))), 
                         position = position_stack(vjust = 0.5), color = "black", size = input$font_size, show.legend = FALSE)+
        labs(subtitle = "Insurance Data",
             caption= "BAN 5100 Data Visualizations")
    }
    if (show_colors) {p
      p <- p + scale_color_manual(values = palette)
    }
    
    p
    
  })
  
  
  # Create bar chart
  output$histogram2 <- renderPlot({
    categorical_var <- input$categorical_var
    show_labels <- input$show_labels
    show_colors <- input$show_colors
    
    s<- ggplot(insurance_dem, aes(x = !!sym(categorical_var), fill = !!sym(categorical_var))) +
      geom_bar(width = 0.5) +
      stat_count(geom = "text", aes(label = paste0(round((..count..) / sum(..count..) * 100, 2), "%")),
                 position = position_stack(vjust = 0.5), color = "black", size = input$font_size, show.legend = FALSE) +
      theme(legend.text = element_text(size = input$font_size + 4),
            axis.text = element_text(size = input$font_size + 4),plot.title = element_text(size = input$font_size + 6, face = "bold"), plot.subtitle = element_text (size= input$font_size + 4),
            plot.caption= element_text (size= input$font_size +4)
      ) +
      labs(title = paste("Histogram of", categorical_var), size= input$font_size )
    if (show_labels) {s
      s <- s + labs(subtitle = "Insurance Data",
                    caption= "BAN 5100 Data Visualizations")
    }
    if (show_colors) {s
      s <- s + scale_color_manual(values = palette)
    }
    
    s
    
  })
  
  
  
  ##jitter plot
  output$jitterplot <- renderPlot ({
    continuous_var <- input$continuous_var
    summary_stat <- input$summary_stat
    categorical_var <- input$categorical_var
    show_labels <- input$show_labels
    show_colors <- input$show_colors
    summary_data <- insurance_dem %>%
      group_by(!!sym(categorical_var)) %>%
      summarize(value = switch(summary_stat,
                               "Total" = sum(!!sym(continuous_var)),
                               "Count" = n()))
    q <-ggplot(insurance_dem, aes(x= !!sym(continuous_var), y= !!sym(categorical_var), color= !!sym(categorical_var)))+
      geom_jitter(alpha=0.5) +
      labs(
        title = paste("Jitter Plot of", continuous_var, "by", categorical_var), 
        x = continuous_var,
        y = categorical_var,
        color = categorical_var  
      ) +
      coord_flip() +
      theme(
        legend.position = "top",
        legend.text = element_text(size = input$font_size + 4),
        axis.text = element_text(size = input$font_size + 4),
        plot.title = element_text(size = input$font_size + 6, face = "bold"),
        plot.subtitle = element_text (size= input$font_size + 4),
        plot.caption= element_text (size= input$font_size +4)
      )
    if (show_labels) {q
      q <- q + labs(subtitle = "Insurance Data",
                    caption= "BAN 5100 Data Visualizations")
    }
    if (show_colors) {q
      q <- q + scale_color_manual(values = palette)
    }
    
    q
    
  })
  
  
  ## scatter plot
  
  output$scatter <- renderPlot({
    show_labels <- input$show_labels
    show_colors <- input$show_colors
    categorical_var <- input$categorical_var
    categorical_varr<- input$categorical_varr
    continuous_var <- input$continuous_var
    v <- ggplot(data = insurance_dem, 
                mapping = aes(x = reorder(!!sym(categorical_var),!!sym(continuous_var), na.rm = TRUE),
                              y = !!sym(continuous_var), color =!!sym(categorical_varr))) +
      geom_jitter(position = position_jitter(width = 0.15), alpha = 0.6) + 
      labs(
        x = NULL,
        y = continuous_var,
        color = "Gender",  
        title = paste("Scatterplot of", continuous_var, "by", categorical_var)
      ) + 
      coord_flip() +
      theme(
        legend.position = "top",
        legend.text = element_text(size = input$font_size + 4),
        axis.text = element_text(size = input$font_size + 4),
        plot.title = element_text(size = input$font_size + 6, face = "bold"),
        plot.subtitle = element_text (size= input$font_size + 4),
        plot.caption= element_text (size= input$font_size +4)
      )
    if (show_labels) {v
      v <-v + labs(subtitle = "Insurance Data", 
                   caption = "BAN 5100 Data Visualizations")
    } 
    if (show_colors) {v
      v <-v + scale_color_manual(values = palette)
    }
    v
  })
  
  ##bar chart
  output$barplot <- renderPlot({
    continuous_var <- input$continuous_var
    categorical_var <- input$categorical_var
    show_labels <- input$show_labels
    show_colors <- input$show_colors
    
    b<- ggplot(insurance_dem, aes(x = !!sym(continuous_var), y = !!sym(categorical_var), fill = !!sym(categorical_var))) +
      geom_col(position = "dodge2", alpha = 0.8) +
      labs(
        x = continuous_var,
        y = categorical_var, 
        fill = categorical_var,
        title = paste("Income Distribution by", categorical_var)
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.text = element_text(size = input$font_size + 4),
        axis.text = element_text(size = input$font_size + 4),
        plot.title = element_text(size = input$font_size + 6, face = "bold"),
        plot.subtitle = element_text (size= input$font_size + 4),
        plot.caption= element_text (size= input$font_size +4)
      )
    if (show_labels) {b
      b <-b + labs(subtitle = "Insurance Data", 
                   caption = "BAN 5100 Data Visualizations")
    } 
    if (show_colors) {b
      b <-b + scale_color_manual(values = palette)
    }
    b
  })
}



# Run the app
shinyApp(ui, server)



