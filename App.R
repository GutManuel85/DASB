# https://shiny.rstudio.com/gallery/
# https://www.youtube.com/watch?v=9uFQECk30kA
# https://www.youtube.com/watch?v=u8KCw3VtFW8
# https://stackoverflow.com/questions/34663099/how-to-center-an-image-in-a-shiny-app
# https://www.r-graph-gallery.com/215-the-heatmap-function.html

library(readr)
library(datasets)
library(dplyr)
library(shiny)
library(MASS)
library(ISLR)
library(tidyverse)
library(officer)
library(shinydashboard)
library(shinyWidgets)
library(gmodels)
rm(list = ls())
dataset <- read_csv("dataset_heartFailure.csv")
dataset_without_NA <- na.omit(dataset)
dataset_correlations <- cor(dataset_without_NA)
parameter_description <- read_csv("parameter_description.csv")
parameter_description <-
  subset(parameter_description, select = c(parameter, description))
heart.failure <- dataset_without_NA
cols <-
  c(
    "death",
    "gender",
    "cancer",
    "cabg",
    "crt",
    "defib",
    "dementia",
    "diabetes",
    "hypertension",
    "ihd",
    "mental_health",
    "arrhythmias",
    "copd",
    "obesity",
    "pvd",
    "renal_disease",
    "valvular_disease",
    "metastatic_cancer",
    "pacemaker",
    "pneumonia",
    "pci",
    "stroke",
    "senile",
    "quintile",
    "ethnicgroup",
    "fu_time"
  )
heart.failure[cols] <- lapply(heart.failure[cols], factor)
log.model <- glm(death ~ los + age + gender + cancer + cabg + crt + defib + dementia + diabetes + hypertension + ihd + mental_health +  arrhythmias + copd + obesity + pvd + renal_disease + metastatic_cancer + pacemaker + pneumonia + prior_appts_attended + prior_dnas + pci + stroke + senile + quintile + fu_time, data = dataset, family = binomial())
for_death <- coef(log.model)[coef(log.model) > 0]
h2.model <- glm(death ~ los + age + cancer + dementia + diabetes + ihd + copd + obesity + renal_disease + metastatic_cancer + pacemaker +prior_dnas + pci + stroke + senile + fu_time, data = dataset, family = binomial())
h2.model.2 <- glm(death ~ los * age, data = heart.failure, family = binomial())
min_age <- min(dataset_without_NA$age)
max_age <- max(dataset_without_NA$age)

ui <-
  fluidPage(
    #Header or Title Panel
    navbarPage(
      title = "Heart Failure:",
      id = "navbarID",
      setBackgroundImage(src = "sky.png"),
      tabPanel(
        "Background",
        HTML('<center><img src="reasons.png" width="600"></center>'),
        br(),
        br(),
        HTML('<center><img src="questions.png" width="600"></center>'),
        br(),
        br(),
        HTML('<center><img src="hypothesis.png" width="600"></center>'),
      ),
      tabPanel(
        "Data",
        tabsetPanel(
          type = "tab",
          tabPanel("Description", wellPanel(tableOutput("description"))),
          tabPanel("Summary", verbatimTextOutput("summary")),
          tabPanel("Structure", verbatimTextOutput("structure")),
          tabPanel(" Show data",
                   tableOutput("data"), )
        )
      ),
      tabPanel("Big Picture",
               mainPanel(
                 br(),
                 plotOutput("heatmap"),
                 br(),
                 plotOutput("death_correlations")
               )),
      
      tabPanel(
        "Significance",
        sidebarPanel(
          selectInput(
            inputId = "single_parameter",
            label = "Parameter:",
            choices = colnames(dataset),
            selected = "age"
          ),
          verbatimTextOutput("single_description")
        ),
        mainPanel(verbatimTextOutput("single_parameter"))
      ),
      tabPanel(
        "Results",
        HTML('<left><img src="hypothesis.png" width="575"></left>'),
        br(),
        br(),
        tabsetPanel(
          type = "tab",
          tabPanel(
            "Hypothesis 1",
            wellPanel(
              h3(
                "Hypothesis: There is no significant difference in survival
                                for men and women."
              ),
              br(),
              h4("1. To test the hypothesis, let's make a first plot"),
              br(),
              plotOutput("gender_plot_1"),
              br(),
              p(
                "The bar plot above shows that the amount of men and women are about equally distributed in cases of survival and death."
              ),
              br(),
              h4(
                "2. To explore the hypothesis even further, we'll use the CrossTable-function"
              ),
              p(
                "The Chi^2 statistic is part of the CrossTable-function and tests whether the two variables are independent. If the test produces a significant result (p less than 0.05 or better 0.01) that is larger than the critical values of the Chi^2 statistic at a certain degree of freedom (with df=1: Chi^2 larger than 3.85 at p=0.05 or larger than 6.63 at p=0.01) we can conclude that there is a significant relationship between the variables."
              ),
              verbatimTextOutput("gender_CrossTable"),
              p(
                "Here the Chi^2 statistic is smaller than the critical values and the p value is far over 0.05 so we can conclude that the variable are independent. I.e. there is no relationship between gender and survival/death which is supported by the bar plot above."
              ),
              br(),
              h4("3. Lets make a last proof with a linear model"),
              verbatimTextOutput("gender_linearModel"),
              p(
                "The summary of the linear modell shows that the p-value of 0.837 is much larger than 0.001. Thus, the linear model also shows that gender is not significant in relation to death."
              ),
              br(),
              h4("4. Final assessment of the hypothesis"),
              p(
                "The hypothesis is confirmed. Thus, gender does not matter whether one survive a heart attack or not"
              )
            )
          ),
          tabPanel(
            "Hypothesis 2",
            wellPanel(
            h3(
              "Hypothesis 2: There are comorbidities which feature significantly in fatalities."
            ),
            br(),
            h4("1. To test the hypothesis, let's make a first summary of the linear modell with all parameters"),
            verbatimTextOutput("h2_summary_lm_all"),
            br(),
            h4("2. Now, let's have a look, which parameters favor a fatal outcome"),
            verbatimTextOutput("h2_parameters_favor_fatal"),
            br(),
            h4("3. Let's check, which of these parameters are significant"),
            verbatimTextOutput("h2_signifigant_parameter"),
            p("According to the modell, 'los' and 'age' are quite signficant, 'metastatic_cancer' and 'prior_dnas' are somewhat significant and 'dementia' is barely significant."),
            br(),
            p("If we only choose the two most sicnificant parameter, we can see, that only age is significant"),
            verbatimTextOutput("h2_only_two_parameter"),
            br(),
            h4("4. Final assessment of the hypothesis"),
            p(
              "The hypothesis is not confirmed. The parameter age is significant, but age cannot be viewed as a real comorbidity. All other commorbities seems to be not significant, whether someone is dying after a heart failure or not."
            )
            
          )),
          tabPanel(
            "Hypothesis 3",
            wellPanel(
            h3(
              "Hypothesis 3: There are other comorbidities which feature significantly in survival."
            ),
            br(),
            h4("1. To test the hypothesis, let's make a first summary of the linear modell with all parameters"),
            verbatimTextOutput("h3_summary_lm_all")
          )),
          tabPanel("Hypothesis 4",
                   wellPanel(
                     
                   )),
          tabPanel("Hypothesis 5", 
                   wellPanel(
                   )),
        )
      ),
      tabPanel("Age",
               h3("Percentage death rate within an age group"),
               br(),
               sidebarPanel(
                 sliderInput(inputId = "min_age", label = "Minimum age:", value = min_age, min = min_age, 
                             max = max_age, sep = ""),
                 sliderInput(inputId = "max_age", label = "Maximum age:", value = max_age, min = min_age, 
                             max = max_age, sep = ""),
                 
               ),
               mainPanel(
               wellPanel(
                 plotOutput("age_piechart"),
      )),
      br(),
      h3("Effect of patient age on dying"),
      wellPanel(
        plotOutput("curve_age_dying")
        
      ))
    )
  )

server <- function(input, output, session) {
  output$heatmap <- renderPlot({
    palette = colorRampPalette(c("green", "white", "red"))(20)
    title <- "Heatmap with correlations"
    heatmap(
      x = subset(dataset_correlations, select = -id),
      col = palette,
      symm = TRUE,
      main =  title,
      scale = "column",
      Colv = NA,
      Rowv = NA,
      mar = c(7, 2)
    )
    legend(
      x = "bottomright",
      legend = c("low", "middle", "high"),
      fill = c("green", "white", "red")
    )
  })
  
  output$death_correlations <- renderPlot({
    correlation <- dataset_correlations[, 'death']
    col <- colnames(dataset)
    death_correlations <- tibble(col, correlation)
    
    ggplot(data = death_correlations) + geom_point(aes(x = col, y = correlation)) +
      geom_hline(yintercept = 0,
                 color = "blue",
                 size = 2) +
      theme(axis.text.x = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      )) +
      ggtitle("Correlations with death")
  })
  
  output$description <- renderTable(parameter_description)
  
  output$structure <- renderPrint(str(dataset))
  
  output$data <- renderTable(dataset)
  
  output$single_parameter <-
    renderPrint(summary(lm(death ~ unlist(
      dplyr::select(dataset, input$single_parameter)
    ), data = dataset)))
  
  output$single_description <-
    renderPrint(toString(parameter_description$description[parameter_description$parameter ==
                                                             input$single_parameter]))
  
  output$summary <- renderPrint(
    summary(dataset),
    quoted = FALSE,
    width = getOption("width"),
    outputArgs = list()
  )
  
  output$gender_plot_1 <- renderPlot({
    ggplot(heart.failure, aes(x = death)) +
      geom_bar(aes(fill = gender)) +
      labs(title = "Proportion of gender in patient status", x = "Patient Status", y = "# of Cases") +
      scale_x_discrete(labels = c("Alive", "Dead")) +
      scale_fill_manual(name = "Gender", labels = c("Male", "Female"), values = c("cornflowerblue", "hotpink"))
  })
  
  output$gender_CrossTable <- renderPrint(
    CrossTable(
      heart.failure$gender,
      heart.failure$death,
      fisher = T,
      chisq = T,
      expected = T,
      sresid = T,
      format = "SPSS"
    )
  )
  
  output$gender_linearModel <-
    renderPrint(summary(lm(death ~ gender, data = dataset)))
  
  output$h2_summary_lm_all <-
    renderPrint(
      summary(log.model)
    )
  
  output$h2_parameters_favor_fatal <-
    renderPrint(
      names(for_death)
    )
  
  output$h2_signifigant_parameter <-
    renderPrint(
    summary(h2.model)
    )
  
  output$h2_only_two_parameter <-
    renderPrint(
      summary(h2.model.2)
    )
  
  output$curve_age_dying <-
    renderPlot({
      ggplot(heart.failure, aes(x = age, y = as.numeric(death) - 1)) +
        geom_point(alpha = 0.25) +
        stat_smooth(method = "glm", aes(color = "red"), se = T, fullrange = T, method.args = list(family = binomial)) +
        theme(legend.position = "none") +
        labs(title = "Effect of Patient Age on dying", x = "Patient Age (in years)", y = "Chance of Death") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + xlim(0, 150)
    })
  
  output$age_piechart <-
    renderPlot({
      ggplot(data = data.frame(group = c("alive","death"), value = c(length(dataset$id[dataset$age >= input$min_age & dataset$age <= input$max_age & dataset$death == 0]), 
               length(dataset$id[dataset$age >= input$min_age & dataset$age <= input$max_age & dataset$death == 1]))),
             aes(x="", y=value, fill=group)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) +
        labs(title = "", x = "", y = "")
    })
  
  observe(updateSliderInput(session, "max_age", min = input$min_age))
  
}


shinyApp(ui, server)
