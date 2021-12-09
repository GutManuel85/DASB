# https://www.kaggle.com/jackleenrasmybareh/heart-failure
# https://shiny.rstudio.com/gallery/
# https://www.youtube.com/watch?v=9uFQECk30kA
# https://www.youtube.com/watch?v=u8KCw3VtFW8
# https://shiny.rstudio.com/reference/shiny/1.6.0/renderTable.html
# https://stackoverflow.com/questions/34663099/how-to-center-an-image-in-a-shiny-app
# https://www.r-graph-gallery.com/215-the-heatmap-function.html
# https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
# https://heima.hafro.is/~einarhj/education/ggplot2/scales.html
# https://community.rstudio.com/t/need-help-with-error-when-running-geom-smooth-using-formula-y-x/92890
# https://bookdown.org/yih_huynh/Guide-to-R-Book/graphing-with-different-datasets.html
# https://bookdown.org/yih_huynh/Guide-to-R-Book/graphing-with-different-datasets.html
# https://stackoverflow.com/questions/20251119/increase-the-size-of-variable-size-points-in-ggplot2-scatter-plot
# https://stackoverflow.com/questions/34638902/point-size-in-ggplot-2-0-0
# https://stackoverflow.com/questions/25293045/count-number-of-rows-in-a-data-frame-in-r-based-on-group

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
library(ggrepel)
library(waffle)

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
log.model <-
  glm(
    death ~ los + age + gender + cancer + cabg + crt + defib + dementia + diabetes + hypertension + ihd + mental_health +  arrhythmias + copd + obesity + pvd + renal_disease + metastatic_cancer + pacemaker + pneumonia + prior_appts_attended + prior_dnas + pci + stroke + senile + quintile + fu_time,
    data = dataset,
    family = binomial()
  )
for_death <- coef(log.model)[coef(log.model) > 0]
h2.model <-
  glm(
    death ~ los + age + cancer + dementia + diabetes + ihd + copd + obesity + renal_disease + metastatic_cancer + pacemaker +
      prior_dnas + pci + stroke + senile + fu_time,
    data = dataset,
    family = binomial()
  )
h2.model.2 <-
  glm(death ~ los * age, data = dataset, family = binomial())
min_age <- min(dataset_without_NA$age)
max_age <- max(dataset_without_NA$age)
for_survival <- coef(log.model)[coef(log.model) < 0]
h3.model <-
  glm(
    death ~ gender + cabg + crt + defib + hypertension + mental_health + arrhythmias + pvd + pneumonia + prior_appts_attended + pci + quintile,
    data = dataset,
    family = binomial()
  )
h3.model.2 <-
  glm(death ~ cabg * mental_health,
      data = dataset,
      family = binomial())
grouped_number <- dataset %>% group_by(prior_dnas, death) %>% summarise(count = n())



ui <-
  fluidPage(
    #Header or Title Panel
    navbarPage(
      title = "Heart Failure:",
      id = "navbarID",
      setBackgroundImage(src = "sky.png"),
      tabPanel(
        "Introduction",
        HTML('<center><img src="reasons.png" width="600"></center>'),
        br(),
        br(),
        HTML('<center><img src="questions.png" width="600"></center>'),
        br(),
        br(),
        HTML('<center><img src="hypothesis.png" width="600"></center>'),
        br(),
        br(),
      ),
      tabPanel(
        "Data",
        tabsetPanel(
          type = "tab",
          tabPanel("Description", wellPanel(tableOutput("description"))),
          tabPanel("Summary", verbatimTextOutput("summary")),
          tabPanel("Structure", verbatimTextOutput("structure")),
          tabPanel(" Show data",
                   tableOutput("data"),)
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
              h4("1. To test the hypothesis, let's create a first plot"),
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
                "The hypothesis is confirmed. Thus, gender does not matter whether one survive a heart failure or not."
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
              h4(
                "1. To test the hypothesis, let's make a first summary of the linear modell with all parameters"
              ),
              verbatimTextOutput("h2_summary_lm_all"),
              br(),
              h4("2. Now, let's have a look, which parameters favor a fatal outcome"),
              verbatimTextOutput("h2_parameters_favor_fatal"),
              br(),
              h4("3. Let's check, which of these parameters are significant"),
              verbatimTextOutput("h2_significant_parameter"),
              p(
                "According to the modell, 'los' and 'age' are quite signficant, 'metastatic_cancer' and 'prior_dnas' are somewhat significant and 'dementia' and 'senile' are barely significant."
              ),
              br(),
              p(
                "If we only choose the two most sicnificant parameter, we can see, that only age is significant."
              ),
              verbatimTextOutput("h2_only_two_parameter"),
              br(),
              h4("4. Final assessment of the hypothesis"),
              p(
                "The hypothesis is not confirmed. The parameter age is significant, but age cannot be viewed as a real comorbidity. All other commorbities seems to be not significant, whether someone is dying after a heart failure or not."
              )
              
            )
          ),
          tabPanel(
            "Hypothesis 3",
            wellPanel(
              h3(
                "Hypothesis 3: There are other comorbidities which feature significantly in survival."
              ),
              br(),
              h4(
                "1. To test the hypothesis, let's make a first summary of the linear modell with all parameters"
              ),
              verbatimTextOutput("h3_summary_lm_all"),
              br(),
              h4("2. Now, let's have a look, which parameters favor the survival"),
              verbatimTextOutput("h3_parameters_favor_survival"),
              br(),
              h4("3. Let's check, which of these parameters are significant"),
              verbatimTextOutput("h3_significant_parameter"),
              p(
                "According to the modell, 'cabg' is somewhat significant and 'mental health' is barely significant."
              ),
              br(),
              p(
                "If we only choose the two significant parameter, we can see, that 'cabg' and 'mental health' are somewhat significant."
              ),
              br(),
              p(
                "Explanation of cabg: cabg shows whether someone already has a bypass or not. With '1' the patient has a bypass, with '0' not."
              ),
              p(
                "Explanation of mental_health: mental_health shows whether someone has mental illnesses. With '1' the patient has mental illnesses, with '0' not."
              ),
              
              verbatimTextOutput("h3_only_two_parameter"),
              p(
                "This analysis shows the same picture. Let's create a crosstable to have a final analysis."
              ),
              br(),
              h4("4. Crosstables"),
              p("Let's start with the crosstable for cabg."),
              verbatimTextOutput("h3_crosstable_cabg"),
              p(
                "The table shows that the chance of survival with 92.9% in the group of patients with a by-pass is significantly higher than in the group with patients without a py-pass (50.2% chance of survival). The Chi-square-test confirms the significance too due to the fact, that the p-value is less than 0.01."
              ),
              br(),
              p("Let's do the same for mental_health."),
              verbatimTextOutput("h3_crosstable_mental_health"),
              p(
                "The table shows that the chance of survival with 60.7% in the group of patients with a mental illness is about 11% higher than in the group with patients without a mental illness (49.3% chance of survival)."
              ),
              p(
                "However, the Chi-square-test shows, that mental_health isn't a significant factor because the p-value is bigger than 0.05."
              ),
              br(),
              h4("5. Final assessment of the hypothesis"),
              p(
                "The hypothesis is confirmed. The parameter cabg (to have a bypass) is significant and increases the chance of survival. "
              )
            )
          ),
          tabPanel(
            "Hypothesis 4",
            wellPanel(
              br(),
              h3(
                "Hypothesis 4: A longer follow-up time has a positive effect on survival"
              ),
              br(),
              h4("1. Data"),
              p(
                "Explination of parameter fu_time: follow up time (time spent monitoring the patients health after treatment)"
              ),
              br(),
              h4("2. Reuse  the analysis of hypothesis 3"),
              p(
                "As we have seen in the analysis of hypothesis 3, there is only the parameter 'cabg', which has a significant positive influence on the chance of survival. Below is the linear model again."
              ),
              verbatimTextOutput("h4_significant_parameter"),
              br(),
              h4("4. Final assessment of the hypothesis"),
              p(
                "On the one hand, the linear model of hypothesis 3 shows, that the hypothesis is not confirmed. On the other hand, we have to ask ourselves what is cause and what is consequence. If a patient dies very quickly, the follow-up time is of course very short. Possibly a short follow-up time is more the consequence than the cause of a fatal outcome."
              )
            )
          ),
          tabPanel(
            "Hypothesis 5",
            wellPanel(
              h3(
                "Hypothesis 5: Missing fewer outpatient appoitments has a positive effect on survival"
              ),
              br(),
              h4("1. Data"),
              p(
                "Explination of parameter prior_dnas: number of outpatient appointments missed in the previous year"
              ),
              br(),
              h4("2. Reuse  the analysis of hypothesis 3"),
              p(
                "As we have seen in the analysis of hypothesis 3, there is only the parameter 'cabg', which has a significant positive influence on the chance of survival. Below is the linear model again."
              ),
              verbatimTextOutput("h5_significant_parameter"),
              br(),
              p(
                "If we have a look at the analysis of hypothesis 2, we can see, that missed appointments have a negative effect on survival."
              ),
              verbatimTextOutput("h5_significant_parameter_2"),
              br(),
              h4("3. Further analysis"),
              p(
                "Now, we take all parameters with a significance tag from the model above and create a new linear model with those parameters"
              ),
              verbatimTextOutput("h5_significant_parameter_3"),
              p(
                "Further, we will look at the linear model, if we use only the most significant parameter 'age' and the parameter 'prior_dnas'."
              ),
              verbatimTextOutput("h5_significant_parameter_4"),
              p("Here, we can see that 'prior_dnas' is still significant."),
              p(
                "Finally, we will plot the correlation between 'death' and 'prior_dnas'."
              ),
              plotOutput("h5_plot"),
              p("As one can see, the graph increases with the number of missed appointments, what further supports our hypothesis."),
              br(),
              h4("4. Final assessment of the hypothesis"),
              p("Our hypothesis seems to be confirmed, so that missed appointments have a negativ effect on survival.")
            )
          ),
          tabPanel(
            "Summary of the results",
              h3("Results of the analysis summarized"),
              HTML('<left><img src="confirmed.png" width="600"></left>'),
            br(),
            br(),
          )
        )
      ),
      tabPanel(
        "Further analysis",
        h3("Percentage death rate within an age group"),
        br(),
        sidebarPanel(
          sliderInput(
            inputId = "min_age",
            label = "Minimum age:",
            value = min_age,
            min = min_age,
            max = max_age,
            sep = ""
          ),
          sliderInput(
            inputId = "max_age",
            label = "Maximum age:",
            value = max_age,
            min = min_age,
            max = max_age,
            sep = ""
          ),
          selectInput(
            inputId = "chart_type",
            label = "Chart type:",
            choices = c("Pie chart", "Waffle chart") ,
            selected = "Pie chart"
          )
        ),
        mainPanel(wellPanel(plotOutput("age_chart"),)),
        br(),
        h3("Effect of patient age on dying"),
        wellPanel(plotOutput("curve_age_dying")),
        br(),
        h3("Effect of patient age on the length of the hospitalisation"),
        sidebarPanel(
          radioButtons(
            inputId = "radio",
            label = "Shown data:",
            choices = list(
              "both" = 1,
              "alive" = 2,
              "death" = 3
            ),
            selected = 1
          )
        ),
        mainPanel(wellPanel(plotOutput("age_los_chart")))
        
      )
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
  
  output$data <- renderTable(dataset, digits = 0)
  
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
      scale_x_discrete(labels = c("alive", "dead")) +
      scale_fill_manual(
        name = "Gender",
        labels = c("Male", "Female"),
        values = c("cornflowerblue", "hotpink")
      )
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
    renderPrint(summary(log.model))
  
  output$h2_parameters_favor_fatal <-
    renderPrint(names(for_death))
  
  output$h2_significant_parameter <-
    renderPrint(summary(h2.model))
  
  output$h2_only_two_parameter <-
    renderPrint(summary(h2.model.2))
  
  output$h3_summary_lm_all <-
    renderPrint(summary(log.model))
  
  output$h3_parameters_favor_survival <-
    renderPrint(names(for_survival))
  
  output$h3_significant_parameter <-
    renderPrint(summary(h3.model))
  
  output$h3_only_two_parameter <-
    renderPrint(summary(h3.model.2))
  
  output$h3_crosstable_cabg <-
    renderPrint(
      CrossTable(
        dataset$cabg,
        dataset$death,
        fisher = F,
        chisq = T,
        expected = F,
        sresid = F,
        format = "SPSS"
      )
    )
  
  output$h4_significant_parameter <-
    renderPrint(summary(h3.model))
  
  output$h5_significant_parameter <-
    renderPrint(summary(h3.model))
  
  output$h5_significant_parameter_2 <-
    renderPrint(summary(h2.model))
  
  output$h5_significant_parameter_3 <-
    renderPrint(summary(
      lm(
        death ~ los + age + dementia + metastatic_cancer + prior_dnas + senile,
        data = dataset
      )
    ))
  
  output$h5_significant_parameter_3 <-
    renderPrint(summary(
      lm(
        death ~ los + age + dementia + metastatic_cancer + prior_dnas + senile,
        data = dataset
      )
    ))
  
  output$h5_significant_parameter_4 <-
    renderPrint(summary(lm(death ~ age + prior_dnas, data = dataset)))
  
  output$h5_plot <-
    renderPlot(
      ggplot() +
        geom_point(
          data = grouped_number,
          aes(x = prior_dnas, y = as.numeric(death), size = grouped_number$count)
        ) +
        geom_smooth(
          data = dataset,
          method = "glm",
          se = F,
          fullrange = T,
          method.args = list(family = binomial),
          formula = y ~ x,
          aes(x = prior_dnas, y = as.numeric(death))
        ) +
        ylab("death") + xlim(0, 10) + ylim(0, 1) +
        theme(legend.position = "right", legend.title = element_blank()) +
        scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) +
        scale_y_continuous(breaks = c(0,1))
    )
  
  
  output$h3_crosstable_mental_health <-
    renderPrint(
      CrossTable(
        dataset$mental_health,
        dataset$death,
        fisher = F,
        chisq = T,
        expected = F,
        sresid = F,
        format = "SPSS"
      )
    )
  
  
  output$curve_age_dying <-
    renderPlot({
      ggplot(heart.failure, aes(x = age, y = as.numeric(death) - 1)) +
        geom_point(alpha = 1) +
        stat_smooth(
          method = "glm",
          aes(color = "red"),
          se = T,
          fullrange = T,
          method.args = list(family = binomial)
        ) +
        theme(legend.position = "none") +
        labs(title = "Effect of patient age on dying", x = "Patient age (in years)", y = "Chance of death") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + xlim(0, 150)
    })
  
  output$age_los_chart <-
    renderPlot({
      if (input$radio == 2) {
        heart.failure = heart.failure[heart.failure$death == 0, ]
        values = c("seagreen")
        labels = c("alive")
      }
      else if (input$radio == 3) {
        heart.failure = heart.failure[heart.failure$death == 1, ]
        values = c("chocolate")
        labels = c("dead")
      }
      else{
        values = c("seagreen", "chocolate")
        labels = c("alive", "dead")
      }
      ggplot(heart.failure, aes(x = age, y = los, color = death)) +
        geom_point(alpha = 1, size = 2) +
        scale_color_manual(values = values,
                           labels = labels,
                           name = "") +
        stat_smooth(
          method = "lm",
          aes(color = death),
          se = F,
          fullrange = F
        ) +
        theme(legend.position = "right") +
        labs(title = "Effect of patient age on the length of the hospitalisation", x = "Patient age (in years)", y = "Length of hospitalisation (in nights)")
    })
  
  output$age_chart <-
    renderPlot({
      mytitle <-
        paste(
          "Percentage death rate within the age group from",
          input$min_age,
          "to",
          input$max_age
        )
      if (input$chart_type == "Pie chart") {
        ggplot(data = data.frame(
          group = c("alive", "dead"),
          value = c(length(dataset$id[dataset$age >= input$min_age &
                                        dataset$age <= input$max_age &
                                        dataset$death == 0]),
                    length(dataset$id[dataset$age >= input$min_age &
                                        dataset$age <= input$max_age &
                                        dataset$death == 1]))
        ),
        aes(x = "", y = value, fill = group)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar("y", start = 0) +
          labs(title = mytitle, x = "", y = "") +
          scale_fill_brewer(palette = "Dark2") +
          geom_label_repel(
            aes(label = c(
              length(dataset$id[dataset$age >= input$min_age &
                                  dataset$age <= input$max_age &
                                  dataset$death == 0]),
              length(dataset$id[dataset$age >= input$min_age &
                                  dataset$age <= input$max_age &
                                  dataset$death == 1])
            )),
            size = 5,
            show.legend = F,
            nudge_x = 1
          ) +
          guides(fill = guide_legend(title = ""))
      } else{
        values <-
          c(length(dataset$id[dataset$age >= input$min_age &
                                dataset$age <= input$max_age &
                                dataset$death == 0]),
            length(dataset$id[dataset$age >= input$min_age &
                                dataset$age <= input$max_age &
                                dataset$death == 1]))
        names <- c("alive", "dead")
        named_vector <- setNames(values, names)
        waffle::waffle(named_vector, colors = c("seagreen", "chocolate")) +
          theme(legend.position = "right") +
          labs(title = mytitle, x = "", y = "")
      }
    })
  
  observe(updateSliderInput(session, "max_age", min = input$min_age))
  
}


shinyApp(ui, server)
