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

linebreaks <- function(n) {
  HTML(strrep(br(), n))
}

dataset <- read_csv("dataset_heartFailure.csv")
dataset_without_NA <- na.omit(dataset)
dataset_correlations <- cor(dataset_without_NA)
parameter_description <- read_csv("parameter_description.csv")
parameter_description <-
  subset(parameter_description, select = c(parameter, description))
heart.failure <-
  subset(dataset_without_NA, select = -c(ethnicgroup, id))

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
    "quintile"
  )

heart.failure[cols] <- lapply(heart.failure[cols], factor)

fullmodel <-
  glm(death ~ ., data = heart.failure, family = binomial())

nullmodel <-
  glm(death ~ 1, data = heart.failure, family = binomial()) # Null model

backwards <- step(fullmodel, trace = 0)

forwards <-
  step(
    nullmodel,
    scope = list(lower = formula(nullmodel), upper = formula(fullmodel)),
    direction = "forward",
    trace = 0
  )

bothways <-
  step(
    nullmodel,
    scope = list(lower = formula(nullmodel), upper = formula(fullmodel)),
    direction = "both",
    trace = 0
  )

h4.los <- glm(death ~ los, data = heart.failure, family = binomial)

h4.interaction <- lm(los ~ age, data = heart.failure)

grouped_prior_dnas <-
  dataset %>% group_by(prior_dnas, death) %>% summarise(count = n())
grouped_age <-
  dataset %>% group_by(age, death) %>% summarise(count = n())
grouped_los <-
  dataset %>% group_by(los, death) %>% summarise(count = n())





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
              h4("To test the hypothesis, let's create a first plot"),
              br(),
              plotOutput("gender_plot_1"),
              br(),
              p(
                "The bar plot above shows that the amount of men and women are about equally distributed in cases of survival and death."
              ),
              br(),
              h4(
                "To explore the hypothesis even further, we'll use the CrossTable-function"
              ),
              p(
                "The Chi^2 statistic is part of the CrossTable-function and tests whether the two variables are independent. If the test produces a significant result (p less than 0.05 or better 0.01) that is larger than the critical values of the Chi^2 statistic at a certain degree of freedom (with df=1: Chi^2 larger than 3.85 at p=0.05 or larger than 6.63 at p=0.01) we can conclude that there is a significant relationship between the variables."
              ),
              verbatimTextOutput("gender_CrossTable"),
              p(
                "Here the Chi^2 statistic is smaller than the critical values and the p value is far over 0.05 so we can conclude that the variable are independent. I.e. there is no relationship between gender and survival/death which is supported by the bar plot above."
              ),
              br(),
              h4("Lets make a last proof with a logistic linear model"),
              verbatimTextOutput("gender_linearModel"),
              p(
                "The summary of the logistic linear model shows that the p-value of 0.837 is much larger than 0.001. Thus, the linear model also shows that gender is not significant in relation to death."
              ),
              br(),
              h4("Final assessment of the hypothesis"),
              p(
                "The distribution of mortality is very similar for men and women. The hypothesis thus seems to be confirmed. However, this does not have to mean that gender has no influence on the chances of survival, as we will see when analyzing hypotheses 2 and 3."
              )
            )
          ),
          tabPanel(
            "Hypothesis 2 & 3",
            wellPanel(
              h3(
                "Hypothesis 2: There are comorbidities which feature significantly in fatalities."
              ),
              h3(
                "Hypothesis 3: There are other comorbidities which feature significantly in survival."
              ),
              br(),
              h4(
                "Approach: Looking for the best model step by step to find the significant predictor."
              ),
              p(
                "We want to see what effect each predictor factor has on the outcome of hospitalization (survival or death). So we try a logistic regression with all predictors. In the picture below, one can see, how we chose the model type."
              ),
              HTML('<left><img src="modeltype.png" width="900"></left>'),
              br(),
              br(),
              p(
                "And below, you can see the logistic regression model with all predictors."
              ),
              verbatimTextOutput("h2_summary_lm_all"),
              br(),
              p(
                "Our models residual deviance is smaller than the null deviance, so having all parameters in our model gives a better prediction of the outcome of hospitalization than having no parameters in (just random guessing). There are however many parameters that appear to be not significant. So lets try a stepwise logistic regression to find the best combination of parameters to predict survival of patients."
              ),
              p(
                "Let's try to go backward (start with a full model and try removing parameters), forward (the opposite of backward) and both ways!"
              ),
              wellPanel(
                p(
                  "Let's compare the AIC of the different models created with the different methods."
                ),
                p("Backwards:"),
                verbatimTextOutput("h2_backwards"),
                p("Forwards:"),
                verbatimTextOutput("h2_forwards"),
                p("Both ways:"),
                verbatimTextOutput("h2_bothways"),
                p(
                  "The AIC of all three models is 1174.165, so all three approaches seem to have resulted in the same model."
                )
              ),
              wellPanel(
                p("Let's show the used model, which has the smallest AIC-value:"),
                verbatimTextOutput("h2_formula_forwards"),
              ),
              wellPanel(
                p("Let's have a look at the predictors of this model"),
                verbatimTextOutput("h2_best_model"),
                p(
                  "So what do the numbers mean? For age which is numerical the estimate is 0.07. Each additional year increases the chance of dying by 0.07. For a categorical predictor like gender an estimate of -0.47 means that if the patient is female the chance of dying is decreased by 0.47."
                )
              ),
              wellPanel(
                p("Lets consider the coefficients/predictors"),
                verbatimTextOutput("h2_names_coefficients"),
                p(
                  "Lets discard 'crt' since its p-Values doesn't indicate as significant. The remaining predictors are:"
                ),
                verbatimTextOutput("h2_remaining"),
              ),
              br(),
              h4("Final assessment of the hypothesis"),
              p(
                "The hypothesis 2 and 3 seems to be confirmed. There are comorbidities which feature significantly in fatalities and other which feature significantly in survival."
              ),
              p(
                "Good for survival is having a bypass (cabg), arrhythmias and being female."
              ),
              p(
                "Bad for survival is having metastatic cancer, being old, missed many outpatient appointments (prior_dnas) or a long stay in the hospital."
              ),
              br(),
              h4("Further Analysis"),
              p(
                "Since we have seen, that the predictor 'age' with a p-value of 2e-16 is very significant, we will do some further analysis."
              ),
              br(),
              plotOutput("curve_age_dying"),
              linebreaks(2),
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
              )
              ,
              mainPanel(wellPanel(plotOutput("age_chart"), )),
              linebreaks(25),
            )
          ),
          tabPanel(
            "Hypothesis 4",
            wellPanel(
              h3(
                "Hypothesis 4: A longer follow-up time has a positive effect on survival"
              ),
              br(),
              h4("Data"),
              p(
                "Explination of parameter fu_time: follow up time (time spent monitoring the patients health after treatment)"
              ),
              p(
                "Explination of parameter los: length of stay (nights spent at the hospital)"
              ),
              br(),
              h4("Reuse  the analysis of hypothesis 2 & 3"),
              p(
                "As we have seen in the analysis of hypothesis 2 & 3, the parameter fu_time does not have a significant effect on the chance of survival. Therefore, we will have a look at the parameter 'los' instead."
              ),
              br(),
              h4("Predictor 'los'"),
              p("First of all, we will have a look at the AIC value of 'los'"),
              verbatimTextOutput("h4_los_summary"),
              p(
                "As we can see, the AIC value 1304.2 is better than the value of 'Null deviance'. Therefore, we will do some further analysis."
              ),
              p("Lets plot the regression graph:"),
              plotOutput("h4_los_plot"),
              br(),
              p(
                "As we can see, to have a long stay in hospital seems to have a negative effect on the chance of survival. But we have to concider to aspects here. On the one hand, the plot shows that the confidence bands around the predictor are quite large, so 'los' alone is not a very good predictor of patient mortality. On the other hand, we have to ask, wheter the length of stay is more effect or more consequence of a fatale outcome."
              ),
              br(),
              h4("Combination of 'los' and 'age'"),
              p(
                "Finally, we will have a look at the interaction of los and age, the two most significant factors in our model with the smallest AIC-value from hypothesis 2 & 3."
              ),
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
              mainPanel(wellPanel(plotOutput("age_los_chart"))),
              p(
                "As we can see in the plot above, the length of stay and the age seems to have a correlation. The older the patient, the longer the stay. Especially in cases where the patient survives."
              ),
              p("We will check this with a new model."),
              verbatimTextOutput("h4_interaction"),
              br(),
              p(
                "The model shows, that there is a significant effect. Each year increases the stay by 0.0978 nights in average."
              ),
              br(),
              h4("4. Final assessment of the hypothesis"),
              p(
                "Our analysis shows, that there is no significant correlation between follow-up time an the chance of survival. Therefore, the hypothesis 4 is not confirmed. But we could show, that there is a negative correleation between length of stay and chance of survival."
              ),
            ),
          ),
            tabPanel(
              "Hypothesis 5",
              wellPanel(
                h3(
                  "Hypothesis 5: Missing fewer outpatient appoitments has a positive effect on survival"
                ),
                br(),
                h4("Data"),
                p(
                  "Explination of parameter prior_dnas: number of outpatient appointments missed in the previous year"
                ),
                br(),
                h4("Reuse  the analysis of hypothesis 2 & 3"),
                p(
                  "As we have seen in the analysis of hypothesis 2 & 3, missing outpatient appointments has a significant negative effect on the chance of survival."
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
                  "Furthermore, we consider the linear model if we only use the most significant parameter 'age' and the parameter 'prior_dnas'."
                ),
                verbatimTextOutput("h5_significant_parameter_4"),
                p("Here, we can see that 'prior_dnas' is still significant."),
                p(
                  "Finally, we will plot the correlation between 'death' and 'prior_dnas'."
                ),
                plotOutput("h5_plot"),
                p(
                  "As one can see, the graph increases with the number of missed appointments, what further supports our hypothesis."
                ),
                br(),
                h4("4. Final assessment of the hypothesis"),
                p(
                  "Our hypothesis seems to be confirmed, so that missed appointments have a negativ effect on survival."
                ),
                br(),
              )
            ),
            tabPanel(
              "Summary of the results",
              h3("Results of the analysis summarized"),
              HTML('<left><img src="confirmed.png" width="600"></left>'),
              br(),
              br(),
            ),
          )
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
          
          output$data <- renderTable(dataset, digits = 0)
          
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
            renderPrint(summary(glm(
              death ~ gender, data = dataset, family = binomial()
            )))
          
          output$h2_summary_lm_all <-
            renderPrint(summary(fullmodel))
          
          output$h2_backwards <-
            renderPrint(backwards$aic)
          
          output$h2_forwards <-
            renderPrint(forwards$aic)
          
          output$h2_bothways <-
            renderPrint(bothways$aic)
          
          output$h2_formula_forwards <-
            renderPrint(formula(forwards))
          
          output$h2_best_model <-
            renderPrint(summary(forwards))
          
          output$h2_names_coefficients <-
            renderPrint(names(forwards$coefficients))
          
          output$h2_remaining <-
            renderPrint(forwards$coefficients[c(
              "age" ,
              "los" ,
              "metastatic_cancer1",
              "prior_dnas",
              "gender2",
              "cabg1",
              "arrhythmias1"
            )])
          
          output$h4_los_summary <-
            renderPrint(summary(h4.los))
          
          output$h4_los_plot <-
            renderPlot(
              ggplot() +
                geom_point(
                  data = grouped_los,
                  aes(x = los, y = as.numeric(death)),
                  size = grouped_los$count
                ) +
                stat_smooth(
                  data = heart.failure,
                  method = "glm",
                  se = T,
                  fullrange = T,
                  method.args = list(family = binomial),
                  aes(x = los, y = as.numeric(death) - 1)
                ) +
                ylab("Death") + xlim(-200, 200) + ylim(-0.25, 1.25)
            )
          
          output$h4_interaction <-
            renderPrint(summary(h4.interaction))
          
          
          
          
          
          
          
          
          
          output$h5_significant_predictor <-
            renderPrint(summary(h3.model))
          
          output$h5_significant_predictor_2 <-
            renderPrint(summary(h2.model))
          
          output$h5_significant_predictor_3 <-
            renderPrint(summary(
              glm(
                death ~ los + age + dementia + metastatic_cancer + prior_dnas + senile,
                data = dataset,
                family = binomial()
              )
            ))
          
          
          
          
          
          
          
          
          output$h4_significant_parameter <-
            renderPrint(summary(h3.model))
          
          output$h5_significant_parameter <-
            renderPrint(summary(h3.model))
          
          output$h5_significant_parameter_2 <-
            renderPrint(summary(h2.model))
          
          output$h5_significant_parameter_3 <-
            renderPrint(summary(
              glm(
                death ~ los + age + dementia + metastatic_cancer + prior_dnas + senile,
                data = dataset,
                family = binomial()
              )
            ))
          
          output$h5_significant_parameter_4 <-
            renderPrint(summary(glm(
              death ~ age + prior_dnas, data = dataset, family = binomial()
            )))
          
          output$h5_plot <-
            renderPlot(
              ggplot() +
                geom_point(
                  data = grouped_prior_dnas,
                  aes(
                    x = prior_dnas,
                    y = as.numeric(death),
                    size = grouped_prior_dnas$count
                  )
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
                scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
                scale_y_continuous(breaks = c(0, 1))
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
              ggplot() +
                geom_point(data = grouped_age,
                           aes(x = age, y = as.numeric(death)),
                           size = grouped_age$count) +
                stat_smooth(
                  data = heart.failure,
                  method = "glm",
                  se = T,
                  fullrange = T,
                  method.args = list(family = binomial),
                  aes(
                    x = age,
                    y = as.numeric(death) - 1,
                    color = "red"
                  )
                ) +
                theme(legend.position = "none") +
                labs(title = "Effect of patient age on dying", x = "Patient age (in years)", y = "Chance of death") +
                scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + xlim(0, 150) + ylim(-0.25, 1.25)
            })
          
          output$age_los_chart <-
            renderPlot({
              if (input$radio == 2) {
                heart.failure = heart.failure[heart.failure$death == 0,]
                values = c("seagreen")
                labels = c("alive")
              }
              else if (input$radio == 3) {
                heart.failure = heart.failure[heart.failure$death == 1,]
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
        