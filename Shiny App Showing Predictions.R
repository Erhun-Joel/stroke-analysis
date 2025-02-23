# Make a stroke website to determine if someone should go to the hospital
# Loading packages
library(tidyverse)
library(tidymodels)
library(httr)
library(jsonlite)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)

# Load fitted model
# Kindly impute the directory of the stroke fit to continue
# stroke.fit <- read_rds("")
stroke.fit

# Creating importance tibble for plotting
importance <-
tibble(
  variable = names(stroke.fit$fit$fit$fit$variable.importance),
  value = stroke.fit$fit$fit$fit$variable.importance
) %>%
  arrange(-value)
importance

# Create Gemini querying key
# Get your Gemini querying key and store here:
# google.key <- ""

# Creating Gemini querying function
gemini <- function(prompt,
                   api.key = NULL,
                   model = "gemini-1.5-flash-latest",
                   temperature = 0.2,
                   output.tokens.n = 1024){
  
  # Checking if api key was inputed and requesting input if not
  if(is.null(api.key)) api.key = readline(prompt = "Kindly input your api key here: ")
  
  # Posting and recieving response from Gemini
  response = POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/",
                 model,
                 ":generateContent"),
    query = list(key = api.key),
    content_type_json(),
    encode = "json",
    body = list(
      contents = list(
        parts = list(
          list(text = prompt)
        )
      ),
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = output.tokens.n
      )
    )
  )
  
  # Parsing out text
  gemini.speech = content(response)$candidates[[1]]$content$parts[[1]]$text
  
  # Returning text
  return(gemini.speech)
  
}

# Testing the query out
gemini("Hello! How was your day?", api.key = google.key) %>%
  HTML()

# Creating UI function
ui <- fluidPage(
  titlePanel(HTML("<b>STROKE PREDICTION APPLICATION</b>")),
  theme = shinytheme("lumen"),
  sidebarLayout(
    sidebarPanel(
      HTML("<b>General Information</b>"),
      hr(),
      fluidRow(
        column(12,
               sliderInput("age", "How old are you?", min = 0, max = 110, value = 20)
               )
      ),
      fluidRow(
        column(6,
               selectInput("city", "Do you live in a modernized City?", choices = c("Yes" = "Urban", "No" = "Rural"), selected = "Yes")
        ),
        column(6,
               selectInput("smoke", "Do you smoke?", choices = c("Not anymore" = "formerly smoked", "Never smoked" = "never smoked", "Consistently" = "smokes"))
        )
      ),
      fluidRow(
        column(6,
               selectInput("marriage", "Have you ever been married?", choices = c("Yes", "No"), selected = "No")
        ),
        column(6,
               selectizeInput("work",
                              "What type of work do you do?",
                              choices = c("Private" = "Private", "Self-employed" = "Self-employed", "Government" = "Govt_job", "Still a child" = "children", "Have never worked" = "Never_worked"),
                              multiple = FALSE)
        )
      ),
      hr(HTML("<b>Medical Information</b>")),
      fluidRow(
        column(6,
               selectInput("heart", "Have you had a heart condition before?", choices = c("Yes" = 1, "No" = 0))
               ),
        column(6,
               numericInput("glucose", "What is your glucose level (in mg/dL)?", min = 40, max = 300, value = 90)
               )
      ),
      fluidRow(
        column(6,
               selectInput("hyper", "Have you had Hypertension before?", choices = c("Yes" = 1, "No" = 0), selected = "Yes")
        ),
        column(6,
               numericInput("bmi", "What is your Body Mass Index?", min = 5, max = 120, value = 25)
        )
      ),
      actionButton("predict", HTML("<b>COMPUTE!</b>")),
      hr(HTML("<span>NOTE:</span><p> Initial values of numerical inputs represent averages and should be left as is if its value is unknown. </p>
              <p> Predictions are contained Results tab while the thinking behind these results are given in the Reasoning tab.</p>")),
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Results",
          icon = icon("bar-chart"),
          hr(),
          fluidRow(
            column(6,
                   withSpinner(valueBoxOutput("prob_one"))
                   ),
            column(6,
                   withSpinner(valueBoxOutput("prob_two"))
                   )
          ),
          hr(),
          fluidRow(
            textOutput("gemini_response")
          )
        ),
        tabPanel(
          "Model Reasoning",
          icon = icon("question"),
          fluidRow(
            column(6,
                   hr(),
                   h3("Plot of Probability Variance of Age along with Randomized Variables"),
                   p(),
                   h4("This plot takes imputed age and randomizes other variables to show how important age is to the occurance of stroke. Not only does it act as a model variable, it also increases the probability of other variables acting up."),
                   p(),
                   h5("Click the compute button without changing any variable")
                   ),
            column(6,
                   shinycssloaders::withSpinner(plotOutput("plot_age"))
                   )
          ),
          fluidRow(
            column(6,
                   hr(),
                   h3("Plot of the Variable Importance of Model Used"),
                   p(),
                   h4("This plot shows a summary of the variables which are important, helping the model of this App make its decisions. It is ranked by the magnitude the variables affect the outcome of calculated probabilities.")
            ),
            column(6,
                   shinycssloaders::withSpinner(plotOutput("plot_importance"))
            )
          )
        )
      )
    )
  ),
  fluidRow(
    column(6,
           helpText("Disclaimer: This app is simply a displaying App to show how such a project might be carried out. If experiencing actual stroke concerns, please see a doctor.")
           ),
    column(6,
           helpText("Source: The underlying model is developed using data from kaggle. A link to the data source is given ", a("here", href = "https://www.kaggle.com/datasets/fedesoriano/stroke-prediction-dataset"), ".")
           )
  )
)

# Creating the server function
server <- function(input, output){
  
  # Creating data variable that contains input from UI
  data = reactive({
    tibble(
      id = 10,
      gender = "Male",
      age = input$age,
      hypertension = input$hyper,
      heart_disease = input$heart,
      ever_married = input$marriage,
      work_type = input$work,
      Residence_type = input$city,
      avg_glucose_level = input$glucose,
      bmi = input$bmi,
      smoking_status = input$smoke
    ) %>%
      mutate(
        hypertension = as.numeric(hypertension),
        heart_disease = as.numeric(heart_disease)
      )
  })
  
  # Declaring prediction data and plot data
  current_pred <- reactiveVal()
  plot_data <- reactiveVal()
  
  # Encasing prediction and plot data in an observe event container so that they are recalculated only when the action button is clicked
  observeEvent(input$predict, {
    # Calculating stroke likelihood
    current_pred(
      stroke.fit %>%
        predict(data(), type = "prob") %>%
        mutate(.pred_class = as.factor(if_else(.pred_1 >= 0.015, 1, 0)))
    )
    
    # Randomizing important variables to see effect of the age variable
    plot_data(
      tibble(x = 5:110) %>%
        bind_cols(
          data()
        ) %>%
        mutate(age = 5:110,
               x = NULL,
               avg_glucose_level = case_when(
                 age < 18 ~ sample(90:130, length(age), replace = TRUE),
                 age < 65 ~ sample(80:130, length(age), replace = TRUE),
                 TRUE ~ sample(30:99, length(age), replace = TRUE)
               ),
               heart_disease = sample(c(1, 0), length(heart_disease), replace = TRUE),
               hypertension = sample(c(1, 0), length(hypertension), replace = TRUE))
    )
  })
  
  # Changing components only if the action button is clicked
  observeEvent(input$predict, {
    # Plot output
    output$plot_age <- renderPlot({
            
      predict(stroke.fit, plot_data(), type = "prob") %>%
        bind_cols(plot_data() %>% select(age)) %>%
        ggplot(aes(age, .pred_1)) +
        geom_line() +
        labs(
          x = "Age",
          y = "Probability of occurance"
        ) +
        theme_classic() +
        theme(axis.text = element_text(size = 15))
      
    })
    
    # Display box for probability occurance of stroke
    output$prob_one <- renderValueBox({
      valueBox(
        value = round(current_pred()$.pred_1, 2) * 100,
        subtitle = "% occurance probability"
      )
    })
    
    # Display box for probability occurance of no stroke
    output$prob_two <- renderValueBox({
      valueBox(
        value = round(current_pred()$.pred_0, 2) * 100,
        subtitle = "% non-occurance probability"
      )
    })
  })
  
  # Prompting and outputing Gemini for prediction interpretation
  output$gemini_response <- renderText({
    gemini(
      prompt = paste0(
        "Hello Gemini. I need you to make some inference from some results of a stroke prdiction to a third party user. A model was created and the probability of stroke occuring and not occuring was given as ",
        current_pred()$.pred_1, "and", current_pred()$.pred_0,
        "respectively. The cutoff of mark of stroke occuring is pred_1 >= 0.015. What do you make of this result?",
        "(Side note for your eyes only, not for the user: this prompt outside the bracket is gathered automatically from a shiny app. If the probabilities given above are zero, null or not numeric, it means the user hasn't entered them. If such is the case, ignore everthing and kindly ask the user input his/her data.",
        "Other things to note, this model is purposefully made to output results that have high predictions for cases of 1 in the expense of low prediction accuracy for 0. It is made to filter those who definetly won't have stroke so that stroke testing costs are reduced.",
        "Also, the App use a disclaimer to tell the view to see the doctor so you dont need to do that unless the probability is very high.",
        " Try to use paragraphs instead of bolding the text)."
      ),
      api.key = google.key
    ) %>%
      HTML()
  })
  
  # Plotting summary model's decision making process
  output$plot_importance <- renderPlot({
    
    importance %>%
      filter(value != 0) %>%
      ggplot(aes(as.numeric(value), fct_reorder(variable, as.numeric(value)), fill = variable)) +
      geom_col(show.legend = FALSE) +
      labs(
        x = "Magnitude",
        y = NULL
      ) +
      theme_classic() +
      theme(
        axis.text = element_text(size = 15),
        axis.text.x = element_blank()
      )
    
  })
  
}

# Run the Shiny Webpage
shinyApp(ui, server)
