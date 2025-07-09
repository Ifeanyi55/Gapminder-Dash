library(shiny)
library(bslib)
library(bs4Dash)
library(ellmer)
library(querychat)
library(gapminder)
library(highcharter)
library(reactable)
library(shinychat)

Sys.setenv(GOOGLE_API_KEY = "Your API Key")

# get Gemini api key
api_key = Sys.getenv("GOOGLE_API_KEY")

# create data of the first 500 rows in gapminder
gdp_data <- gapminder


# initialize querychat with mtcars dataset and Gemini chat function
querychat_config <- querychat_init(gdp_data,
                                   create_chat_func = purrr::partial(ellmer::chat_google_gemini, model = "gemini-2.0-flash"),
                                   greeting = "")

# define the UI of the dashboard
ui <- tabsetPanel(
  id = "tabs",
  type = "pills",
  tabPanel(
    id = "query-tab",
    title = strong("Data Query"),
    icon = icon("chart-line",lib = "font-awesome"),
    page_sidebar(
      id = "pg-side",
      # hover bounce effect of value boxes
      tags$head(
        tags$style(HTML("
      .bounce-hover:hover {
          animation: bounce 0.6s infinite alternate;
        }
      
        @keyframes bounce {
          from {
            transform: translateY(0);
          }
          to {
            transform: translateY(-10px);
          }
        }
      "))
      ),
      sidebar = querychat_sidebar("chat",width = "25%"),
      # to change div background color on hover: onmouseover = "this.style.backgroundColor='mediumseagreen';", onmouseout="this.style.backgroundColor='DodgerBlue';". In style = "cursor: pointer;
      fluidRow(column(4,tags$div(class = "bounce-hover",valueBoxOutput("GDP"), style = "text-align:center; font-size: 20px; font-weight: bold;color:black;")),
               column(4,tags$div(class = "bounce-hover",valueBoxOutput("Population"), style = "text-align:center; font-size: 20px; font-weight: bold;")),
               column(4,tags$div(class = "bounce-hover",valueBoxOutput("LifeExp"),style = "text-align:center; font-size: 20px; font-weight: bold;"))),
      fluidRow(column(6,highchartOutput("line",width = "98%")),
               column(6,highchartOutput("column",width = "98%"))),
      fluidRow(reactable::reactableOutput("table",width = "100%"))
    )
  ),
  tabPanel(
    id = "chat-tab",
    title = strong("EconoBot"),
    icon = icon("robot",lib = "font-awesome"),
    h3(strong("🤖 EconoBot"),style="text-align:center;color:DodgerBlue;"),
    h5(strong("Answers your economic and financial questions"),style="text-align:center;color:#37474f;"),
    chat_ui("gemini_chat",placeholder = "Type your message here...")
  )
)

# define the server logic required to interact with the data 
server <- function(input, output) {

  # create a querychat object using the config from step 1.
  query_chat <- querychat_server("chat", querychat_config)
  
  output$table <- reactable::renderReactable({
    reactable::reactable(query_chat$df(),
                         bordered = T,
                         compact = T,
                         highlight = T,
                         striped = T,
                         searchable = T,
                         filterable = T,
                         defaultColDef = colDef(align = "center"),
                         theme = reactable::reactableTheme(
                           stripedColor = "#0091ea",
                           borderColor = "#0091ea",
                           borderWidth = 3
                         ))
  })
  
  output$line <- renderHighchart({
    req(query_chat$df())
    my_colors <- c("Africa" = "#1f77b4", "Asia" = "#ff7f0e", "Europe" = "#2ca02c",
                   "Americas" = "#d62728", "Oceania" = "#9467bd")
    
    hchart(query_chat$df(),
                 type = "line",
                 hcaes(x = year, y = pop, group = continent)) |>
      hc_colors(unname(my_colors)) |>
      hc_title(text = "Population Per Continent or Country")
  })
  
  output$column <- renderHighchart({
    req(query_chat$df())
    my_colors <- c("Africa" = "#1f77b4", "Asia" = "#ff7f0e", "Europe" = "#2ca02c",
                   "Americas" = "#d62728", "Oceania" = "#9467bd")
    
    hchart(query_chat$df(),
           type = "column",
           hcaes(x = year, y = gdpPercap, group = continent)) |>
      hc_colors(unname(my_colors)) |>
      hc_title(text = "GDP Per Capita Per Continent or Country")
  })
  
  output$GDP <- renderValueBox({
    req(query_chat$df())
    valueBox(
      value = round(mean(query_chat$df()$gdpPercap, na.rm = TRUE), 2),
      color = "success",
      icon = icon("dollar"),
      elevation = 4,
      width = 4,
      subtitle = " ",
      footer = "Average GDP Per Capita"
    )
  })
  
  output$Population <- renderValueBox({
    req(query_chat$df())
    valueBox(
      value = round(mean(query_chat$df()$pop, na.rm = TRUE), 0),
      color = "primary",
      icon = icon("people-group"),
      elevation = 4,
      width = 4,
      subtitle = " ",
      footer = "Average Population"
    )
  })
  
  output$LifeExp <- renderValueBox({
    req(query_chat$df())
    valueBox(
      value = round(mean(query_chat$df()$lifeExp, na.rm = TRUE), 2),
      color = "warning",
      icon = icon("person-walking"),
      elevation = 4,
      width = 4,
      subtitle = " ",
      footer = "Average Life Expectancy"
    )
  })
  
  # write logic for Gemini chat
  gemini <- chat_google_gemini(
    system_prompt = "You are a friendly and helpful economic and financial expert who only answers questions around country economy and finance. If a user asks a question that is not related to economics or finance, give the following polite response: 'I'm sorry, I can only provide economic and financial information'.
    Also, if a user asks for specific or personal financial advice, give the following polite response: 'I'm sorry, I cannot provide specific financial advice. Please consult a financial advisor for personalized guidance.'",
    model = "gemini-2.0-flash",
    api_key = api_key,
    echo = T
  )
  
  observeEvent(input$gemini_chat_user_input, {
    
    # input$gemini_chat_user_input is from the chat id in the UI
    gemini$chat_async(input$gemini_chat_user_input)$then(function(response) {
      chat_append("gemini_chat", response)
    })$catch(function(error) {
      warning("An error occurred during chat_async: ", error$message)
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
