if (interactive()) {
  library(shiny)
  library(png)
  library(shinyWidgets)
  library(DT)
  library(dplyr)
  library(kableExtra)
  
  DimBooks <- read.csv("https://raw.githubusercontent.com/GiselaVillanueva/Portfolio_R/refs/heads/main/Shiny/DB%20-%20DimBooks.csv")
  DimUsers <- read.csv("https://github.com/GiselaVillanueva/Portfolio_R/raw/refs/heads/main/Shiny/DB%20-%20DimUsers.csv")
  FactBooks_read_by_Users <- read.csv("https://github.com/GiselaVillanueva/Portfolio_R/raw/refs/heads/main/Shiny/DB%20-%20FactBooks_read_by_Users.csv")
  Total_books <- DimBooks %>% summarise(Total = n())
  Books_read_and_Users <- FactBooks_read_by_Users %>%
    left_join(DimUsers, by = c("User_ID" = "ID"))
  Genres_Books_read_and_Users <- Books_read_and_Users %>%
    left_join(DimBooks, by = c("Book_ID" = "ID"))
  Most_read_Genre <- Genres_Books_read_and_Users %>%
    group_by(User_name, Genre) %>%
    summarise(
      Genres_read = n()) %>%
    arrange(desc(Genres_read))
  Show_Books_read_and_Users <- Genres_Books_read_and_Users %>%
    group_by(User_name) %>%
    summarise(
      Total = n(),
      Genres_read = n_distinct(Genre)
    ) %>%
    mutate(
      Completed = paste(round((Total / Total_books$Total)*100, 2), "%")
    ) %>%
    arrange(desc(Total))
  Pretty_Show_Books_read_and_Users <- Show_Books_read_and_Users %>% 
    kable(col.names = c("Member", "Total books read", "Genres read", "% read over total books"),
          align = "c") %>%
    kable_styling(font_size = 15, position = "center")
  Logo_url <- "https://github.com/GiselaVillanueva/Portfolio_R/raw/refs/heads/main/Shiny/MyBooks.png"
  Temp_file_logo <- tempfile(fileext = ".png")
  download.file(Logo_url, destfile = Temp_file_logo, mode = "wb")
  FAQ_url <- "https://github.com/GiselaVillanueva/Portfolio_R/raw/refs/heads/main/Shiny/FAQ.jpg"
  Temp_file_FAQ <- tempfile(fileext = ".jpg")
  download.file(FAQ_url, destfile = Temp_file_FAQ, mode = "wb")
  
  
  ui <- fluidPage(
    setBackgroundColor(
      color = c("#F7F2E6")
    ),
    titlePanel(
      div(class = "header",
          style = "text-align: center;",
          img(src = Logo_url, height = 300, width = 300),
          div(class = "title", "Welcome!")
      )
    ),
    
    div(
      br(),
      br(),
      style = "display: flex; flex-direction: column; align-items: center; justify-content: center;",
      br(),
      actionBttn(inputId = "click1", "See books and locations", size = "sm", style = "jelly", color = "success"),
      br(),
      actionBttn(inputId = "click2", "See readers' scores", size = "sm", style = "jelly", color = "success"),
      br(),
      actionBttn(inputId = "click3", "FAQ", size = "sm", style = "jelly", color = "success"),
      br(),
      br()
    ),
    uiOutput("mainUI")
  )
}


server <- function(input, output, session) {
  
  observeEvent(input$click1, {
    output$mainUI <- renderUI({
      sidebarLayout(
        sidebarPanel(
          pickerInput("GenreDropdown", "Genre", 
                      choices = unique(DimBooks$Genre), 
                      selected = unique(DimBooks$Genre),
                      options = list(`actions-box` = TRUE), 
                      multiple = TRUE),
          pickerInput("AuthorDropdown", "Author", 
                      choices = unique(DimBooks$Author), 
                      selected = unique(DimBooks$Author),
                      options = list(`actions-box` = TRUE), 
                      multiple = TRUE),
          pickerInput("TitleDropdown", "Title", 
                      choices = unique(DimBooks$Title), 
                      selected = unique(DimBooks$Title),
                      options = list(`actions-box` = TRUE), 
                      multiple = TRUE),
          pickerInput("RoomDropdown", "Room", 
                      choices = unique(DimBooks$Room), 
                      selected = unique(DimBooks$Room),
                      options = list(`actions-box` = TRUE), 
                      multiple = TRUE),
          width = 3
        ),
        mainPanel(
          DTOutput("BooksTable")
        )
      )
    })
    
    output$BooksTable <- renderDT({
      req(input$GenreDropdown, input$AuthorDropdown, input$TitleDropdown, input$RoomDropdown)
      DimBooks %>%
        filter(
          Genre %in% input$GenreDropdown,
          Author %in% input$AuthorDropdown,
          Title %in% input$TitleDropdown,
          Room %in% input$RoomDropdown
        ) %>%
        select(ID, Title, Author, Genre, Room, Furniture, Shelf_or_drawer_number)
    })
  })
  
  observeEvent(input$click2, {
    output$mainUI <- renderUI({
      tagList(
        tags$head(
          tags$style(HTML("
          .well {
            background-color: #F7F2E6 !important;
            border: none !important;
            box-shadow: none !important;
            border-top: none !important;
          }
        "))
        ),
        sidebarLayout(
          sidebarPanel(
            width = 2,
            p(" ")
          ),
          mainPanel(
            tableOutput("scoreTable")  
          )
        )
      )
    })
  })
  
    output$scoreTable <- renderText({
      Pretty_Show_Books_read_and_Users
    })
    
    observeEvent(input$click3, {
      output$mainUI <- renderUI(
        div(
          style = "display: flex; justify-content: center; align-items: center;",
          imageOutput("FAQ_Text")
        )
      )
    })
    
    output$FAQ_Text <- renderImage({
      list(
        src = Temp_file_FAQ,
        width = 725,
        height = 550
      )
    })
  }


shinyApp(ui, server)