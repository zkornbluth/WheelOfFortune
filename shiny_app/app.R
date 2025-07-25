#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyWidgets)
library(bslib)
library(scales)
library(ggplot2)
library(forcats)
library(dplyr)

if (FALSE) {
  library(munsell)
}

# Import our data, add puzzle length and pct_letters_revealed
wheel_data <- read.csv("wheeloffortune.csv", stringsAsFactors = FALSE)
wheel_data$Date <- as.Date(wheel_data$Date, format = "%B %d, %Y")
supplied_letters = c('R', 'S', 'T', 'L', 'N', 'E') # players are given RSTLNE before they choose more

wheel_data$puzzle_letters <- gsub("[^a-zA-Z]", "", wheel_data$Puzzle)
wheel_data$puzzle_length <- nchar(wheel_data$puzzle_letters)
wheel_data <- wheel_data |>
  rowwise() |>
  mutate(num_revealed_letters = sum(sapply(supplied_letters, function(l) {
    lengths(regmatches(puzzle_letters, gregexpr(l, puzzle_letters, ignore.case = TRUE)))
  })))
wheel_data$pct_letters_revealed <- wheel_data$num_revealed_letters / wheel_data$puzzle_length

wheel_data$year <- wheel_data$Date %>% format("%Y")

# Data spans 2001-2016
yearly_wheel_data <- tibble(year = c(2001:2016), win_pct = 0, puzzle_length = 0, pct_letters_revealed = 0)

# For each year - collect that year's games
# Calculate win %, avg puzzle length, avg % of letters revealed

for (y in 1:nrow(yearly_wheel_data)) {
  yr = yearly_wheel_data[y, 'year']
  this_years_games <- wheel_data %>% 
    filter(year == yr)
  yearly_win_pct <- nrow(filter(this_years_games, `Win.` == "Yes")) / nrow(this_years_games)
  yearly_wheel_data[y, 'win_pct'] <- yearly_win_pct
  yearly_wheel_data[y, 'puzzle_length'] <- mean(this_years_games$puzzle_length)
  yearly_wheel_data[y, 'pct_letters_revealed'] <- mean(this_years_games$pct_letters_revealed)
}

# Handle category data
# Combine plurals into singular (Places -> Place, Things -> Thing, etc) all smaller ones into "Other"
# Small categories (10 or fewer games): Best Seller, Classic TV, Fictional Place, Landmark, Quotation, Song Lyrics, On the Menu, In the Kitchen, Rhyme Time, Title
small_cats <- c("Best Seller", "Classic TV", "Fictional Place", "Landmark", "Quotation", "Song Lyrics", "On the Menu", "In the Kitchen", "Rhyme Time", "Title")
plurals <- c("Places", "Things", "Fictional Characters", "Living Things")

for (i in 1:nrow(wheel_data)) {
  cat = wheel_data[i, "Category"]
  if (cat %in% small_cats) {
    wheel_data[i, "Category"] <- "Other"
  } else if (cat %in% plurals) {
    wheel_data[i, "Category"] <- substr(cat, 1, nchar(cat) -1)
  } else if (cat == "People") {
    wheel_data[i, "Category"] <- "Person"
  }
}

# Functions - for data that needs to be updated live with year/category changes
getCategorizedWheelDataYearFilter <- function(startYear, endYear) {
  categorized_wheel_data <- tibble(
    category = unique(wheel_data$Category),
    win_pct = 0,
    puzzle_length = 0,
    pct_letters_revealed = 0
  )
  
  for (x in 1:nrow(categorized_wheel_data)) {
    cat <- categorized_wheel_data$category[x]
    this_cats_games <- wheel_data %>%
      filter(Category == cat,
             year >= startYear,
             year <= endYear)
    
    num_games <- nrow(this_cats_games)
    
    if (num_games > 0) {
      categorized_wheel_data$win_pct[x] <- nrow(filter(this_cats_games, `Win.` == "Yes")) / num_games
      categorized_wheel_data$puzzle_length[x] <- mean(this_cats_games$puzzle_length)
      categorized_wheel_data$pct_letters_revealed[x] <- mean(this_cats_games$pct_letters_revealed)
    } else {
      # If no data, use NA so it doesn't mess up plots
      categorized_wheel_data$win_pct[x] <- NA
      categorized_wheel_data$puzzle_length[x] <- NA
      categorized_wheel_data$pct_letters_revealed[x] <- NA
    }
  }
  
  # Filter out rows where all values are NA (no data in selected years)
  categorized_wheel_data %>% filter(!is.na(win_pct))
}

getLetterFreq <- function(startYear, endYear, category=NULL) {
  guessable_letters <- c("A", "B", "C", "D", "F", "G", "H", "I", "J", "K",
               "M", "O", "P", "Q", "U", "V", "W", "X", "Y", "Z")
  games_to_check <- wheel_data %>% 
    filter(as.numeric(year) >= startYear,
           as.numeric(year) <= endYear)
  if (!is.null(category)) {
    games_to_check <- games_to_check %>% 
      filter(Category == category)
  }
  
  vowels <- c("A", "I", "O", "U")
  
  letter_freq_data <- tibble(letter = guessable_letters, appearance_rate = 0, is_vowel = letter %in% vowels)
  
  for (i in 1:nrow(letter_freq_data)) {
    letter = letter_freq_data[i, 'letter'][[1]]
    contains_letter <- grepl(letter, games_to_check$puzzle_letters)
    num_games <- nrow(games_to_check)
    letter_freq_data[i, 'appearance_rate'] <- sum(contains_letter, na.rm=TRUE) / num_games
  }
  
  return(letter_freq_data)
}

getCatsInYearRange <- function(startYear, endYear) {
  games_to_check <- wheel_data %>% 
    filter(as.numeric(year) >= startYear,
           as.numeric(year) <= endYear)
  
  return(unique(games_to_check$Category))
}

ui <- page_fillable(
  title = "Bonus Round",
  h1("Wheel of Fortune Bonus Round Dashboard"),
  airYearpickerInput(
    "yearpicker",
    label="Year Range",
    multiple=FALSE,
    range=TRUE,
    min="2001-01-01",
    max="2016-12-31",
    autoClose=TRUE,
    addon='none',
    update_on='close'
  ),
  materialSwitch(
    "categoriesOn",
    label="Show Categories",
    status="info"
  ),
  layout_columns(
    card(
      card_header("Win Percentage", class="bg-dark"),
      plotOutput("winpctplot")
      ),
    card(
      card_header("Puzzle Letter Frequency", class="bg-dark"),
      selectInput(
        "pickcategory",
        label = "Select a category:",
        choices=c("All", getCatsInYearRange(2001, 2016)),
        selected="All"
      ),
      plotOutput("letterfreqplot")
      ),
    card(
      card_header("Avg Puzzle Length", class="bg-dark"),
      plotOutput("puzzlengthplot")
      ),
    card(
      card_header("Avg Percentage of Puzzle Revealed", class="bg-dark"),
      plotOutput("revealedplot")
      ),
    col_widths = c(6, 6, 6, 6),
    row_heights = c(1, 1)
  )
)

server <- function(input, output, session) {
  plot_data <- reactive({
      if (input$categoriesOn & is.null(input$yearpicker)) {
        getCategorizedWheelDataYearFilter(2001, 2016)
        
      } else if (input$categoriesOn & !is.null(input$yearpicker)) {
        startYear <- lubridate::year(as.Date(input$yearpicker[1]))
        endYear <- lubridate::year(as.Date(input$yearpicker[2]))
        getCategorizedWheelDataYearFilter(startYear, endYear)
        
      } else if (!input$categoriesOn & is.null(input$yearpicker)) {
        yearly_wheel_data
        
      } else {
        startYear <- lubridate::year(as.Date(input$yearpicker[1]))
        endYear <- lubridate::year(as.Date(input$yearpicker[2]))
        yearly_wheel_data %>%
          filter(as.numeric(year) >= startYear,
                 as.numeric(year) <= endYear)
      }
    })
  
  letter_data <- reactive({
    if (is.null(input$yearpicker) & input$pickcategory == "All") {
      getLetterFreq(2001, 2016)
    } else if (input$pickcategory == "All") {
      startYear <- lubridate::year(as.Date(input$yearpicker[1]))
      endYear <- lubridate::year(as.Date(input$yearpicker[2]))
      getLetterFreq(startYear, endYear)
    } else if (is.null(input$yearpicker)) {
      getLetterFreq(2001, 2016, input$pickcategory)
    } else {
      startYear <- lubridate::year(as.Date(input$yearpicker[1]))
      endYear <- lubridate::year(as.Date(input$yearpicker[2]))
      getLetterFreq(startYear, endYear, input$pickcategory)
    }
  })
  
  cat_options <- reactive({
    if (is.null(input$yearpicker)) {
      getCatsInYearRange(2001, 2016)
    } else {
      startYear <- lubridate::year(as.Date(input$yearpicker[1]))
      endYear <- lubridate::year(as.Date(input$yearpicker[2]))
      getCatsInYearRange(startYear, endYear)
    }
  })
  
  # Update category dropdown based on selected years
  observeEvent(input$yearpicker, {
    updateSelectInput(session, "pickcategory", choices = c("All", cat_options()))
  })
  
  output$winpctplot <- renderPlot(
    if (input$categoriesOn) {
      ggplot(plot_data()) + 
        geom_col(mapping=aes(y=fct_reorder(category, win_pct), x=win_pct), fill="purple") + 
        labs(x = "Win Percentage", y = "Category") +
        scale_x_continuous(labels = scales::percent_format(scale = 100)) +
        theme_minimal()
    } else {
      ggplot(plot_data()) + 
        geom_path(mapping=aes(year, win_pct), color="purple") + 
        labs(x = "Year", y = "Win Percentage") +
        scale_y_continuous(labels = scales::percent_format(scale = 100)) +
        theme_minimal()
    }
  )
  
  output$letterfreqplot <- renderPlot(
    ggplot(letter_data()) +
      geom_col(mapping=aes(x=fct_rev(fct_reorder(letter, appearance_rate)), y=appearance_rate, fill=is_vowel)) +
      labs(y = "Appearance Rate", x = "Letter", fill = "") +
      scale_y_continuous(labels = scales::percent_format(scale = 100)) +
      scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue"), labels = c("Consonant", "Vowel")) +
      theme_minimal()
  )
  
  output$puzzlengthplot <- renderPlot(
    if (input$categoriesOn) {
      ggplot(plot_data()) + 
        geom_col(mapping=aes(y=fct_reorder(category, puzzle_length), x=puzzle_length), fill="green") + 
        labs(x = "Average Puzzle Length", y = "Category") +
        theme_minimal()
    } else {
      ggplot(plot_data()) + 
        geom_path(mapping=aes(year, puzzle_length), color="green") + 
        labs(x = "Year", y = "Average Puzzle Length") +
        theme_minimal()
    }
  )
  
  output$revealedplot <- renderPlot(
    if (input$categoriesOn) {
      ggplot(plot_data()) + 
        geom_col(mapping=aes(y=fct_reorder(category, pct_letters_revealed), x=pct_letters_revealed), fill="brown") + 
        labs(x = "Average Percent of Letters Revealed", y = "Category") +
        scale_x_continuous(labels = scales::percent_format(scale = 100)) +
        theme_minimal()
    } else {
      ggplot(plot_data()) +
        geom_path(mapping=aes(year, pct_letters_revealed), color="brown") + 
        labs(x = "Year", y = "Average Percent of Letters Revealed") +
        scale_y_continuous(labels = scales::percent_format(scale = 100)) +
        theme_minimal()
    }
  )
}

shinyApp(ui, server)