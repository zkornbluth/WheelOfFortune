# Filename: app.R
# Description: Logic for Wheel of Fortune Dashboard
# Author: Zachary Kornbluth <github.com/zkornbluth>

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
library(plotly)

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

# Data spans 2001-2025
yearly_wheel_data <- tibble(year = c(2001:2025), win_pct = 0, puzzle_length = 0, pct_letters_revealed = 0)

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
# Combine plurals into singular (Places -> Place, Things -> Thing, etc), all smaller ones into "Other"
# Small categories (10 or fewer games): 
# Best Seller, Classic TV, Fictional Place, Landmark, 
# Quotation, Song Lyrics, On the Menu, In the Kitchen, 
# Rhyme Time, Title
small_cats <- c("Best Seller", "Character", "Characters", "Classic TV", "Fictional Place", 
                "Landmark", "On the Menu", "Quotation", "Rhyme Time", "Song Lyrics", "Title")
plurals <- c("Events", "Fictional Characters", "Living Things", "Occupations", "Places", "Things")

for (i in 1:nrow(wheel_data)) {
  cat = wheel_data[i, "Category"]
  if (cat %in% small_cats) {
    wheel_data[i, "Category"] <- "Other"
  } else if (cat %in% plurals) {
    wheel_data[i, "Category"] <- substr(cat, 1, nchar(cat) -1)
  } else if (cat == "People") { # Also a plural but not as simple as removing the 's'
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
  # All letters are guessable except R, S, T, L, N, and E
  guessable_letters <- c("A", "B", "C", "D", "F", "G", "H", "I", "J", "K",
               "M", "O", "P", "Q", "U", "V", "W", "X", "Y", "Z")
  games_to_check <- wheel_data %>% 
    filter(as.numeric(year) >= startYear,
           as.numeric(year) <= endYear)
  if (!is.null(category)) { # if no category, don't filter
    games_to_check <- games_to_check %>% 
      filter(Category == category)
  }
  
  vowels <- c("A", "I", "O", "U") # So we can color vowels differently
  
  letter_freq_data <- tibble(letter = guessable_letters, appearance_rate = 0, is_vowel = letter %in% vowels)
  
  for (i in 1:nrow(letter_freq_data)) {
    letter = letter_freq_data[i, 'letter'][[1]]
    contains_letter <- grepl(letter, games_to_check$puzzle_letters)
    num_games <- nrow(games_to_check)
    letter_freq_data[i, 'appearance_rate'] <- sum(contains_letter, na.rm=TRUE) / num_games
  }
  
  return(letter_freq_data)
}

# Only show category if there's games in the year range with that category
getCatsInYearRange <- function(startYear, endYear) {
  games_to_check <- wheel_data %>% 
    filter(as.numeric(year) >= startYear,
           as.numeric(year) <= endYear)
  
  return(unique(games_to_check$Category))
}

# For 'Year' axis breaks, so we don't get decimals
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

ui <- page_fillable(
  title = "WoF Dashboard",
  h1("Wheel of Fortune Bonus Round Dashboard"),
  # Year range controls (2001 - 2025)
  div(
    class = "d-flex align-items-center",
    sliderInput(
      "yearrange",
      label = "Year Range",
      min = 2001,
      max = 2025,
      value = c(2001, 2025),
      step = 1,
      sep = ""
    ),
    div(
      class = "pb-2",
      style = "margin-left: 1.5rem;",
      actionButton(
        "alltime",
        label = "All Games",
        width = "auto",
        class = "btn-sm btn-primary"
      )
    )
  ),
  # Show Categories switch
  materialSwitch(
    "categoriesOn",
    label="Show Categories",
    status="info",
    value=TRUE
  ),
  layout_columns(
    # Win Percentage plot
    card(
      card_header("Win Percentage", class="bg-dark"),
      plotlyOutput("winpctplot")
      ),
    # Puzzle Letter Frequency plot
    card(
      card_header("Puzzle Letter Frequency", class="bg-dark"),
      selectInput(
        "pickcategory",
        label = "Select a category:",
        choices=c("All", getCatsInYearRange(2001, 2025)),
        selected="All"
      ),
      plotlyOutput("letterfreqplot")
      ),
    # Average Puzzle Length plot
    card(
      card_header("Avg Puzzle Length", class="bg-dark"),
      plotlyOutput("puzzlengthplot")
      ),
    # Average Percentage of Puzzle Revealed plot
    card(
      card_header("Avg Percentage of Puzzle Revealed", class="bg-dark"),
      plotlyOutput("revealedplot")
      ),
    col_widths = c(6, 6, 6, 6), # each plot/card is half the screen width
    row_heights = c(1, 1)
  )
)

server <- function(input, output, session) {
  plot_data <- reactive({
    startYear <- input$yearrange[1]
    endYear <- input$yearrange[2]
    
    if (input$categoriesOn) {
      getCategorizedWheelDataYearFilter(startYear, endYear)
    } else {
      yearly_wheel_data %>%
        filter(as.numeric(year) >= startYear,
               as.numeric(year) <= endYear)
    }
  })
  
  letter_data <- reactive({
    startYear <- input$yearrange[1]
    endYear <- input$yearrange[2]
    
    if (input$pickcategory == "All") {
      getLetterFreq(startYear, endYear)
    } else {
      getLetterFreq(startYear, endYear, input$pickcategory)
    }
  })
  
  cat_options <- reactive({
    startYear <- input$yearrange[1]
    endYear <- input$yearrange[2]
    getCatsInYearRange(startYear, endYear)
  })
  
  # Update category dropdown based on selected years
  observeEvent(input$yearrange, {
    updateSelectInput(session, "pickcategory", choices = c("All", cat_options()))
  })
  
  # All Time button: reset slider to full range
  observeEvent(input$alltime, {
    updateSliderInput(session, "yearrange", value = c(2001, 2025))
  })
  
  # Render plots
  # Other than Puzzle Letter Frequency, the plots change on Show Categories
  # Show Categories is on: bar charts by category
  # Show Categories is off: line graphs by year
  
  # Render Win Percentage plot
  output$winpctplot <- plotly::renderPlotly({
    if (input$categoriesOn) {
      p <- ggplot(plot_data()) + 
        geom_col(
          mapping = aes(
            y = fct_reorder(category, win_pct),
            x = win_pct,
            text = paste0(
              "Category: ", category,
              "<br>Win Percentage: ", scales::percent(win_pct, accuracy = 0.1)
            )
          ),
          fill = "purple"
        ) + 
        labs(x = "Win Percentage", y = "Category") +
        scale_x_continuous(labels = scales::percent_format(scale = 100)) +
        theme_minimal()
      
      plotly::ggplotly(p, tooltip = "text") %>%
        layout(hovermode = "closest", margin = list(t = 35))
    } else {
      d <- plot_data()
      plotly::plot_ly(
        data = d,
        x = ~as.numeric(year),
        y = ~win_pct,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "purple", width = 2),
        marker = list(color = "purple", size = 6),
        text = ~paste0(
          "Year: ", year,
          "<br>Win Percentage: ", scales::percent(win_pct, accuracy = 0.1)
        ),
        hoverinfo = "text"
      ) %>%
        layout(
          xaxis = list(title = "Year", tickmode = "array"),
          yaxis = list(
            title = "Win Percentage",
            tickformat = ".0%",
            range = c(0, 0.5)
          ),
          hovermode = "closest",
          margin = list(t = 60)
        )
    }
  })
  
  # Render Puzzle Letter Frequency plot
  output$letterfreqplot <- plotly::renderPlotly({
    p <- ggplot(
      letter_data() |>
        dplyr::mutate(vowel_label = if_else(is_vowel, "Vowel", "Consonant"))
    ) +
      geom_col(
        mapping = aes(
          x = fct_rev(fct_reorder(letter, appearance_rate)),
          y = appearance_rate,
          fill = vowel_label,
          text = paste0(
            "Letter: ", letter,
            "<br>Appearance rate: ", scales::percent(appearance_rate, accuracy = 0.1)
          )
        )
      ) +
      labs(y = "Appearance Rate", x = "Letter", fill = "") +
      scale_y_continuous(labels = scales::percent_format(scale = 100)) +
      scale_fill_manual(values = c("Consonant" = "blue", "Vowel" = "red")) +
      theme_minimal()
    
    plotly::ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "closest", margin = list(t = 35))
  })
  
  # Render Average Puzzle Length plot
  output$puzzlengthplot <- plotly::renderPlotly({
    if (input$categoriesOn) {
      p <- ggplot(plot_data()) + 
        geom_col(
          mapping = aes(
            y = fct_reorder(category, puzzle_length),
            x = puzzle_length,
            text = paste0(
              "Category: ", category,
              "<br>Average Puzzle Length: ", round(puzzle_length, 1)
            )
          ),
          fill = "springgreen3"
        ) + 
        labs(x = "Average Puzzle Length", y = "Category") +
        theme_minimal()
      
      plotly::ggplotly(p, tooltip = "text") %>%
        layout(hovermode = "closest", margin = list(t = 35))
    } else {
      d <- plot_data()
      plotly::plot_ly(
        data = d,
        x = ~as.numeric(year),
        y = ~puzzle_length,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "springgreen3", width = 2),
        marker = list(color = "springgreen3", size = 6),
        text = ~paste0(
          "Year: ", year,
          "<br>Average Puzzle Length: ", round(puzzle_length, 1)
        ),
        hoverinfo = "text"
      ) %>%
        layout(
          xaxis = list(title = "Year", tickmode = "array"),
          yaxis = list(title = "Average Puzzle Length"),
          hovermode = "closest",
          margin = list(t = 60)
        )
    }
  })
  
  # Render Average Percentage of Puzzle Revealed plot
  output$revealedplot <- plotly::renderPlotly({
    if (input$categoriesOn) {
      p <- ggplot(plot_data()) + 
        geom_col(
          mapping = aes(
            y = fct_reorder(category, pct_letters_revealed),
            x = pct_letters_revealed,
            text = paste0(
              "Category: ", category,
              "<br>Average Percent of Letters Revealed: ",
              scales::percent(pct_letters_revealed, accuracy = 0.1)
            )
          ),
          fill = "deepskyblue"
        ) + 
        labs(x = "Average Percent of Letters Revealed", y = "Category") +
        scale_x_continuous(labels = scales::percent_format(scale = 100)) +
        theme_minimal()
      
      plotly::ggplotly(p, tooltip = "text") %>%
        layout(hovermode = "closest", margin = list(t = 35))
    } else {
      d <- plot_data()
      plotly::plot_ly(
        data = d,
        x = ~as.numeric(year),
        y = ~pct_letters_revealed,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "deepskyblue", width = 2),
        marker = list(color = "deepskyblue", size = 6),
        text = ~paste0(
          "Year: ", year,
          "<br>Average Percent of Letters Revealed: ",
          scales::percent(pct_letters_revealed, accuracy = 0.1)
        ),
        hoverinfo = "text"
      ) %>%
        layout(
          xaxis = list(title = "Year", tickmode = "array"),
          yaxis = list(
            title = "Average Percent of Letters Revealed",
            tickformat = ".0%"
          ),
          hovermode = "closest",
          margin = list(t = 60)
        )
    }
  })
}

shinyApp(ui, server)