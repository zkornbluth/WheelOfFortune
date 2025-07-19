# Wheel of Fortune Bonus Round Dashboard

If you're ever on Wheel of Fortune and make it to the Bonus Round, this dashboard will help you choose a category and the best letters to help you solve the puzzle.

## Data Source

I obtained this data from [https://www.angelfire.com/mi4/malldirectories/wheel/wheelbonus.html](https://www.angelfire.com/mi4/malldirectories/wheel/wheelbonus.html). 
I gathered every game from September 2001 (when prizes changed to just cash or cars) to March 2016 (the most recent the site included).

## Features
The dashboard displays the following graphs:
* Bonus round win rate by year
* Frequency of guessable letters (excludes RSTLNE)
* Average puzzle length by year
* Average percentage of puzzle revealed with RSTLNE by year

The dashboard includes the following configuration options:
* Selecting a start and end year filters the data used in all graphs to the games between the selected years.
* Clicking the "Show Categories" toggle changes the three yearly graphs to bar charts comparing categories.
* Selecting a category under Puzzle Letter Frequency shows the frequency of guessable letters for that category's puzzles.

[<img width="1470" height="919" alt="dashboard-1" src="https://github.com/user-attachments/assets/7972ab2a-bbeb-40f5-b39d-e25eb8566e4c" />]: # 
<img width="1470" height="919" alt="dashboard-4" src="https://github.com/user-attachments/assets/f4ed3e9b-ea73-49b7-9823-d2451d0094e6" />

<img width="1470" height="919" alt="dashboard-2" src="https://github.com/user-attachments/assets/48c362c3-7c44-4201-8463-dea2d7371aa5" />

[Future - add gif of scrolling to choose category]: #

<img width="717" height="321" alt="dashboard-3" src="https://github.com/user-attachments/assets/b352edea-a50d-48bc-ab2d-28cf5de157fd" />


## Getting Started

### 1. Clone the repository
```bash
git clone https://github.com/zkornbluth/WheelOfFortune.git
cd WheelOfFortune
```

### 2. Open RStudio and install dependencies
The packages used are `shiny`, `shinyWidgets`, `tidyverse`, `bslib`, and `scales`. There are two easy ways to install these:

Option 1: Copy and paste this into the RStudio console:
```r
install.packages("shiny")
install.packages("shinyWidgets")
install.packages("tidyverse")
install.packages("bslib")
install.packages("scales")
```

Option 2: Navigate to the Packages tab in RStudio and click "Install".
<img width="669" height="564" alt="Screenshot 2025-07-19 at 8 55 28 AM" src="https://github.com/user-attachments/assets/19f25787-c49e-49c7-ac5d-eee47bc90e63" />

In the popup window, enter the packages separated by a space or comma and click "Install".
<img width="383" height="283" alt="Screenshot 2025-07-19 at 8 57 24 AM" src="https://github.com/user-attachments/assets/a0a5c559-8dab-4b78-a728-b16aa31fcecd" />


### 3. Run the app
Click the "Run App" button in RStudio.
