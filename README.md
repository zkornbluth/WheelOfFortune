# Wheel of Fortune Bonus Round Dashboard

If you're ever on Wheel of Fortune and make it to the Bonus Round, this dashboard will help you choose a category and the best letters to help you solve the puzzle.
Check it out here: [http://zkornbluth.github.io/WheelOfFortune](http://zkornbluth.github.io/WheelOfFortune)

## What is the Wheel of Fortune Bonus Round?

At the end of the regular puzzles in every Wheel of Fortune episode, the highest-scoring contestant advances to the Bonus Round - a single puzzle for a chance at an extra prize. The contestant gets to select the category from a set of three options, then they're shown a puzzle board and the standard letters R, S, T, L, N, and E are automatically revealed. The contestant then chooses three more consonants and one more vowel (and a fourth consonant if they've earned a Wild Card). Once those are revealed, they get 10 seconds to solve the puzzle.

The goal of this dashboard is to help contestants choose a category and their letters for the best chance at correctly solving the Bonus Round puzzle.

## Data Source

I obtained this data from two sources:
- [https://www.angelfire.com/mi4/malldirectories/wheel/wheelbonus.html](https://www.angelfire.com/mi4/malldirectories/wheel/wheelbonus.html)
- [https://andynwof.wordpress.com/](https://andynwof.wordpress.com/)
  
Games September 2001 - March 2016 are from the first source. Games April 2016 - June 2025 are from the second source.

Contestants were able to choose their Bonus Round category from three options starting in September 2017.

## Features
The dashboard displays the following graphs:
* Bonus round win rate by year
* Frequency of guessable letters (excludes RSTLNE)
* Average puzzle length by year
* Average percentage of puzzle revealed with RSTLNE by year

The dashboard includes the following configuration options:
* Selecting a start and end year filters the data used in all graphs to the games between the selected years.
* Clicking the "Show Categories" toggle changes between bar charts of categories and line graphs over time. The toggle defaults to On.
* Selecting a category under Puzzle Letter Frequency shows the frequency of guessable letters for that category's puzzles.

[<img width="1470" height="919" alt="dashboard-1" src="https://github.com/user-attachments/assets/7972ab2a-bbeb-40f5-b39d-e25eb8566e4c" />]: # 
[<img width="1470" height="919" alt="dashboard-4" src="https://github.com/user-attachments/assets/f4ed3e9b-ea73-49b7-9823-d2451d0094e6" />]: #
<img width="1470" height="956" alt="Screenshot 2025-08-11 at 8 00 23 AM" src="https://github.com/user-attachments/assets/8068d516-fa71-4226-a781-fdeaeda25de6" />

[<img width="1470" height="919" alt="dashboard-2" src="https://github.com/user-attachments/assets/48c362c3-7c44-4201-8463-dea2d7371aa5" />]: #
<img width="1470" height="956" alt="Screenshot 2025-08-11 at 8 00 45 AM" src="https://github.com/user-attachments/assets/d8cd58b2-2714-4b02-91e6-766fab7754ac" />

[<img width="717" height="321" alt="dashboard-3" src="https://github.com/user-attachments/assets/b352edea-a50d-48bc-ab2d-28cf5de157fd" />]: #
![Kapture 2025-08-11 at 08 04 30](https://github.com/user-attachments/assets/f7b129f9-4a30-4346-b97d-4f5f980e2f0b)


## Getting Started

If you want to set it up locally, follow the instructions below.

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

<a href="https://www.flaticon.com/free-icons/wheel-of-fortune" title="wheel of fortune icons">Wheel of fortune icon created by Freepik - Flaticon</a>
