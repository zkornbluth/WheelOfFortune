# Unit tests for Wheel of Fortune app logic
# Functions under test: integer_breaks, getCatsInYearRange, getLetterFreq,
#   getCategorizedWheelDataYearFilter (and dependency on wheel_data)

test_that("integer_breaks returns a function that produces integer breaks", {
  f <- integer_breaks(n = 5)
  expect_type(f, "closure")

  x <- c(2001, 2005, 2010, 2015, 2025)
  breaks <- f(x)
  expect_true(all(breaks == floor(breaks)))
  expect_true(min(breaks) <= min(x))
  expect_true(max(breaks) >= max(x))
})

test_that("integer_breaks handles single year", {
  f <- integer_breaks(n = 3)
  breaks <- f(2020)
  expect_true(all(breaks == floor(breaks)))
})

test_that("getCatsInYearRange returns character vector of categories", {
  cats <- getCatsInYearRange(2001, 2025)
  expect_type(cats, "character")
  expect_true(length(cats) >= 1)
  expect_false(any(is.na(cats)))
})

test_that("getCatsInYearRange respects year range", {
  cats_full <- getCatsInYearRange(2001, 2025)
  cats_sub  <- getCatsInYearRange(2020, 2025)
  expect_true(length(cats_full) >= length(cats_sub))
  expect_true(all(cats_sub %in% cats_full))
})

test_that("getLetterFreq returns tibble with expected structure", {
  out <- getLetterFreq(2001, 2025)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("letter", "appearance_rate", "is_vowel"))
  expect_equal(nrow(out), 20L)  # guessable letters excluding R,S,T,L,N,E
})

test_that("getLetterFreq letters are guessable set (no R,S,T,L,N,E)", {
  out <- getLetterFreq(2001, 2025)
  forbidden <- c("R", "S", "T", "L", "N", "E")
  expect_false(any(out$letter %in% forbidden))
})

test_that("getLetterFreq marks vowels correctly", {
  out <- getLetterFreq(2001, 2025)
  vowels <- c("A", "I", "O", "U")
  expect_true(all(out$letter[out$is_vowel] %in% vowels))
  expect_false(any(out$letter[!out$is_vowel] %in% vowels))
})

test_that("getLetterFreq appearance_rate is in [0, 1]", {
  out <- getLetterFreq(2001, 2025)
  expect_true(all(out$appearance_rate >= 0 & out$appearance_rate <= 1))
})

test_that("getLetterFreq with category filter returns same structure", {
  cats <- getCatsInYearRange(2001, 2025)
  skip_if(length(cats) == 0, "No categories in data")
  out <- getLetterFreq(2001, 2025, category = cats[1])
  expect_named(out, c("letter", "appearance_rate", "is_vowel"))
  expect_equal(nrow(out), 20L)
})

test_that("getCategorizedWheelDataYearFilter returns tibble with expected columns", {
  out <- getCategorizedWheelDataYearFilter(2001, 2025)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("category", "win_pct", "puzzle_length", "pct_letters_revealed"))
})

test_that("getCategorizedWheelDataYearFilter has no NA in filtered result", {
  out <- getCategorizedWheelDataYearFilter(2001, 2025)
  expect_false(any(is.na(out$win_pct)))
  expect_false(any(is.na(out$puzzle_length)))
  expect_false(any(is.na(out$pct_letters_revealed)))
})

test_that("getCategorizedWheelDataYearFilter win_pct in [0, 1]", {
  out <- getCategorizedWheelDataYearFilter(2001, 2025)
  expect_true(all(out$win_pct >= 0 & out$win_pct <= 1))
})

test_that("getCategorizedWheelDataYearFilter puzzle_length and pct_letters_revealed non-negative", {
  out <- getCategorizedWheelDataYearFilter(2001, 2025)
  expect_true(all(out$puzzle_length >= 0))
  expect_true(all(out$pct_letters_revealed >= 0 & out$pct_letters_revealed <= 1))
})

test_that("getCategorizedWheelDataYearFilter narrow year range returns subset or empty", {
  out_full <- getCategorizedWheelDataYearFilter(2001, 2025)
  out_narrow <- getCategorizedWheelDataYearFilter(2024, 2025)
  expect_true(nrow(out_narrow) <= nrow(out_full))
})
