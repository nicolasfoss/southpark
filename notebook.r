
# Load libraries
library(dplyr)
library(readr)
library(tidytext)
library(sweary)

# Get wd

wd <- getwd()

# Load datasets
sp_lines <- read_csv("datasets/sp_lines.csv")
sp_ratings <- read_csv("datasets/sp_ratings.csv")

# Take a look at the last six observations
tail(sp_lines)
tail(sp_ratings)

# These packages need to be loaded in the first @tests cell. 
library(testthat) 
library(IRkernel.testthat)

run_tests({
    test_that("sp_lines was loaded correctly", {
        expect_equal(nrow(sp_lines), 78700, info = "`sp_lines doesn't have a correct number of rows.`")
        expect_equal(ncol(sp_lines), 5, info = "`sp_lines doesn't have a correct number of columns.")
        expect_is(sp_lines, "tbl_df")
    })
    
    test_that("sp_ratings was loaded correctly", {
        expect_equal(nrow(sp_ratings), 287, info = "`sp_ratings doesn't have a correct number of rows.")
        expect_equal(ncol(sp_ratings), 5, info = "`sp_ratings doesn't have a correct number of columns")
        expect_is(sp_ratings, "tbl_df")
    })
})

# Load english swear words
en_swear_words <- sweary::get_swearwords("en") %>%
    mutate(stem = SnowballC::wordStem(word))

# Load the AFINN lexicon
afinn  <- read_rds("datasets/afinn.rds")


# Join lines with episode ratings
sp <- sp_lines %>% 
inner_join(sp_ratings, by = c("season_number", "episode_number"))

# Unnest lines to words, leave out stop words and add a 
# swear_word logical column
sp_words <- sp %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    left_join(afinn) %>%
    mutate(word_stem = SnowballC::wordStem(word),
           swear_word = word %in% en_swear_words$word | word_stem %in% en_swear_words$stem)

# View the last six observations
tail(sp_words)

run_tests({
    test_that("sp_words was constructed correctly", {
        expect_equal(nrow(sp_words), 310821, info = "`sp_words` doesn't have a correct number of rows.")
        expect_equal(ncol(sp_words), 11, info = "`sp_words` doesn't have a correct number of columns.")
        expect_is(sp_words, "tbl_df")
    })
})

# Group by and summarize data by episode
by_episode <- sp_words %>%
    group_by(episode_name, rating, episode_order) %>% 
    summarize(
        swear_word_ratio = sum(swear_word) / n(),
        sentiment_score = mean(value, na.rm = TRUE)) %>%
    arrange(episode_order)

# Examine the last few rows of by_episode
tail(by_episode)

# What is the naughtiest episode?
( naughtiest <- by_episode[which.max(by_episode$swear_word_ratio), ] )

run_tests({
    test_that("by_episode was constructed correctly", {
        expect_equal(nrow(by_episode), 287, info = "`by_episode` doesn't have a correct number of rows.")
        expect_equal(ncol(by_episode), 5, info = "`by_episode` doesn't have a correect number of columns.")
        expect_is(by_episode, "tbl_df")
    })
    
    test_that("naughtiest episode is It Hits the Fan", {
        expect_equal(naughtiest$episode_name, "It Hits the Fan", info = "Did you use `which.max` on `by_episode$swear_word_ratio`?")
    })
})

# Load the ggplot2
library(ggplot2)

# Set a minimal theme for all future plots
theme_set(theme_minimal())

# Plot sentiment score for each episode
ggplot(by_episode, aes(episode_order, sentiment_score)) +
    geom_col() +
    geom_smooth(se = FALSE)

p <- last_plot()

run_tests({
    test_that("plot is ggplot", {
        expect_is(p, "ggplot", info = "Did you create the plot using ggplot2?")
    })
    
    test_that("plot aesthetics are correct", {
        expect_equal(deparse(p$mapping$x), "~episode_order", info = "Did you put `episode_order` on `x`?")
        expect_equal(deparse(p$mapping$y), "~sentiment_score", info = "Did you put `sentiment_score` on `y`?")
    })
    
    test_that("col and smooth layers are present", {
        expect_equal(class(p$layers[[1]]$geom)[1], "GeomCol", info = "Did you use `geom_col` to create the plot?")
        expect_equal(class(p$layers[[2]]$geom)[1], "GeomSmooth", info = "Did you add `geom_smooth` to see the trend?")
    })
    
    test_that("by_episode data is used in the plot", {
        expect_equal(by_episode, p$data %>% select(-.group), info = "Did you use `by_episode?`")
    })
})

# Plot episode ratings
ggplot(by_episode, aes(episode_order, rating)) +
    geom_point() +
    geom_smooth(se = FALSE) +
    geom_vline(xintercept = 100, linetype = "dashed", color = "red")

p <- last_plot()

run_tests({
    test_that("plot is ggplot", {
        expect_is(p, "ggplot", info = "Did you create the plot using ggplot2?")
    })
    
    test_that("plot aesthetics are correct", {
        expect_equal(deparse(p$mapping$x), "~episode_order", info = "Did you put `episode_order` on `x`?")
        expect_equal(deparse(p$mapping$y), "~rating", info = "Did you put `rating` on `y`?")
    })
    
    test_that("col, smooth and vline layers are present", {
        expect_equal(class(p$layers[[1]]$geom)[1], "GeomPoint", info = "Did you use `geom_point` to create the plot?")
        expect_equal(class(p$layers[[2]]$geom)[1], "GeomSmooth", info = "Did you add `geom_smooth` to see the trend?")
        expect_equal(class(p$layers[[3]]$geom)[1], "GeomVline", info = "Did you add geom_vline to see episode number 100?")
    })
    
    test_that("by_episode data is used in the plot", {
        expect_equal(by_episode, p$data %>% select(-.group), info = "Did you use `by_episode?`")
    })
    
    test_that("geom_vline style and data are correct", {
        aes <- cbind(p$layers[[3]]$aes_params, p$layers[[3]]$data)
        expect_true(aes$colour == "red", info = "Did you set `col` to red?")
        expect_true(aes$linetype == "dashed", info = "Did you set `lty` to dashed?")
        expect_true(aes$xintercept == 100, info = "Did you set `xintercept` to 100?")
    })
})

# Plot swear word ratio over episode rating
ggplot(by_episode, aes(rating, swear_word_ratio)) +
    geom_point(alpha = 0.6, size = 3) +
    geom_smooth(se = FALSE) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = seq(6, 10, 0.5)) +
    labs(
        x = "IMDB rating",
        y = "Episode swear word ratio"
)

p <- last_plot()

run_tests({
    test_that("plot is ggplot", {
        expect_is(p, "ggplot", info = "Did you create the plot using ggplot2?")
    })
    
    test_that("plot aesthetics are correct", {
        expect_equal(deparse(p$mapping$x), "~rating", info = "Did you put `rating` on `x`?")
        expect_equal(deparse(p$mapping$y), "~swear_word_ratio", info = "Did you put `swear_word_ratio` on `y`?")
    })
    
    test_that("point and smooth layers are present", {
        expect_equal(class(p$layers[[1]]$geom)[1], "GeomPoint", info = "Did you use `geom_point` to create the plot?")
        expect_equal(class(p$layers[[2]]$geom)[1], "GeomSmooth", info = "Did you add `geom_smooth` to see the trend?")
    })
    
    test_that("by_episode data is used in the plot", {
        expect_equal(by_episode, p$data %>% select(-.group), info = "Did you use `by_episode?`")
    })
    
    test_that("geom_point parameters are correct", {
        aes <- p$layers[[1]]$aes_params
        expect_true(aes$alpha == 0.6, info = "Did you set `alpha` to 0.6?")
        expect_true(aes$size == 3, info = "Did you set `size` to 3?")
    })
})

# Create a function that compares profanity of two characters
compare_profanity <- function(char1, char2, words) {
    char_1 <- filter(words, character == char1)
    char_2 <- filter(words, character == char2)
    char_1_summary <- summarise(char_1, swear = sum(swear_word), total = n() - sum(swear_word))
    char_2_summary <- summarise(char_2, swear = sum(swear_word), total = n() - sum(swear_word))
    char_both_summary <- bind_rows(char_1_summary, char_2_summary)
    result <- prop.test(as.matrix(char_both_summary), correct = FALSE)
    return(broom::tidy(result) %>% bind_cols(character = char1))
}

run_tests({
    test_that("compary_profanity works", {
        cartman_cartman <- compare_profanity("cartman", "cartman", sp_words)
        cartman_stan <- compare_profanity("cartman", "stan", sp_words)
        
        expect_equal(as.numeric(cartman_cartman$statistic), 0, info = "Function doesn't work for two same characters (It should have a statistic = 0).")
        expect_true(cartman_stan$statistic > 0, info = "Function doesn't work - does it return a data frame?")
        expect_equal(nrow(cartman_stan), 1, info = "It should only return 1 row.")
        expect_equal(ncol(cartman_stan), 10, info = "It should return 10 columns.")
    })
})

# Vector of most speaking characters in the show
characters <- c("butters", "cartman", "kenny", "kyle", "randy", "stan", "gerald", "mr. garrison",
                "mr. mackey", "wendy", "chef", "jimbo", "jimmy", "sharon", "sheila", "stephen")

# Map compare_profanity to all characters against Cartman
prop_result <- purrr::map_df(characters, compare_profanity, "cartman", sp_words)

# Plot estimate1-estimate2 confidence intervals of all characters and color it by a p.value threshold
ggplot(prop_result, aes(x = reorder(character, -estimate1), y = estimate1-estimate2, color = p.value < 0.05)) +
    geom_point() + 
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE) +
    geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))

p <- last_plot()

run_tests({
    test_that("prop_result is correct", {
        expect_equal(nrow(prop_result), 16, info = "`prop_result` doesn't have 16 rows - did you use the whole `characters` vector?")
        expect_equal(ncol(prop_result), 10, info = "Does compare_profanity work? It doesn't have 10 columns.")
    })
    
    test_that("plot is ggplot", {
        expect_is(p, "ggplot", info = "Did you create the plot using ggplot2?")
    })
    
    test_that("plot aesthetics are correct", {
        expect_equal(deparse(p$mapping$x), "~reorder(character, -estimate1)", info = "Did you put `character` reordered decreasing by `estimate1` on `x`?")
        expect_equal(deparse(p$mapping$y), "~estimate1 - estimate2", info = "Did you put `estimate1` on `y`?")
        expect_true(deparse(p$mapping$colour) == "~p.value < 0.05", info = "Did you set `color` to `p.value < 0.05`")
    })

    test_that("point, errorbar and hline layers are present", {
        expect_equal(class(p$layers[[1]]$geom)[1], "GeomPoint", info = "Did you use `geom_point` to create the plot?")
        expect_equal(class(p$layers[[2]]$geom)[1], "GeomErrorbar", info = "Did you use `geom_errorbar` to add error bars?")
        expect_equal(class(p$layers[[3]]$geom)[1], "GeomHline", info = "Did you add `geom_hline` to see the trend?")
    })
    
    test_that("by_episode data is used in the plot", {
        expect_equal(prop_result, p$data, info = "Did you use `prop_result?`")
    })
    
    test_that("errorbar mappings are correct", {
        m <- p$layers[[2]]$mapping
        expect_true(deparse(m$ymin) == "~conf.low", info = "Did you set `ymin` to `conf.low`?")
        expect_true(deparse(m$ymax) == "~conf.high", info = "Did you set `ymax` to `conf.high`?")
    })
})

# Are naughty episodes more popular? TRUE/FALSE
naughty_episodes_more_popular <- FALSE

# Is Eric Cartman the naughtiest character? TRUE/FALSE
eric_cartman_naughtiest <- FALSE

# If he is, assign an empty string, otherwise write his name
who_is_naughtiest <- "kenny"

run_tests({
    test_that("the answers are correct", {
        expect_equal(naughty_episodes_more_popular, FALSE, info = "Are you sure that naughty episodes are more popular?")
        expect_equal(eric_cartman_naughtiest, FALSE, info = "Are you sure that Eric Cartman is the naughtiest character?")
        expect_equal(who_is_naughtiest, "kenny", info = "Are you sure that the character you picked is the naughtiest character?")
    })
})
