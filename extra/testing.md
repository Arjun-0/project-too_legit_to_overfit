tester
================

``` r
library(tidyverse)
library(broom)
library(pander)
library(here)
library(tidymodels)
```

## R Markdown

``` r
summary(board_games)
```

    ##     game_id       description           image            max_players     
    ##  Min.   :     1   Length:10532       Length:10532       Min.   :  0.000  
    ##  1st Qu.:  5444   Class :character   Class :character   1st Qu.:  4.000  
    ##  Median : 28822   Mode  :character   Mode  :character   Median :  4.000  
    ##  Mean   : 62059                                         Mean   :  5.657  
    ##  3rd Qu.:126410                                         3rd Qu.:  6.000  
    ##  Max.   :216725                                         Max.   :999.000  
    ##   max_playtime         min_age        min_players     min_playtime     
    ##  Min.   :    0.00   Min.   : 0.000   Min.   :0.000   Min.   :    0.00  
    ##  1st Qu.:   30.00   1st Qu.: 8.000   1st Qu.:2.000   1st Qu.:   25.00  
    ##  Median :   45.00   Median :10.000   Median :2.000   Median :   45.00  
    ##  Mean   :   91.34   Mean   : 9.715   Mean   :2.071   Mean   :   80.88  
    ##  3rd Qu.:   90.00   3rd Qu.:12.000   3rd Qu.:2.000   3rd Qu.:   90.00  
    ##  Max.   :60000.00   Max.   :42.000   Max.   :9.000   Max.   :60000.00  
    ##      name            playing_time       thumbnail         year_published
    ##  Length:10532       Min.   :    0.00   Length:10532       Min.   :1950  
    ##  Class :character   1st Qu.:   30.00   Class :character   1st Qu.:1998  
    ##  Mode  :character   Median :   45.00   Mode  :character   Median :2007  
    ##                     Mean   :   91.34                      Mean   :2003  
    ##                     3rd Qu.:   90.00                      3rd Qu.:2012  
    ##                     Max.   :60000.00                      Max.   :2016  
    ##     artist            category         compilation          designer        
    ##  Length:10532       Length:10532       Length:10532       Length:10532      
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##   expansion            family            mechanic          publisher        
    ##  Length:10532       Length:10532       Length:10532       Length:10532      
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  average_rating   users_rated     
    ##  Min.   :1.384   Min.   :   50.0  
    ##  1st Qu.:5.830   1st Qu.:   85.0  
    ##  Median :6.393   Median :  176.0  
    ##  Mean   :6.371   Mean   :  870.1  
    ##  3rd Qu.:6.943   3rd Qu.:  518.0  
    ##  Max.   :9.004   Max.   :67655.0

``` r
board_games <- board_games %>%
  filter(playing_time != 0)

set.seed(1116)

games_split <- initial_split(board_games, prop = 0.8)
train_data <- training(games_split)
test_data <- testing(games_split) 
```

## Including Plots

``` r
games_lm_mod <- linear_reg() %>%
  set_engine("lm")

games_play_recipe <- recipe(average_rating ~ playing_time, data = train_data) %>%
  step_filter(playing_time != 0) %>%
  step_log(all_predictors()) 

games_play_workflow <- workflow() %>%
  add_model(games_lm_mod) %>%
  add_recipe(games_play_recipe)

games_play_all_recipe <- recipe(
  average_rating ~ playing_time + min_playtime + max_playtime,
  data = train_data
  ) %>%
  step_filter(playing_time != 0) %>%
  step_log(all_predictors())

games_play_all_workflow <- workflow() %>%
  add_model(games_lm_mod) %>%
  add_recipe(games_play_all_recipe)  

games_play_fit <- games_play_workflow %>% 
  fit(data = train_data)
```

positive playtime coefficient implies that as playtime increases average
rating is expected to increase.

``` r
games_play_pred <- predict(games_play_fit, test_data) %>%
  bind_cols(test_data)

games_play_pred %>%
  ggplot(aes(x = playing_time)) +
  geom_point(aes(y = average_rating)) +
  geom_line(aes(y = .pred), colour = "red") +
  scale_x_log10() +
  labs(x = "Average Playing Time", y = "Average Rating")
```

![](testing_files/figure-gfm/predict-1.png)<!-- -->

``` r
board_games %>%
  ggplot(aes(playing_time, average_rating)) +
  geom_point(alpha = 0.6)
```

![](testing_files/figure-gfm/plot-1.png)<!-- -->

``` r
games_play_pred %>%
  filter(playing_time < 1)
```

    ## # A tibble: 0 x 23
    ## # … with 23 variables: .pred <dbl>, game_id <dbl>, description <chr>,
    ## #   image <chr>, max_players <dbl>, max_playtime <dbl>, min_age <dbl>,
    ## #   min_players <dbl>, min_playtime <dbl>, name <chr>, playing_time <dbl>,
    ## #   thumbnail <chr>, year_published <dbl>, artist <chr>, category <chr>,
    ## #   compilation <chr>, designer <chr>, expansion <chr>, family <chr>,
    ## #   mechanic <chr>, publisher <chr>, average_rating <dbl>, users_rated <dbl>

``` r
board_games %>%
  filter(playing_time > 40000) %>%
  select(name)
```

    ## # A tibble: 1 x 1
    ##   name                         
    ##   <chr>                        
    ## 1 The Campaign for North Africa

``` r
board_games %>%
  filter(playing_time != 0) %>%
  ggplot(aes(playing_time, average_rating)) +
  geom_point(alpha = 0.6) +
  scale_x_log10()
```

![](testing_files/figure-gfm/log_scale-1.png)<!-- -->

``` r
no_playtime <- board_games %>%
  filter(playing_time == 0) %>%
  arrange(desc(average_rating))
```
