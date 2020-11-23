tester
================

``` r
library(tidyverse)
library(broom)
library(pander)
library(here)
library(rsample)
library(parsnip)
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
set.seed(1116)

games_split <- initial_split(board_games, prop = 0.8)
train_data <- training(games_split)
test_data <- testing(games_split)
```

## Including Plots

``` r
games_lm_mod <- linear_reg() %>%
  set_engine("lm")

games_play_lm_fit <- games_lm_mod %>%
  fit(average_rating ~ playing_time, data = train_data)

tidy(games_play_lm_fit)
```

    ## # A tibble: 2 x 5
    ##   term          estimate std.error statistic    p.value
    ##   <chr>            <dbl>     <dbl>     <dbl>      <dbl>
    ## 1 (Intercept)  6.37      0.00933      682.   0         
    ## 2 playing_time 0.0000590 0.0000127      4.66 0.00000319

``` r
games_play_all_fit <- games_lm_mod %>%
  fit(average_rating ~ playing_time + min_playtime + max_playtime, data = board_games)

tidy(games_play_all_fit)
```

    ## # A tibble: 4 x 5
    ##   term          estimate  std.error statistic   p.value
    ##   <chr>            <dbl>      <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)   6.36      0.00833      763.    0.      
    ## 2 playing_time  0.000519  0.0000566      9.17  5.82e-20
    ## 3 min_playtime -0.000473  0.0000586     -8.08  7.12e-16
    ## 4 max_playtime NA        NA             NA    NA

positive playtime coefficient implies that as playtime increases average
rating is expected to increase.

``` r
board_games %>%
  ggplot(aes(playing_time, average_rating)) +
  geom_point(alpha = 0.6)
```

![](testing_files/figure-gfm/plot-1.png)<!-- -->

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
