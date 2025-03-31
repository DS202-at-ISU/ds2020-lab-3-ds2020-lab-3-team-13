
<!-- README.md is generated from README.Rmd. Please edit the README.Rmd file -->

# Lab report \#3 - instructions

Follow the instructions posted at
<https://ds202-at-isu.github.io/labs.html> for the lab assignment. The
work is meant to be finished during the lab time, but you have time
until Monday evening to polish things.

Include your answers in this document (Rmd file). Make sure that it
knits properly (into the md file). Upload both the Rmd and the md file
to your repository.

All submissions to the github repo will be automatically uploaded for
grading once the due date is passed. Submit a link to your repository on
Canvas (only one submission per team) to signal to the instructors that
you are done with your submission.

# Lab 3: Avenger’s Peril

## As a team

Extract from the data below two data sets in long form `deaths` and
`returns`

``` r
av <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/avengers/avengers.csv", stringsAsFactors = FALSE)
head(av)
```

    ##                                                       URL
    ## 1           http://marvel.wikia.com/Henry_Pym_(Earth-616)
    ## 2      http://marvel.wikia.com/Janet_van_Dyne_(Earth-616)
    ## 3       http://marvel.wikia.com/Anthony_Stark_(Earth-616)
    ## 4 http://marvel.wikia.com/Robert_Bruce_Banner_(Earth-616)
    ## 5        http://marvel.wikia.com/Thor_Odinson_(Earth-616)
    ## 6       http://marvel.wikia.com/Richard_Jones_(Earth-616)
    ##                    Name.Alias Appearances Current. Gender Probationary.Introl
    ## 1   Henry Jonathan "Hank" Pym        1269      YES   MALE                    
    ## 2              Janet van Dyne        1165      YES FEMALE                    
    ## 3 Anthony Edward "Tony" Stark        3068      YES   MALE                    
    ## 4         Robert Bruce Banner        2089      YES   MALE                    
    ## 5                Thor Odinson        2402      YES   MALE                    
    ## 6      Richard Milhouse Jones         612      YES   MALE                    
    ##   Full.Reserve.Avengers.Intro Year Years.since.joining Honorary Death1 Return1
    ## 1                      Sep-63 1963                  52     Full    YES      NO
    ## 2                      Sep-63 1963                  52     Full    YES     YES
    ## 3                      Sep-63 1963                  52     Full    YES     YES
    ## 4                      Sep-63 1963                  52     Full    YES     YES
    ## 5                      Sep-63 1963                  52     Full    YES     YES
    ## 6                      Sep-63 1963                  52 Honorary     NO        
    ##   Death2 Return2 Death3 Return3 Death4 Return4 Death5 Return5
    ## 1                                                            
    ## 2                                                            
    ## 3                                                            
    ## 4                                                            
    ## 5    YES      NO                                             
    ## 6                                                            
    ##                                                                                                                                                                              Notes
    ## 1                                                                                                                Merged with Ultron in Rage of Ultron Vol. 1. A funeral was held. 
    ## 2                                                                                                  Dies in Secret Invasion V1:I8. Actually was sent tto Microverse later recovered
    ## 3 Death: "Later while under the influence of Immortus Stark committed a number of horrible acts and was killed.'  This set up young Tony. Franklin Richards later brought him back
    ## 4                                                                               Dies in Ghosts of the Future arc. However "he had actually used a hidden Pantheon base to survive"
    ## 5                                                      Dies in Fear Itself brought back because that's kind of the whole point. Second death in Time Runs Out has not yet returned
    ## 6                                                                                                                                                                             <NA>

Get the data into a format where the five columns for Death\[1-5\] are
replaced by two columns: Time, and Death. Time should be a number
between 1 and 5 (look into the function `parse_number`); Death is a
categorical variables with values “yes”, “no” and ““. Call the resulting
data set `deaths`.

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.4.3

    ## Warning: package 'ggplot2' was built under R version 4.4.3

    ## Warning: package 'tidyr' was built under R version 4.4.3

    ## Warning: package 'purrr' was built under R version 4.4.3

    ## Warning: package 'dplyr' was built under R version 4.4.3

    ## Warning: package 'forcats' was built under R version 4.4.3

    ## Warning: package 'lubridate' was built under R version 4.4.3

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.4     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
deaths <- av %>% 
  pivot_longer(
    starts_with("Death"),
    names_to = "Time",
    values_to = "Died"
  ) %>% 
  select(
    URL, Name.Alias, Time, Died
  )

maxdeaths <- deaths %>% 
  mutate(
    Time = parse_number(Time)
  ) %>% 
  group_by(URL, Died) %>% 
  summarise(
    total_death = max(Time)
  ) %>% 
  filter(Died != "")
```

    ## `summarise()` has grouped output by 'URL'. You can override using the `.groups`
    ## argument.

Similarly, deal with the returns of characters.

``` r
returns <- av %>% 
  pivot_longer(
    starts_with("Return"),
    names_to = "Time",
    values_to = "Returned"
  ) %>% 
  select(
    URL, Name.Alias, Time, Returned
  )

View(returns)
View(deaths)
View(av)
```

Based on these datasets calculate the average number of deaths an
Avenger suffers.

``` r
avenger_deaths <- deaths %>%
  mutate(Died = ifelse(Died == "YES", 1, 0))

View(avenger_deaths)

avenger_returns <- returns %>%
  mutate(Returned = ifelse(Returned == "YES", 1, 0))

avenger_summary <- avenger_deaths %>%
  left_join(avenger_returns, by = c("URL", "Name.Alias", "Time")) %>%
  group_by(Name.Alias) %>%
  summarise(total_deaths = sum(Died, na.rm = TRUE))

mean(avenger_summary$total_deaths)
```

    ## [1] 0.5460123

## Izzy Grabowski

For each team member, copy this part of the report.

Each team member picks one of the statements in the FiveThirtyEight
[analysis](https://fivethirtyeight.com/features/avengers-death-comics-age-of-ultron/)
and fact checks it based on the data. Use dplyr functionality whenever
possible.

### FiveThirtyEight Statement

> My statement: Out of 173 listed Avengers, my analysis found that 69
> had died at least one time after they joined the team.

### Include the code

``` r
# FACT CHECKING
library(dplyr)
library(tidyverse)


avenger_deaths <- deaths %>%
  mutate(Died = ifelse(Died == "YES", 1, 0))

total_avengers <- avenger_deaths %>%
  summarise(total = n_distinct(Name.Alias)) # organize by each avenger

avengers_who_died <- avenger_deaths %>%
  filter(Died == 1) %>%
  summarise(Died_count = n_distinct(Name.Alias)) # counting number of avengers that died

print(total_avengers)
```

    ## # A tibble: 1 × 1
    ##   total
    ##   <int>
    ## 1   163

``` r
print(avengers_who_died)
```

    ## # A tibble: 1 × 1
    ##   Died_count
    ##        <int>
    ## 1         64

Make sure to include the code to derive the (numeric) fact for the
statement

### Include your answer

Include at least one sentence discussing the result of your
fact-checking endeavor.

My Answer: I found that there was a total of 163 avengers, and after
checking the data there are 10 missing entries for the Names.ALias
column. I also found that 64 avengers died at least once, rather than
69. So, the fact is not true.

Upload your changes to the repository. Discuss and refine answers as a
team.

## Ethan Rollinger

For each team member, copy this part of the report.

Each team member picks one of the statements in the FiveThirtyEight
[analysis](https://fivethirtyeight.com/features/avengers-death-comics-age-of-ultron/)
and fact checks it based on the data. Use dplyr functionality whenever
possible.

### FiveThirtyEight Statement

> The Statement that I am fact-checking is: “There’s a 2-in-3 chance
> that a member of the Avengers returned from their first stint in the
> afterlife”

### Include the code

``` r
#filtering the data sets to only look at the first deaths and returns and turn each Death and Return numerically
first_death <- deaths %>% filter(Time == "Death1") %>% mutate(Died = ifelse(Died == "YES", 1, 0))

first_return <- returns %>% filter(Time == "Return1") %>% mutate(Returned = ifelse(Returned == "YES", 1, 0))

#Combining the data sets together
return_rate <- first_death %>% left_join(first_return, by = c("URL", "Name.Alias")) %>% filter(Died == 1)

#Finding the mean of the return rate after the first death
mean(return_rate$Returned, na.rm = TRUE)
```

    ## [1] 0.6666667

### Include your answer

Based on the above code, the fact check that I did on “There’s a 2-in-3
chance that a member of the Avengers returned from their first stint in
the afterlife” is correct as the data produces a 0.666667, which is 2/3.

#### Benjamin Herschel Statement

> “But you can only tempt death so many times. There’s a 2-in-3 chance
> that a member of the Avengers returned from their first stint in the
> afterlife, **but only a 50 percent chance they recovered from a second
> or third death.**”

#### Benjamin Herschel’s Code

``` r
deaths <- av %>% 
  pivot_longer(
    starts_with("Death"),
    names_to = "Time",
    values_to = "Died"
  ) %>% 
  select(
    URL, Name.Alias, Time, Died
  ) %>%
  mutate(Time = parse_number(Time),
         Died_numeric = ifelse(Died == "YES", 1, 0))

returns <- av %>% 
  pivot_longer(
    starts_with("Return"),
    names_to = "Time",
    values_to = "Returned"
  ) %>% 
  select(
    URL, Name.Alias, Time, Returned
  ) %>%
  mutate(Time = parse_number(Time),
         Returned_numeric = ifelse(Returned == "YES", 1, 0))

second_third_deaths <- deaths %>%
  filter(Time %in% c(2, 3))

recovery_data <- second_third_deaths %>%
  left_join(returns, by = c("URL", "Name.Alias", "Time"))

recovery_rate <- recovery_data %>%
  filter(Died_numeric == 1) %>%
  summarise(return_percentage = mean(Returned_numeric, na.rm = TRUE) * 100)

if(nrow(recovery_rate) > 0 && is.finite(recovery_rate$return_percentage)){
  print(paste("Recovery percentage after second/third death:", round(recovery_rate$return_percentage, 2), "%"))
  if (abs(recovery_rate$return_percentage - 50) < 5) {
    print("The 50% statement is correct.")
  } else {
    print("The 50% statement is incorrect.")
  }
} 
```

    ## [1] "Recovery percentage after second/third death: 50 %"
    ## [1] "The 50% statement is correct."

## Individually

For each team member, copy this part of the report.

Each team member picks one of the statements in the FiveThirtyEight
[analysis](https://fivethirtyeight.com/features/avengers-death-comics-age-of-ultron/)
and fact checks it based on the data. Use dplyr functionality whenever
possible.
=======
## Christopher Moseley
>>>>>>> c66ab6f66de0352685a515c3b6eb6084297364da

### FiveThirtyEight Statement

> The Statement I’m fact checking is: “Given the Avengers’ 53 years in
> operation and overall mortality rate, fans of the comics can expect
> one current or former member to die every seven months or so.”

### Include the code

Make sure to include the code to derive the (numeric) fact for the
statement

``` r
#View(av)

DeathColumns <- c("Death1", "Death2", "Death3", "Death4", "Death5")

yesCount <- sum(av[, DeathColumns] == "YES")

avTimeBetweenDeaths <- (53 / yesCount) * 12
```

### Include your answer

For the statement I did, “Given the Avengers’ 53 years in operation and
overall mortality rate, fans of the comics can expect one current or
former member to die every seven months or so”, I found that it was
accurate at 7.146 months on average. So it wasn’t exactly 7 months but
it said or so, and I’d consider the .146 extra to consider the statement
accurate.
