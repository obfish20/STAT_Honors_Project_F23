## mh comments
## Load in package and data
library(tidyverse)
library(here)
library(lubridate)
camera_df <- read_csv(here("data/camera_trap.csv"))
## why group by date here? 
## I think the issue here is that the code does not work for any date
## (we haven't yet got to the "wrapping in a function" part).

camera_df2 <- camera_df |> 
  separate(col = DTO, into = c("Date", "Time"), sep = " ") |>
  mutate(Year = mdy(Date)) |>
  select(Year, everything())
camera_df2



  


## anyway, the first goal is to create a filtered data set for a date range
## with __one__ particular date as a midpoint

camera_df2 <- camera_df |> 
  separate(col = DTO, into = c("Date", "Time"), sep = " ") |>
  mutate(Year = mdy(Date)) |>
  select(Year, everything())

deer_stat <- camera_df2 |>
  filter(choice == "DEERWHITETAILED")

## after that is working, we want to wrap it in a function to return
## a data frame for any user-specified date:
## 
## my function argument is named date_choice and appears where 
## test_date appears in the code above

get_interval_data <- function(date_choice) {
  camera_df2 |> 
    filter(Year >= ymd(date_choice) - 30 &
             Year <= ymd(date_choice) + 30) |>
    filter(choice == "DEERWHITETAILED")
}

## now, I can test my function on a couple of dates:
## 

get_interval_data(date_choice = "2024-08-28")
get_interval_data(date_choice = "2021-07-03")

time_df <- get_interval_data(date_choice = "2022-10-10") |>
  group_by(date = mdy(Date)) |>
  summarise(count_deer = n())

## Function to create smooth dataframe

timemin <- camera_df2 |> arrange(Year)
## 2021-07-03
timemax <- camera_df2 |> arrange(desc(mdy(Date)))
## 2024-08-28



smooth_deer_df <- data.frame(lapply(camera_df2, get_interval_data))
