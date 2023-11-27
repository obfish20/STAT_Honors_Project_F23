## Function for single choice density
library(tidyverse)
library(overlap)
library(here)
library(lubridate)
library(purrr)
camera_df <- read_csv(here("data/camera_trap.csv"))

get_interval_data <- function(choice_choice, date_choice) {
  camera_df |> 
    mutate(time = mdy_hm(DTO), 
           frac_day = ((hour(time)*60 + minute(time))/60)/24,
           frac_week = (lubridate::wday(time, week_start = 1) - 1 + frac_day)/7, 
           frac_month = (day(time) - 1 + frac_day)/days_in_month(time),
           frac_year = (lubridate::yday(time))/365,
           time_3 = round(frac_day,3),
           week = round(frac_week,3),
           month = round(frac_month,3),
           year = round(frac_year, 3)) |> 
    mutate(timeRad = time_3*2*pi) |>
    filter(choice == choice_choice) |>
    filter(time >= ymd(date_choice) - 30 &
             time <= ymd(date_choice) + 30) 
}

## Kinda works: creates 91 rows. Where is the extra 30 coming from?
df_fun <- get_interval_data(choice_choice = 'DEERWHITETAILED', date_choice = "2022-08-28")

## Sequence from min date to max date
min_to_max <- seq(as.Date("2021-07-03"), as.Date("2024-08-28"), "day")




map(range_df, get_interval_data)



squirrel_g <- timeRad[df_fun$choice == 'SQUIRRELGRAY']
deer <- timeRad[df_fun$choice == 'DEERWHITETAILED']


densityPlot(deer, rug=TRUE)

overlapPlot(deer, squirrel_g)
legend('topright', c("DEERWHITETAILED", "SQUIRRELGRAY"), lty=c(1,2), col=c(1,4), bty='n')


gganimate