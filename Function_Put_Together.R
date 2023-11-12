## I am currently struggleing with keeping the date_choice (aka, how to apply the function)
## I am not sure how the change of data sets camera_df in stead of camera_df2 is causing such and issue
library(tidyverse)
library(overlap)
library(here)
library(lubridate)
camera_df <- read_csv(here("data/camera_trap.csv"))

get_interval_data <- function(date_choice) {
  camera_df |> 
    mutate(time = mdy_hm(DTO), 
                  frac_day = ((hour(time)*60 + minute(time))/60)/24,
                  frac_week = (lubridate::wday(time, week_start = 1) - 1 + frac_day)/7, 
                  frac_month = (day(time) - 1 + frac_day)/days_in_month(time),
                  time = round(frac_day,3),
                  week = round(frac_week,3),
                  month = round(frac_month,3),) |>
    ## This section (Dr. Higham)
    filter(time >= ymd(date_choice) - 30 &
             time <= ymd(date_choice) + 30) |>
    mutate(timeRad = time * 2 * pi)
}


get_interval_data(date_choice = "2023-08-28")
## Why is it returning no rows?

## Fitting kernel density (Not sure where an how to pace this)
squirrel_g <- timeRad[camera_df4$choice == 'SQUIRRELGRAY']
deer <- timeRad[camera_df4$choice == 'DEERWHITETAILED']


## for if I get it
densityPlot(deer, rug=TRUE)

overlapPlot(deer, coyote)
legend('topright', c("DEERWHITETAILED", "SQUIRRELGRAY"), lty=c(1,2), col=c(1,4), bty='n')

