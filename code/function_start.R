## mh comments

## why group by date here? 
## I think the issue here is that the code does not work for any date
## (we haven't yet got to the "wrapping in a function" part).

time_df <- camera_df2 |> group_by(date = mdy(Date)) |>
  interval(ymd("2022-07-01"), ymd("2022-09-01")) |>
  filter(choice == "DEERWHITETAILED") |>
  summarise(count_deer = n())


## anyway, the first goal is to create a filtered data set for a date range
## with __one__ particular date as a midpoint

test_date <- "2022-08-05"
camera_df2 |> 
  filter(Year >= ymd(test_date) - 30 &
           Year <= ymd(test_date) + 30) |>
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

get_interval_data(date_choice = "2021-07-02")
get_interval_data(date_choice = "2022-10-10")
