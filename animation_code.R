library(tidyverse)
library(overlap)
library(here)
library(lubridate)

camera_df <- read_csv(here("data/camera_trap.csv"))


get_interval_data <- function(date_choice, animal = "DEERWHITETAILED",
                              plus_minus = 30) {
  interval_df <- camera_df |> 
    mutate(time = mdy_hm(DTO), 
           frac_day = ((hour(time) * 60 + minute(time)) / 60) / 24,
           frac_week = (lubridate::wday(time, week_start = 1) - 1 + frac_day) / 7, 
           frac_month = (day(time) - 1 + frac_day) / days_in_month(time),
           time_of_day = round(frac_day, 3),
           week = round(frac_week, 3),
           month = round(frac_month, 3)) |>
    mutate(day_of_year = yday(time)) |>
    filter(day_of_year >= yday(ymd(date_choice)) - plus_minus &
             day_of_year <= yday(ymd(date_choice)) + plus_minus) |>
    filter(choice == animal) |>
    mutate(timeRad = time_of_day * 2 * pi) |>
    filter(year(time) != 2024) |> ## filter out 2024 data
    select(time, day_of_year, everything())
  
  plot_obj <- densityPlot(interval_df$timeRad, rug = TRUE) |>
    mutate(date_choice = date_choice,
           n_obs = nrow(interval_df))
  
  rug_obj <- interval_df |> select(time) |>
    mutate(date_choice = date_choice)
  
  return(list(plot_obj, rug_obj))
}

date_vec <- seq(ymd('2010-07-15'), ymd('2010-11-15'), by = "days")

get_interval_data(date_choice = "2022-08-28", animal = "DEERWHITETAILED",
                  plus_minus = 15)

map_out <- map(date_vec, get_interval_data,
               animal = "DEERWHITETAILED",
               plus_minus = 15)
map_comb <- pmap(map_out, bind_rows, .id = "id")

full_df <- map_comb[[1]] |> as_tibble() |>
  mutate(id = as.numeric(id)) |>
  filter(x >= -0.1 & x <= 24.1)

full_rug_df <- map_comb[[2]] |> as_tibble() |>
  mutate(id = as.numeric(id)) |>
  mutate(frac_day = ((hour(time) * 60 + minute(time)) / 60) / 24,
         time_Rad = frac_day * 24)


anim_base <- ggplot(data = full_df, aes(x = x, y = y, group = id)) +
  geom_line(colour = "blue", linewidth = 2) +
  geom_rug(data = full_rug_df, aes(x = time_Rad, y = NULL,
                                   group = NULL)) +
  theme_minimal(base_size = 18) +
  labs(x = "Time of Day") +
  ylim(c(0, NA))

library(gganimate)
library(transformr)
anim_base + transition_time(date_choice) +
  labs(title = "{frame_time}") +
  shadow_mark(colour = "black", linewidth = 0.25, alpha = 0.15,
              exclude_layer = 2)




