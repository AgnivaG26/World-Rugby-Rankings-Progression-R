if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, httr, lubridate, hrbrthemes, janitor, jsonlite, fredr,
               listviewer, usethis)
theme_set(hrbrthemes::theme_ipsum())
endpoint = "https://cmsapi.pulselive.com/rugby/rankings/mru?language=en&client=pulse"
rugby = fromJSON(endpoint)
str(rugby)
listviewer::jsonedit(rugby, mode = "view")
head(rugby$entries$team)
rankings =
  bind_cols(
    rugby$entries$team,
    rugby$entries %>% select(matches:previousPos)
  ) %>%
  clean_names() %>%
  select(-c(id, alt_id, annotations)) %>% 
  select(pos, pts, everything()) %>% 
  as_tibble() 
rankings
start_date = ymd("2004-01-01")
end_date = floor_date(today(), unit="years")
dates = seq(start_date, end_date, by="years")
dates = floor_date(dates, "week", week_start = getOption("lubridate.week.start", 1))
dates
rm(rugby, rankings, endpoint)
rugby_scrape =
  function(x) {
    endpoint = paste0("https://cmsapi.pulselive.com/rugby/rankings/mru?date=", x, "&client=pulse")
    rugby = fromJSON(endpoint)
    rankings =
      bind_cols(
        rugby$entries$team,
        rugby$entries %>% select(matches:previousPos)
      ) %>%
      clean_names() %>%
      mutate(date = x) %>% ## New column to keep track of the date
      select(-c(id, alt_id, annotations)) %>% ## These columns aren't adding much of interest
      select(date, pos, pts, everything()) %>% ## Reorder remaining columns
      as_tibble() ## "Enhanced" tidyverse version of a data frame
    Sys.sleep(3) ## Be nice!
    return(rankings)
  }
rankings_history =
  lapply(dates, rugby_scrape) %>% 
  bind_rows() 
rankings_history
teams = c("NZL", "RSA", "ENG", "JPN")
team_cols = c("NZL"="black", "RSA"="#4DAF4A", "ENG"="#377EB8", "JPN" = "red")
rankings_history %>%
  ggplot(aes(x=date, y=pts, group=abbreviation)) +
  geom_line(col = "grey") +
  geom_line(
    data = rankings_history %>% filter(abbreviation %in% teams),
    aes(col=fct_reorder2(abbreviation, date, pts)),
    lwd = 1
  ) +
  scale_color_manual(values = team_cols) +
  labs(
    x = "Date", y = "Points",
    title = "International rugby rankings", caption = "Source: World Rugby"
  ) +
  theme(legend.title = element_blank())
