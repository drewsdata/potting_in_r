library(tidyverse)
library(lubridate)
library(janitor)
library(scales)
library(plotly)
library(timetk)
library(forecast)

###
# Using the GSuite / Google Workplace GAMADV-XTD GAM CLI
# and R to build various data pipelines and
# plots showing daily, weekly and monthly
# changes and cumulative provisioning and deprovisioning events
# and changes in license availabilty
# Data is aggrated and plots are pre-built and saved quick
# loading and rendering in Shiny 

# base_log started with initial six month gsuite log license assignment events
# 30 day log license assignment events gam script runs daily:
# "C:\GAMADV-XTD3\gam.exe" report admin event "USER_LICENSE_ASSIGNMENT" 
#   start -30d > "C:\scripts\license_assignment_30d.csv"

base_log <- read_csv("c:/scripts/license_assignment_base.csv", 
                     col_types = cols(.default = "c"))

last_30_log  <- read_csv("c:/scripts/license_assignment_30d.csv", 
                         col_types = cols(.default = "c"))

admin_license_log <- bind_rows(base_log, last_30_log) %>% 
  distinct(id.uniqueQualifier, .keep_all = TRUE) 

#### UNCOMMENT THIS IN PRODUCTION !!!!  #####
write_csv(admin_license_log, "c:/scripts/license_assignment_base.csv")

admin_license_log$id.time <- admin_license_log$id.time %>%
  strftime(format = "%Y-%m-%d") %>% ymd()

# some days return no license assignment events so no log entry for dates
# need to fix with zeroes for later plotting
all_dates  <- as_tibble(tidyr::full_seq(admin_license_log$id.time,1))

admin_license_log <- admin_license_log %>%  
  distinct(USER_EMAIL, .keep_all = TRUE) %>% 
  filter(id.time >= "2020-09-01") %>% 
  select(NEW_VALUE, USER_EMAIL, id.time) %>%
  mutate(
    license_type = case_when(
      NEW_VALUE == "G Suite Business - Archived User" ~ "archive",
      NEW_VALUE == "G Suite Business" ~ "gsuite")) %>% 
  clean_names()

admin_license_log <- left_join(all_dates, admin_license_log, by = c("value" = "id_time"))

net__gsuite_license_log <- admin_license_log %>% 
  group_by(day_date = value) %>%
  count(license_type) %>% 
  pivot_wider(names_from = license_type, values_from = n) %>%
  rename(no_event = 'NA') %>%
  replace_na(list(archive = 0, gsuite = 0)) %>%
  mutate(net_change = gsuite - archive) %>% 
  select(-no_event) %>% 
  filter(day_date >= "2020-09-01")

net__gsuite_license_log$cumulative <- cumsum(net__gsuite_license_log$net_change)
net__gsuite_license_log$past_days <- as.integer(rownames(net__gsuite_license_log))
net__gsuite_license_log$past_days <- order(-net__gsuite_license_log$past_days)

net__gsuite_license_weeks <- net__gsuite_license_log %>%
  summarise_by_time(
    .date_var = day_date,
    .by       = "week",
    net_change  = sum(net_change),
    cumulative = last(cumulative)
  )

net__gsuite_license_months <- net__gsuite_license_log %>%
  summarise_by_time(
    .date_var = day_date,
    .by       = "month", # Setup for monthly aggregation
    net_change  = sum(net_change),     # Summarization
    cumulative = last(cumulative)
  )


### plots

# daily
daily_fig <- plot_ly(net__gsuite_license_log)%>%
  add_trace(x = ~day_date, 
            y = ~net_change, 
            type = 'bar', 
            name = ~ifelse(net_change < 0, "Decrease", "Increase"),
            color = ~net_change < 0, 
            colors = c("lawngreen", "red")) %>%
  add_trace(x = ~day_date, y = ~cumulative, 
            type = 'scatter', 
            mode = 'line',
            yaxis = 'y2',
            line = list(color = 'dodgerblue',
                        width = 4),
            name = "Cumulative") %>% 
  layout(title = list(text = ' GSuite Daily License Change',
                      y = 0.95, font = list(size = 22))) %>% 
  
  layout(xaxis = list(title = "",
                      tickfont = list(size = 16))) %>% 
  
  layout(yaxis = list(side = 'left', 
                      title = 'Net Daily', 
                      showgrid = FALSE, 
                      zeroline = TRUE,
                      titlefont = list(size = 16), 
                      tickfont = list(size = 16))) %>% 
  
  layout(yaxis2 = list(side = 'right',
                       automargin = TRUE,
                       overlaying = "y",
                       showticklabels = TRUE,
                       title = 'Cumulative', 
                       showgrid = FALSE, 
                       zeroline = TRUE,
                       color = "dodgerblue",
                       titlefont = list(size = 12), 
                       tickfont = list(size = 12))) %>%
  layout(hovermode = "x unified") %>% 
  plotly::config(displayModeBar = FALSE) %>% 
  plotly_build()

# weekly
weekly_fig <- plot_ly(net__gsuite_license_weeks)%>%
  add_trace(x = ~day_date, 
            y = ~net_change, 
            type = 'bar', 
            name = ~ifelse(net_change < 0, "Decrease", "Increase"),
            color = ~net_change < 0, 
            colors = c("lawngreen", "red")) %>%
  add_trace(x = ~day_date, y = ~cumulative, 
            type = 'scatter', 
            mode = 'lines+markers',
            yaxis = 'y2',
            line = list(color = 'dodgerblue',
                        width = 4),
            marker = list(color = 'dodgerblue'),
            name = "Cumulative") %>% 
  layout(title = list(text = ' GSuite Weekly License Change',
                      y = 0.95, font = list(size = 22))) %>% 
  
  layout(xaxis = list(title = "",
                      tickfont = list(size = 16))) %>% 
  
  layout(yaxis = list(side = 'left', 
                      title = 'Net Weekly', 
                      showgrid = FALSE, 
                      zeroline = TRUE,
                      color = "black",
                      titlefont = list(size = 16), 
                      tickfont = list(size = 16))) %>% 
  
  layout(yaxis2 = list(side = 'right', 
                       automargin = TRUE,
                       overlaying = "y",
                       showticklabels = TRUE,
                       title = 'Cumulative', 
                       showgrid = FALSE, 
                       zeroline = TRUE,
                       color = "dodgerblue",
                       titlefont = list(size = 12), 
                       tickfont = list(size = 12))) %>%
  layout(hovermode = "x unified") %>%
  plotly::config(displayModeBar = FALSE) %>% 
  plotly_build()

# monthly
monthly_color <- ifelse(net__gsuite_license_months$net_change > 0, "lawngreen", "red")

monthly_fig <- plot_ly(net__gsuite_license_months)%>%
  add_trace(x = ~day_date, 
            y = ~net_change, 
            type = 'bar', 
            name = ~ifelse(net_change < 0, "Decrease", "Increase"),
            marker = list(color = monthly_color)) %>%
  add_trace(x = ~day_date, y = ~cumulative, 
            type = 'scatter', 
            mode = 'lines+markers',
            yaxis = 'y2',
            line = list(color = 'dodgerblue',
                        width = 4),
            marker = list(color = 'dodgerblue'),
            name = "Cumulative") %>% 
  layout(title = list(text = ' GSuite Monthly License Change',
                      y = 0.95, font = list(size = 22))) %>% 
  
  layout(xaxis = list(title = "",
                      tickfont = list(size = 16))) %>% 
  
  layout(yaxis = list(side = 'left', 
                      title = 'Net Monthly', 
                      showgrid = FALSE, 
                      zeroline = TRUE,
                      titlefont = list(size = 16), 
                      tickfont = list(size = 16))) %>% 
  
  layout(yaxis2 = list(side = 'right', 
                       automargin = TRUE,
                       overlaying = "y",
                       showticklabels = TRUE,
                       title = 'Cumulative', 
                       showgrid = FALSE, 
                       zeroline = TRUE,
                       color = "dodgerblue",
                       titlefont = list(size = 12), 
                       tickfont = list(size = 12))) %>%
  layout(hovermode = "x unified") %>%
  plotly::config(displayModeBar = FALSE) %>%
  plotly_build()

# Save as a list to later read into Shiny app from inputSelect
gsuite_license_plots <- list(daily_fig = (daily_fig), weekly_fig = (weekly_fig), monthly_fig = (monthly_fig))

saveRDS(gsuite_license_plots, "c:/scripts/gsuite_license_plots.rds")
