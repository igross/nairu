# =============================================
# 1) Setup & libraries
# =============================================
suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(ggplot2)
  library(tidyr)
  library(plotly)
  library(readrba)   # for read_rba()
  library(scales)
})

# =============================================
# 2) Load data & RMSE lookup
# =============================================
cash_rate <- readRDS("combined_data/all_data.Rds")    # columns: date, cash_rate, scrape_time
load("combined_data/rmse_days.RData")                 # object rmse_days: days_to_meeting ↦ finalrmse

blend_weight <- function(days_to_meeting) {
  # Linear blend from 0 to 1 over last 30 days
  pmax(0, pmin(1, 1 - days_to_meeting / 30))
}

# =============================================
# 3) Define RBA meeting schedule
# =============================================
meeting_schedule <- tibble(
  meeting_date = as.Date(c(
    "2025-02-18","2025-04-01","2025-05-20","2025-07-08",
    "2025-08-12","2025-09-30","2025-11-04","2025-12-09"
  ))
) %>%
  mutate(expiry = floor_date(meeting_date, "month")) %>%
  select(expiry, meeting_date)

# =============================================
# 4) Identify last meeting, collect scrapes
# =============================================
last_meeting <- max(meeting_schedule$meeting_date[meeting_schedule$meeting_date <= Sys.Date()])

all_times <- sort(unique(cash_rate$scrape_time))
scrapes   <- all_times[all_times > last_meeting]   # every scrape after the last decision

# =============================================
# 5) Build implied‐mean panel for each scrape × meeting
# =============================================
all_list <- map(scrapes, function(scr) {
  scr_date <- as.Date(scr)  # convert POSIXct to Date

  df <- cash_rate %>%
    filter(scrape_time == scr) %>%
    select(date, forecast_rate = cash_rate) %>%
    filter(date >= min(meeting_schedule$expiry),
           date <= max(meeting_schedule$expiry)) %>%
    left_join(meeting_schedule, by = c("date" = "expiry")) %>%
    distinct() %>% 
    arrange(date)

  rt  <- df$forecast_rate[1]
  out <- vector("list", nrow(df))

  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    dim <- days_in_month(row$date)

    if (!is.na(row$meeting_date)) {
      nb    <- (day(row$meeting_date) - 1) / dim
      na    <- 1 - nb
      r_tp1 <- (row$forecast_rate - rt * nb) / na
    } else {
      r_tp1 <- row$forecast_rate
    }

    out[[i]] <- tibble(
      scrape_time     = scr,
      meeting_date    = row$meeting_date,
      implied_mean    = r_tp1,
      days_to_meeting = as.integer(row$meeting_date - scr_date)
    )

    if (!is.na(row$meeting_date)) rt <- r_tp1
  }

  bind_rows(out)
})

all_estimates <- bind_rows(all_list) %>%
  filter(!is.na(meeting_date)) %>%
  left_join(rmse_days, by = "days_to_meeting") %>%
  rename(stdev = finalrmse)

# =============================================
# 6) Build bucketed probabilities for each row
# =============================================
bucket_centers <- seq(0.10, 5.10, by = 0.25)
half_width     <- 0.125

current_rate <- read_rba(series_id = "FIRMMCRTD") %>%
  filter(date == max(date)) %>%
  pull(value)

current_center <- bucket_centers[ which.min( abs(bucket_centers - current_rate) ) ]

bucket_list <- vector("list", nrow(all_estimates))
for (i in seq_len(nrow(all_estimates))) {
  mu_i    <- all_estimates$implied_mean[i]
  sigma_i <- all_estimates$stdev[i]
  d_i     <- all_estimates$days_to_meeting[i]
  rc      <- current_rate  

  # Probabilistic method
  p_vec <- sapply(bucket_centers, function(b) {
    lower <- b - half_width
    upper <- b + half_width
    pnorm(upper, mean = mu_i, sd = sigma_i) - pnorm(lower, mean = mu_i, sd = sigma_i)
  })
  p_vec[p_vec < 0]    <- 0
  p_vec[p_vec < 0.01] <- 0
  p_vec <- p_vec / sum(p_vec)

  # Linear method
  nearest <- order(abs(bucket_centers - mu_i))[1:2]
  b1 <- min(bucket_centers[nearest])
  b2 <- max(bucket_centers[nearest])
  w2 <- (mu_i - b1) / (b2 - b1)
  l_vec <- numeric(length(bucket_centers))
  l_vec[bucket_centers == b1] <- 1 - w2
  l_vec[bucket_centers == b2] <- w2

  # Blending
  blend <- blend_weight(d_i)
  v <- blend * l_vec + (1 - blend) * p_vec

  bucket_list[[i]] <- tibble(
    scrape_time   = all_estimates$scrape_time[i],
    meeting_date  = all_estimates$meeting_date[i],
    implied_mean  = mu_i,
    stdev         = sigma_i,
    days_to_meeting = d_i,
    bucket        = bucket_centers,
    probability_linear = l_vec,
    probability_prob   = p_vec,
    probability        = v,
    diff           = bucket_centers - current_rate,
    diff_s         = sign(bucket_centers - current_rate) * abs(bucket_centers - current_rate)^(1/4)
  )
}

all_estimates_buckets <- bind_rows(bucket_list)

print(all_estimates_buckets, n=20, width = Inf)

# =============================================
# 7 Bar charts for every future meeting (latest scrape)
# =============================================

# 1) Identify all future RBA meetings
future_meetings <- meeting_schedule$meeting_date
future_meetings <- future_meetings[future_meetings > Sys.Date()]

# 2) Grab the most recent scrape_time
latest_scrape <- max(all_estimates_buckets$scrape_time)

# 3) Loop through each meeting, filter & plot
for (mt in future_meetings) {
  # a) extract the slice for this meeting
  bar_df <- all_estimates_buckets %>%
    filter(
      scrape_time  == latest_scrape,
      meeting_date == mt
    )

  # d) create the bar chart
p <- ggplot(bar_df, aes(x = factor(bucket), y = probability, fill = diff_s)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_gradient2(
    midpoint = 0,
    low      = "#0022FF",    # vivid blue for cuts
    mid      = "#B3B3B3",    # white at no change
    high     = "#FF2200",    # vivid red for hikes
    limits   = range(bar_df$diff_s, na.rm = TRUE)
  ) +
    labs(
      title    = paste("Cash Rate Outcome Probabilities —", format(as.Date(mt), "%d %B %Y")),
      subtitle = paste("As of", format(as.Date(latest_scrape), "%d %B %Y")),
      x        = "Target Rate (%)",
      y        = "Probability (%)"
    ) +
   theme_bw()  +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))

                     
  ggsave(
      filename = paste0("docs/rate_probabilities_", gsub(" ", "_", mt), ".png"),
    plot     = p,
    width    = 6,
    height   = 4,
    dpi      = 300,
    device   = "png"
  )
}

                     tail(bar_df,n=20,width=Inf)

# =============================================
# Define next_meeting (the very next date after today)
# =============================================
next_meeting <- meeting_schedule %>%
  filter(meeting_date > Sys.Date()) %>%
  slice_min(meeting_date) %>%
  pull(meeting_date) 

# —————————————————————————————————————————————————————————————————————
# build top3_df and turn the numeric bucket centers into descriptive moves
# —————————————————————————————————————————————————————————————————————
# 1) compute the true “no‐change” bucket centre once:
current_center <- bucket_centers[which.min(abs(bucket_centers - current_rate))]

top3_buckets <- all_estimates_buckets %>% 
  filter(meeting_date == next_meeting) %>%          # keep the target meeting
  group_by(bucket) %>%                              # pool all scrapes
  summarise(probability = mean(probability, na.rm = TRUE),
            .groups = "drop") %>%                   # average across scrapes
  slice_max(order_by = probability, n = 3, with_ties = FALSE) %>% 
  pull(bucket)

tail(top3_buckets, n = 10, width = Inf)

# B) now build top3_df by filtering all dates to those same 3 buckets
top3_df <- all_estimates_buckets %>%
  filter(
    meeting_date == next_meeting,
    bucket %in% top3_buckets
  ) %>%
  # compute diff & move exactly as before
  mutate(
    diff_center = bucket - current_center,
    move = case_when(
      near(diff_center, -0.75) ~ "-75 bp cut",
      near(diff_center, -0.50) ~ "-50 bp cut",
      near(diff_center, -0.25) ~ "-25 bp cut",
      near(diff_center,  0.00) ~ "No change",
      near(diff_center,  0.25) ~ "+25 bp hike",
      near(diff_center,  0.50) ~ "+50 bp hike",
      near(diff_center,  0.75) ~ "+75 bp hike",
      TRUE                      ~ sprintf("%+.0f bp", diff_center*100)
    ),
    move = factor(
      move,
      levels = c("-50 bp cut","-25 bp cut","No change","+25 bp hike","+50 bp hike")
    )
  ) %>%
  select(-diff_center)

tail(top3_df, n = 10, width = Inf)
                     
# 3) then use `move` in your ggplot:
line <- ggplot(top3_df, aes(
    x     = scrape_time  + hours(34),
    y     = probability,
    color = move,
    group = move
  )) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c(
  "-75 bp or more cut" = "#000080",  # navy blue
  "-50 bp cut"         = "#004B8E",
  "-25 bp cut"         = "#5FA4D4",
  "No change"          = "#BFBFBF",
  "+25 bp hike"        = "#E07C7C",
  "+50 bp hike"        = "#B50000",
  "+75 bp or more hike"= "#800000"   # dark red
    ),
    name = "",
    na.value = "grey80" 
  ) +  
 scale_x_datetime(  
  breaks = function(x) {  
      seq(  
        from = lubridate::floor_date(min(x), "day"),  
        to   = lubridate::ceiling_date(max(x), "day"),  
        by   = "1 day"  
      ) %>%  
        .[!lubridate::wday(.) %in% c(1, 7)]  # drop Sundays (1) & Saturdays (7)  
    },  
    date_labels = "%d %b"  
  ) + 
  scale_y_continuous( limits = c(0, 1),
    expand = c(0, 0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = paste("Cash Rate Moves for the Next Meeting on", format(as.Date(next_meeting), "%d %b %Y")),
    subtitle = paste("as of", format(as.Date(latest_scrape),   "%d %b %Y")),
    x        = "Forecast date",
    y        = "Probability"
  ) +
  theme_bw() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y  = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = c(1.02, 0.5)
  ) 

# overwrite the previous PNG
ggsave("docs/line.png", line, width = 8, height = 5, dpi = 300)


# =============================================
# Interactive widget
# =============================================
line_int <- line +
  aes(text = paste0(
    "Date: ", format(scrape_time + hours(10), "%Y-%m-%d %H:%M"), "<br>",
    "Probability: ", scales::percent(probability, accuracy = 1)
  ))

interactive_line <- ggplotly(line_int, tooltip = "text") %>%
  layout(
    hovermode = "x unified",
    legend    = list(x = 1.02, y = 0.5, xanchor = "left")
  )

htmlwidgets::saveWidget(
  interactive_line,
  file          = "docs/line_interactive.html",
  selfcontained = TRUE
)
