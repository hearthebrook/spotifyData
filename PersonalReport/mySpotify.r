library(tidyverse)
library(jsonlite)


# Combining brook data
brook_data_one <- fromJSON("PersonalReport/data/StreamingHistory_music_0.json")
brook_data_two <- fromJSON("PersonalReport/data/StreamingHistory_music_1.json")
brook_data_three <- fromJSON("PersonalReport/data/StreamingHistory_music_2.json")
brook_data_four <- fromJSON("PersonalReport/data/StreamingHistory_music_3.json")

brook_all <- bind_rows(brook_data_one, brook_data_two, brook_data_three, brook_data_four)


# Use Lubridate to format datetiems as wanted
brook_all <- brook_all %>% 
    mutate(endTime = ymd_hm(endTime))


# Build a visual (For Fall and Winter Semester)
brook_recent_days <- brook_all %>%
    mutate(date = date(endTime)) %>%
    group_by(date) %>%
    summarize(total_ms = sum(msPlayed)) %>%
    mutate(
        total_min = total_ms / 60000,
        total_hr = total_min / 60,
        event = case_when(
            date >= as.Date("2023-09-11") & date <= as.Date("2023-12-18") ~ "Fall Semester",
            date >= as.Date("2023-12-18") & date <= as.Date("2024-01-08") ~ "Break",
            date >= as.Date("2024-01-09") & date <= as.Date("2024-03-14") ~ "Winter Semester"
        ),
        day_of_week = wday(date, label=TRUE), 
        event = factor(event, levels=c("Fall Semester", "Break", "Winter Semester"))

    ) %>%
    filter(between(date, as.Date("2023-09-11"), as.Date("2024-03-14")))


high_df <- data.frame(
    date = c(as.Date("2023-11-22")),
    total_hr = 11.56
)

journal_two <- data.frame(
    date = c(as.Date("2024-02-20")),
    total_hr = 8.92
)

ggplot(brook_recent_days, aes(x=date, y=total_hr)) +
    geom_line(color="steelblue") +
    geom_point(aes(color=event)) +
    geom_point(data=high_df, aes(x=date, y=total_hr), color="orange") +
    geom_text(data=high_df, aes(x=date, y=total_hr, label="Journal Entry #1"),
        vjust = -0.5, hjust = 0.5
    ) +
    geom_point(data=journal_two, aes(x=date, y=total_hr), color="orange") +
    geom_text(data=journal_two, aes(x=date, y=total_hr, label="Journal Entry #2"),
        vjust = -0.5, hjust = 0.5
    ) +
    xlab("") +
    scale_x_date(
        # limit=c(as.Date("2023-09-11"), as.Date("2024-03-14")),
        date_labels= "%b",
        date_breaks = "1 month"
    ) +
    labs(y= "Hours Listened", title="What Makes Me Listen?", legend="Event") +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust=0.5)
    )



# Visual #2 - mean listening time for events
brook_period <- brook_recent_days %>%
    group_by(event) %>%
    summarise(
        num_of_days = n(),
        avg_listened = mean(total_hr),
        low = min(total_hr),
        high = max(total_hr)
    )


ggplot(brook_recent_days, aes(x=event, y=total_hr, color=event))+
    geom_boxplot() +
    labs(x=NULL, y= "Hours Listened", title="Periods of time") +
    theme_light()



# Visual #3 - day of week 

ggplot(brook_recent_days, aes(x = day_of_week, y = total_hr)) +
  facet_wrap(~event) +
  stat_summary(fun = "mean", geom = "col", fill = "steelblue") +
  labs(y = "Average Total Hours", x=NULL) +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0.05))






# Visual 4 - Does spotify know my class schedule?

brook_winter_semester <- brook_all %>%
    mutate(
        date = date(endTime),
        hour = hour(endTime)
    ) %>%
    filter(
        between(date, as.Date("2024-01-09"), as.Date("2024-03-14")),
        trackName != "r0ut1n3 - Gamers Mix"
    ) %>%
    group_by(date, hour) %>%
    summarize(
        time_listened = sum(msPlayed) / 3600000,
        number_of_tracks = n()
    ) %>%
    mutate(
        day_of_week = wday(date, label=TRUE)
    ) %>% 
    filter(day_of_week %in% c("Mon", "Tue", "Wed", "Thu", "Fri"))



day_plot <- ggplot(brook_winter_semester, aes(x=hour, y=time_listened)) +
    facet_wrap(~day_of_week, nrow = 1) +
    stat_summary(fun = "sum", geom = "line") 
    # geom_vline(xintercept = 12, color="red", linetype="dashed") 
    

day_order <- c("Mon", "Tue", "Wed", "Thu", "Fri")

class_shades <- data.frame(
    day_of_week = c("Mon", "Mon", "Tue", "Tue", "Wed", "Wed", "Wed", "Thu", "Fri"),
    xmin = c(10.25, 12.75, 8, 11.5, 10.25, 12.75, 16.5, 8, 10.25),
    xmax = c(11.25, 13.75, 9.5, 12.5, 11.25, 13.75, 18, 9.5, 11.25),
    ymin = -Inf,
    ymax = Inf,
    time_listened = 5,
    hour = 5
) %>%
mutate(
    day_of_week = factor(day_of_week, levels=day_order)
)



day_plot + geom_rect(
    data=class_shades,
    aes(xmin=xmin,
        xmax=xmax,
        ymin = ymin,
        ymax = ymax), fill = "lightblue",
    alpha = 0.5
) +
theme_bw() +
coord_cartesian(expand = FALSE) +
labs(
    x="Hour of Day (Military Time)",
    y="Collective Time Listened (hr)",
    title="Does Spotify Know When My Classes Are?"
)
 






