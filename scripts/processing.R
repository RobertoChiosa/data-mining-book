
source("./scripts/packages.R") # load necessary packages

## create raw weekly data
df <- read.csv("./data/data2.csv", header = TRUE, sep = ",", dec = ".", check.names = FALSE)

df <- df %>% select(Date_Time, Total_Power)
# subset
df1 <- df[c(117313:117984),] 
df1$Type <- "Correct"

# add missing values
df1$Total_Power[c(120:140)] <- NA
df1$Total_Power[c(500:560)] <- NA

df1$Type[c(120:140, 500:560)] <- "Missing Value"

# add outliers and inconsistances
df1$Total_Power[234] <- 1933
df1$Total_Power[449] <- 1200
df1$Type[c(234,449)] <- "Outlier"

df1$Total_Power[321] <- -100
df1$Total_Power[212] <- -100
df1$Total_Power[243] <- -110
df1$Type[c(321, 212, 243)] <- "Inconsistence"

write.csv(df1,"./data/week_not_clean.csv", row.names = FALSE)

df1 %>%
ggplot() + 
geom_line(aes(x = as.POSIXct(Date_Time, format = "%H:%M:%S" , tz = "Europe/Rome") , y = Total_Power) , 
          color = "grey", size = 0.7) +
  scale_x_datetime(
    breaks = date_breaks("12 hour"),                     # specify breaks every 4 hours
    labels = date_format(("%H:%M") , tz="Etc/GMT+12"),  # specify format of labels
    expand = c(0,0)                                     # expands x axis
  ) +
  scale_y_continuous(
    limits = c(0,ceiling(max(df1$Total_Power)) +10 ),       # set limits from 0 to higher power consumption
    expand=c(0,0)                                       # expands x axis
  ) +
  theme_bw() +
  ggplot2::theme(
                 axis.title.x = element_text(size = 12, margin = margin(t = 20, r = 20, b = 0, l = 0)),
                 axis.title.y = element_text(size = 12, margin = margin(t = 20, r = 20, b = 0, l = 0)),
                 axis.text.x = element_text(size = 11, angle = 45, vjust = .0),
                 axis.text.y = element_text(size = 11 , vjust = .4),
                 panel.background = element_rect(fill = "white")
  ) +
  labs(x = "Hour" , y = "Power [kW]")

