

# This code calculates and graphs cumulative growing degree day values based on 
# historical observations at the AZMET Willcox Bench station for the 2020 July
# issue of the Climate Viticulture Newsletter

# AZMET data are at: https://cals.arizona.edu/azmet/

# Author:
# Jeremy Weiss, Climate and Geospatial Extension Scientist
# School of Natural Resources and the Environment
# University of Arizona
# 520-626-8063, jlweiss@email.arizona.edu


# SETUP --------------------


# Load needed libraries
library("dplyr")
library("ggplot2")
library("lubridate")
library("extrafont")

# Load additional font options for plotting
font_import()
y
loadfonts(device = "postscript")

# Load AZMET station list
stn_list <- read.csv("azmet-station-list.csv", sep = ",")

# Set the AZMET station name and years of interest
stn_name <- "Willcox Bench"
yr_start <- stn_list$start_yr[which(stn_list$stn == stn_name)]
yr_end <- stn_list$end_yr[which(stn_list$stn == stn_name)]

# Load function to download and transform daily AZMET data
source("azmet.daily.data.download.R")


# DOWNLOAD AND TRANSFORM DAILY AZMET DATA --------------------


stn_data <- azmet.daily.data.download(stn_name)

# Retain necessary variables
stn_data <- select(stn_data, Date, Year, Month, Day, JDay, Tmean)

# Convert temperature from Celsius to Fahrenheit
stn_data$Tmean <- (1.8 * stn_data$Tmean) + 32


# CALCULATE CUMULATIVE GROWING DEGREE DAYS --------------------


# Calculate growing degree days
t_base <- 50
stn_data["GDDs"] <- NA
for (i in 1:nrow(stn_data)) {
  if (stn_data$Tmean[i] < t_base) {
    stn_data$GDDs[i] <- 0
  } else {
    stn_data$GDDs[i] <- stn_data$Tmean[i] - t_base
  }
}
rm(i)

# Cumulative growing degree days start on December 1 and continue through the 
# growing season of the following calendar year. This code is being built as 
# part of the chill portion-heat accumulation analysis code used in other 
# applications. Chill portions start accumulating on November 1. 
for (yr in yr_start:(yr_end - 1)) {
  
  # For leap year as first calendar year
  if (leap_year(yr) == TRUE) {
    df <- rbind(stn_data %>% filter(Year == yr & JDay >= 306),
                stn_data %>% filter(Year == (yr + 1) & JDay <= 304))
    
    df["GS"] <- paste(yr, (yr + 1), sep = "-")
    df["iDay"] <- seq(1, nrow(df))
    df["CGDDs"] <- NA
    
    for (d in 1:nrow(df)) {
      if (df$JDay[d] >= 306 & df$JDay[d] < 336) {
        df$CGDDs[d] <- NA
      } else if (df$JDay[d] == 336) {
        df$CGDDs[d] <- df$GDDs[d]
      } else {
        df$CGDDs[d] <- df$GDDs[d] + df$CGDDs[d - 1]
      }
    }
    rm(d)
  }
  
  # For leap year as second calendar year
  if (leap_year(yr + 1) == TRUE) {
    df <- rbind(stn_data %>% filter(Year == yr & JDay >= 305),
                stn_data %>% filter(Year == (yr + 1) & JDay <= 305))
    
    df["GS"] <- paste(yr, (yr + 1), sep = "-")
    df["iDay"] <- seq(1, nrow(df))
    df["CGDDs"] <- NA
    
    for (d in 1:nrow(df)) {
      if (df$JDay[d] >= 305 & df$JDay[d] < 335) {
        df$CGDDs[d] <- NA
      } else if (df$JDay[d] == 335) {
        df$CGDDs[d] <- df$GDDs[d]
      } else {
        df$CGDDs[d] <- df$GDDs[d] + df$CGDDs[d - 1]
      }
    }
    rm(d)
  }
  
  # For no leap year as either calendar year
  if (leap_year(yr) == FALSE & leap_year(yr + 1) == FALSE) {
    df <- rbind(stn_data %>% filter(Year == yr & JDay >= 305),
                stn_data %>% filter(Year == (yr + 1) & JDay <= 304))
    
    df["GS"] <- paste(yr, (yr + 1), sep = "-")
    df["iDay"] <- seq(1, nrow(df))
    df["CGDDs"] <- NA
    
    for (d in 1:nrow(df)) {
      if (df$JDay[d] >= 305 & df$JDay[d] < 335) {
        df$CGDDs[d] <- NA
      } else if (df$JDay[d] == 335) {
        df$CGDDs[d] <- df$GDDs[d]
      } else {
        df$CGDDs[d] <- df$GDDs[d] + df$CGDDs[d - 1]
      }
    }
    rm(d)
  }
  
  if (yr == yr_start) {
    cgdds <- df
  } else {
    cgdds <- rbind(cgdds, df)
  }
  
}
rm(yr)


# MAKE AND SAVE TIMESERIES PLOT --------------------


# To help with setting axis breaks on the figures, create a function that rounds 
# numbers to the nearest specified base
myround <- function(x, base) {
  base * round(x / base)
}

# To facilitate graphing interannual data that includes leap years, subtract 1
# from 'JDay' values in a leap year. The graph will start on March 1, so this
# won't be an issue.
cgdds$JDay[which(leap_year(cgdds$Year) == TRUE)] <- 
  cgdds$JDay[which(leap_year(cgdds$Year) == TRUE)] - 1

# Create a ggplot object for graphing cumulative growing degree days
p <- ggplot(data = cgdds) +
  
  # Add CGDD data for all years as individual time series
  geom_line(aes(x = JDay, y = CGDDs, color = GS),
            lineend = "round",
            linetype = "solid",
            size = 1.0) +
  
  # Add the title, subtitle, axis labels, and caption
  ggtitle("Heat Accumulation") +
  labs(subtitle = "AZMET Willcox Bench station",
       x = "\nDate",
       y = "Cumulative  Growing  Degree  Days\n",
       caption = paste0(
         "\ndata source: AZMET (cals.arizona.edu/azmet)",
         "\n50°F-based GDD calculation, December 1 start date")) +
  
  # Rename variable description and set the time series line colors
  scale_color_brewer(name = "Growing \nSeason", palette = "Dark2") +
  
  # Specify axis breaks, gridlines, and limits
  scale_x_continuous(breaks = c(60, 91, 121, 152, 182, 213),
                     labels = c("Mar", "Apr", "May", "Jun", "Jul", "Aug"),
                     limits = c((60 - 1), (213 + 1)),
                     expand = c(0.005, 0.0)) +
  scale_y_continuous(
    breaks = seq(
      0,
      myround(
        ceiling(
          max(filter(cgdds, JDay <= (213 + 1))$CGDDs, na.rm = TRUE)),
        100),
      by = 500
      ),
    limits = c(0, max(filter(cgdds, JDay <= (213 + 1))$CGDDs, na.rm = TRUE)),
    expand = c(0.03, 0.0)) +
  
  # Further customize the figure appearance
  theme_light(base_family = "Source Sans Pro") +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(color = "gray40", size = 10),
        axis.text.y = element_text(color = "gray40", size = 10),
        axis.ticks.x.bottom = element_line(color = "gray80", size = 0.25),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(0.0, "mm"),
        axis.ticks.length.y = unit(0.0, "mm"),
        axis.title.x = element_text(color = "gray40", size = 10),
        axis.title.y = element_text(color = "gray40", size = 10),
        legend.direction = "vertical",
        legend.text = element_text(color = "gray40", size = 10),
        legend.title = element_text(color = "gray40", size = 10, face = "bold"),
        legend.position = "right",
        panel.border = element_blank(),
        panel.grid.major.x = element_line(color = "gray80", size = 0.25),
        panel.grid.major.y = element_line(color = "gray80", size = 0.25),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_text(color = "gray40", hjust = 0.0, size = 8),
        plot.caption.position = "plot",
        plot.margin = unit(c(1, 1 ,1, 1), "mm"),
        plot.subtitle = (element_text(family = "Source Serif Pro", size = 12)), 
        plot.title = (
          element_text(face = "bold", family = "Source Serif Pro", size = 16)
          ),
        plot.title.position = "plot")

p

#  Save the figure as a .png file in the current directory
#ggsave("./cgdds-azmet-willcox-bench-cvn202007.eps",
#       plot = p, device = cairo_pdf, path = NULL, scale = 1,
#       width = 6, height = 4, units = "in", dpi = 300)

ggsave(file = paste0("cgdds-azmet-willcox-bench-cvn-2020-", 
                     Sys.Date(),
                     ".eps"),
       plot = p, device = cairo_pdf, path = NULL, scale = 1,
       width = 6, height = 4, units = "in", dpi = 300)

