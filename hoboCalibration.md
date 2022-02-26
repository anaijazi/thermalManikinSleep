Appendix: HOBO Calibration
================

Load libraries

``` r
library(tidyverse)
library(lubridate)
library(broom)
```

Load HOBO raw data

``` r
data_dir <- "calibration/hobo"
data_merged <- list()

for (i in 1:4){
  data_files <- list.files(path = data_dir, pattern = paste0("*_", i, ".csv"), full.names = TRUE)
  
  # Hobo 1 contains two temperature sensors
  if(i == 1){
    colNames_hobo <-c("Number", "Time", "Hobo_1.3", "Hobo_1.4", "HostConnected", "Stopped", "EndOfFile")
  }
  
  else{
    colNames_hobo <- c("Number", "Time", paste0("Hobo_",i), "HostConnected", "Stopped", "EndOfFile")
  }
  
  data_all <- lapply(data_files, read_csv, skip = 2, col_names = colNames_hobo, col_types = cols())
  data_merged[[i]] <- data_all %>%
  reduce(full_join) %>%
  select(-Number, -HostConnected, -Stopped, -EndOfFile) %>%
  pivot_longer(cols = starts_with("Hobo_"), names_to = "Hobo", names_prefix = "Hobo_", values_to = "Temperature_F")
}

hobo_data <- data_merged %>%
  reduce(full_join) %>%
  mutate(Temperature_C = (Temperature_F-32)*5/9) %>%
  mutate(Hobo = as.numeric(Hobo)) %>%
  mutate(Time = case_when(Hobo == 4 ~ mdy_hm(Time),
                          TRUE ~ mdy_hms(Time))) %>%
  select(-Temperature_F)

rm(merged_calibration)
rm(data_calibration)
rm(data_merged)
rm(data_all)
```

Subset HOBO data to steady state periods

``` r
# Read start and stop times for each HOBO and set point
startStop <- read_csv("calibration/startstop.csv", col_types = cols())

hobo_steady <- hobo_data %>%
  left_join(startStop) %>%
  mutate(across(c(starts_with("Start_"), starts_with("End_")), mdy_hm)) %>%
  mutate(steady_15 = ifelse(Time > Start_15 & Time < End_15, TRUE, FALSE)) %>%
  mutate(steady_25 = ifelse(Time > Start_25 & Time < End_25, TRUE, FALSE)) %>%
  mutate(steady_35 = ifelse(Time > Start_35 & Time < End_35, TRUE, FALSE)) %>%
  mutate(steady = ifelse(steady_15 == TRUE | steady_25 == TRUE | steady_35 == TRUE, TRUE, FALSE)) %>%
  filter(steady == TRUE) %>%
  mutate(Hobo = factor(Hobo)) %>%
  mutate(steadyT = case_when(steady_15 == TRUE ~ 15,
                            steady_25 == TRUE ~ 25, 
                            steady_35 == TRUE ~ 35)) %>%
  mutate(difference = Temperature_C - steadyT) %>%
  group_by(Hobo) %>%
  arrange(steadyT) %>%
  mutate(rowNumber = 1:n()) %>%
  select(Hobo, rowNumber, Temperature_C, steadyT) %>%
  mutate(Difference = Temperature_C - steadyT)
```

``` r
theme_custom = function() {
  theme_minimal() %+replace%
    theme(legend.position = "top") +
    theme(panel.grid = element_blank())
}
plot_colors <- c("#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377")
```

Temperature versus time step (during steady state)  
![](hoboCalibration_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Difference from set point temperature versus time step (during steady
state)  
![](hoboCalibration_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Difference from set point temperature versus set point temperature
(during steady state)  
![](hoboCalibration_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Calculate coefficient for linear model where the temperature difference
is a function of set point temperature

``` r
hobo_lm <- hobo_steady %>%
  group_by(Hobo) %>%
  do(tidy(lm(Difference ~ steadyT, .))) %>%
  select(-std.error, -statistic, -p.value) %>%
  mutate(term = case_when(term == "(Intercept)" ~ "Intercept", 
                          TRUE ~ "Slope")) %>%
  mutate(estimate = round(estimate, digits = 5)) %>%
  pivot_wider(names_from = term, values_from = estimate)
```

Calculate calibrated temperature based on linear model

``` r
hobo_calibrate <- hobo_steady %>%
  left_join(hobo_lm) %>%
  mutate(Offset = Intercept + Temperature_C*Slope) %>%
  mutate(Temperature_C_calibrate = Temperature_C - Offset) %>%
  mutate(Difference.calibrated = Temperature_C_calibrate - steadyT)
```

Calibrated temperature versus time step (during steady state)  
![](hoboCalibration_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Calibrated difference from set point temperature versus time step
(during steady state)  
![](hoboCalibration_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

New difference from set point temperature versus set point temperature
(during steady state)  
![](hoboCalibration_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
