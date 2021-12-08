# SLEEPING MANIKIN
# exploratory data analysis
# written by Tom Parkinson, October 2019


#### SETUP ####

# use pacman for package management
require("pacman")

# load packages
p_load(tidyverse, lubridate, here, scales)

# Use "here" package to set working directory
here::i_am("manikin.R")


#### DATA PREP ####

# load data
df_manikin <- read_csv(file = here("data_manikin.csv")) %>%
  rename("temp_air" = "tempAir") %>%
  mutate(time = mdy_hm(x = time, tz = "America/Los_Angeles"))

# summarise data
df_use <- df_manikin %>% 
  group_by(case, system, bedding, clo) %>% 
  summarise(across(where(is.numeric), median)) %>%
  ungroup() %>%
  arrange(case)

# make summary data long
df_use <- df_use %>%
  pivot_longer(cols = -(1:5), names_to = c("segment", ".value"), names_pattern = "(.+)_(.+)", values_to = "value") %>%
  mutate(across(where(is.character), factor))

# clean segment strings
df_use <- df_use %>%
  mutate(segment = str_replace_all(str_to_lower(segment), c("^l" = "left_",  "^r" = "right_")))

# merge clo and bedding into new variable for plots
df_use <- df_use %>%
  mutate(case2 = str_c(clo, " clo,\n", bedding),
         case2 = factor(case2, levels = c("Light clo,\nNo Blanket", "Light clo,\nHead Exposed", "Light clo,\nHead & Arm Exposed", 
                                          "Heavy clo,\nNo Blanket", "Heavy clo,\nHead & Arm Exposed")),
         system = factor(system, levels = c("None", "AC", "Low Heat", "Medium Heat", "High Heat", "Low Fan", "Medium Fan", "High Fan")))

# add power consumption in watts
df_use <- df_use %>%
  mutate(watts = recode(system, "None" = 0, "Low Heat" = 30, "Medium Heat" = 55, "High Heat" = 80,
                        "Low Fan" = 4.4, "Medium Fan" = 8.7, "High Fan" = 16.9)) %>%
  relocate(watts, .after = "system")


#### PLOTS ####

# plot manikin power by system
df_use %>%
  filter(segment == "all" & temp_air != "23" & case != "1") %>% # get overall and remove baseline case
  mutate(temp_air = str_c("Ta = ", temp_air)) %>% # label for plots
  ggplot(., aes(x = system, y = pow, color = case2, group = case2)) +
  geom_point(size = 3, shape = 16) +
  geom_line() +
  scale_y_continuous(labels = number_format(accuracy = 1L)) +
  facet_grid(cols = vars(temp_air), scale = "free_x") +
  labs(title = "Manikin Power", 
       x = NULL, y = expression("Power"~(W/m^2)), color = NULL, shape = NULL) +
  guides(colour = guide_legend(nrow = 1)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

# save plot
ggsave(file = here("mainkin_power.png"), width = 8, height = 5, dpi = 300)


# plot manikin temperature by system
df_use %>%
  filter(segment == "all" & temp_air != "23" & case != "1") %>% # get overall and remove baseline case
  mutate(temp_air = str_c("Ta = ", temp_air)) %>% # label for plots
  ggplot(., aes(x = system, y = temp, color = case2, group = case2)) +
  geom_point(size = 3) +
  geom_line() +
  scale_y_continuous(breaks = seq(31, 34, by = 0.5), labels = number_format(accuracy = 0.1, suffix = "°C")) +
  facet_grid(cols = vars(temp_air), scale = "free_x") +
  labs(title = "Manikin Temperature", 
       x = NULL, y = NULL, color = NULL, shape = NULL) +
  guides(colour = guide_legend(nrow = 1)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

# save plot
ggsave(file = here("manikin_temp.png"), width = 8, height = 5, dpi = 300)
