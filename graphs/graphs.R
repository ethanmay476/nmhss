library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)


data("data_start_2014_filter")

unique_states <- data_start_2014_filter %>%
  distinct(LST) %>%
  arrange(LST) %>%
  pull(LST)

########################TELE PDF########################

pdf("~/GitHub/nmhss/graphs/TELEMED_all_states_data.pdf", width = 11, height = 8.5)
# tele2122_clean <- data_start_2014_filter %>%
#   filter(!TREATTELEMEDINCE %in% c("L","M","-1","-5","-3"))

p_all <- ggplot(data = data_start_2014_filter, aes(x = YEAR, fill = TREATTELEMEDINCE)) +
  geom_bar() +
  labs(title = "Combined Data for All States", x = "Year", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_all) # Explicitly print the combined plot




for (state_name in unique_states)
  {
  byState <- data_start_2014_filter %>%
    filter(LST == state_name)

  # Generate and print the plot for the current state
  p <- ggplot(data = byState, aes(x = YEAR, fill = TREATTELEMEDINCE)) +
    geom_bar() +
    labs(title = paste("Data for State:", state_name), x = "Year", y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  print(p) # Explicitly print the plot
 }
dev.off()
#################################################################################
# column_names <- names(data_start_2014_filter)
#
# # Create a new dataframe with one column containing the column names
# column_names_df <- data.frame(ColumnNames = column_names)
# view(column_names_df)




########################DIAL PDF########################

pdf("~/GitHub/nmhss/graphs/DIAL_all_states_data.pdf", width = 11, height = 8.5)
tele2122_clean <- data_start_2014_filter %>%
  filter(!TREATDIALTHRPY %in% c("L","M","-1","-5"))

p_all <- ggplot(data = tele2122_clean, aes(x = YEAR, fill = TREATDIALTHRPY)) +
  geom_bar() +
  labs(title = "Combined Data for All States", x = "Year", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_all) # Explicitly print the combined plot




for (state_name in unique_states)
{
  byState <- tele2122_clean %>%
    filter(LST == state_name)

  # Generate and print the plot for the current state
  p <- ggplot(data = byState, aes(x = YEAR, fill = TREATDIALTHRPY)) +
    geom_bar() +
    labs(title = paste("Data for State:", state_name), x = "Year", y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  print(p) # Explicitly print the plot
}
dev.off()
#################################################################################




########################PSYCOTHRPY PDF########################

pdf("~/GitHub/nmhss/graphs/PSYTHR_all_states_data.pdf", width = 11, height = 8.5)
tele2122_clean <- data_start_2014_filter %>%
  filter(!TREATPSYCHOTHRPY %in% c("L","M","-1","-5","-7"))

p_all <- ggplot(data = tele2122_clean, aes(x = YEAR, fill = TREATPSYCHOTHRPY)) +
  geom_bar() +
  labs(title = "Combined Data for All States", x = "Year", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_all) # Explicitly print the combined plot




for (state_name in unique_states)
{
  byState <- tele2122_clean %>%
    filter(LST == state_name)

  # Generate and print the plot for the current state
  p <- ggplot(data = byState, aes(x = YEAR, fill = TREATPSYCHOTHRPY)) +
    geom_bar() +
    labs(title = paste("Data for State:", state_name), x = "Year", y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  print(p) # Explicitly print the plot
}
dev.off()
#################################################################################






########################FOCUS PDF########################

pdf("~/GitHub/nmhss/graphs/FOCUS_all_states_data.pdf", width = 11, height = 8.5)
tele2122_clean <- data_start_2014_filter %>%
  filter(!FOCUS %in% c("L","M","-1","-5","-7"))

p_all <- ggplot(data = tele2122_clean, aes(x = YEAR, fill = FOCUS)) +
  geom_bar() +
  labs(title = "Combined Data for All States", x = "Year", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_all) # Explicitly print the combined plot




for (state_name in unique_states)
{
  byState <- tele2122_clean %>%
    filter(LST == state_name)

  # Generate and print the plot for the current state
  p <- ggplot(data = byState, aes(x = YEAR, fill = FOCUS)) +
    geom_bar() +
    labs(title = paste("Data for State:", state_name), x = "Year", y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  print(p) # Explicitly print the plot
}
dev.off()
#################################################################################


#
# ########################LANG_B PDF########################
#
# pdf("~/GitHub/nmhss/graphs/LANG_B_all_states_data.pdf", width = 11, height = 8.5)
# tele2122_clean <- data_start_2014_filter %>%
#   filter(!LANG_B %in% c("L","M","-1","-2","-5","-7"))
#
# p_all <- ggplot(data = tele2122_clean, aes(x = YEAR, fill = LANG_B)) +
#   geom_bar() +
#   labs(title = "Combined Data for All States", x = "Year", y = "Count") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#
# print(p_all) # Explicitly print the combined plot
#
#
#
#
# for (state_name in unique_states)
# {
#   byState <- tele2122_clean %>%
#     filter(LST == state_name)
#
#   # Generate and print the plot for the current state
#   p <- ggplot(data = byState, aes(x = YEAR, fill = LANG_B)) +
#     geom_bar() +
#     labs(title = paste("Data for State:", state_name), x = "Year", y = "Count") +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
#
#   print(p) # Explicitly print the plot
# }
# dev.off()
#################################################################################

########################LANG PDF########################

pdf("~/GitHub/nmhss/graphs/LANG_all_states_data.pdf", width = 11, height = 8.5)
tele2122_clean <- data_start_2014_filter %>%
  filter(!LANG %in% c("L","M","-1","-2","-5","-7"))

p_all <- ggplot(data = tele2122_clean, aes(x = YEAR, fill = LANG)) +
  geom_bar() +
  labs(title = "Combined Data for All States", x = "Year", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_all) # Explicitly print the combined plot




for (state_name in unique_states)
{
  byState <- tele2122_clean %>%
    filter(LST == state_name)

  # Generate and print the plot for the current state
  p <- ggplot(data = byState, aes(x = YEAR, fill = LANG)) +
    geom_bar() +
    labs(title = paste("Data for State:", state_name), x = "Year", y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  print(p) # Explicitly print the plot
}
dev.off()
#################################################################################






########################FACILITY TYPE PDF########################

pdf("~/GitHub/nmhss/graphs/FACILITYTYPE_all_states_data.pdf", width = 11, height = 8.5)
tele2122_clean <- data_start_2014_filter %>%
  filter(!FACILITYTYPE%in% c("L","M","-1","-5","-7"))

p_all <- ggplot(data = tele2122_clean, aes(x = YEAR, fill = FACILITYTYPE)) +
  geom_bar() +
  labs(title = "Combined Data for All States", x = "Year", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_all) # Explicitly print the combined plot




for (state_name in unique_states)
{
  byState <- tele2122_clean %>%
    filter(LST == state_name)

  # Generate and print the plot for the current state
  p <- ggplot(data = byState, aes(x = YEAR, fill = FACILITYTYPE)) +
    geom_bar() +
    labs(title = paste("Data for State:", state_name), x = "Year", y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  print(p) # Explicitly print the plot
}
dev.off()
#################################################################################




######################FACILTYPE PSYCOTHEYRAPY YEAR###############################
pdf("~/GitHub/nmhss/graphs/FACILITYTYPE_PSYCOTHERAPY_all_states_data.pdf", width = 11, height = 8.5)
# Assuming your dataset is named df and is already summarized appropriately
# Plot

tele2122_clean <- data_start_2014_filter %>%
  filter(!FACILITYTYPE%in% c("L","M","-1","-5","-7"))

df_summarized <- tele2122_clean %>%
  group_by(YEAR, FACILITYTYPE, TREATPSYCHOTHRPY) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  filter(!TREATPSYCHOTHRPY%in% c("L","M","-1","-5","-7")) %>%
mutate(TREATPSYCHOTHRPY = as.character(TREATPSYCHOTHRPY), # Convert to character if it's not already
       TREATPSYCHOTHRPY = recode(TREATPSYCHOTHRPY,
                             "0" = 'No treatment',
                             "1" = 'Treatment',),
       TREATPSYCHOTHRPY = factor(TREATPSYCHOTHRPY))

p_all <- ggplot(data = df_summarized, aes(x = YEAR, y = Count, fill = TREATPSYCHOTHRPY)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~FACILITYTYPE) +
  labs(title = "Psychotherapy Treatment by Facility Type Over Years",
       x = "Year",
       y = "Count") +
  theme_minimal()

print(p_all)

dev.off()

#################################################################


table(data_start_2014_filter$FACILITYTYPE,data_start_2014_filter$TREATPSYCHOTHRPY)




table(data_start_2014_filter$FACILITYTYPE,data_start_2014_filter$TREATFAMTHRPY)





# data_all_long <- data_all_years %>%
#   pivot_longer(cols = -c(CASEID, YEAR), names_to = "Category", values_to = "Value") %>%
#   filter(!is.na(Value))
#
#
# ggplot(data_all_years, aes(x = YEAR, fill = MHINTAKE)) +
#   geom_bar(position = "fill") + # Use position = "fill" to show proportions
#   scale_y_continuous(labels = scales::percent_format()) + # Convert y-axis to percentage
#   labs(y = "Percentage", x = "Year", title = "Category Distribution by Year") +
#   theme_minimal()
#
# ggplot(data_all_years, aes(x = YEAR, fill = MHDIAGEVAL)) +
#   geom_bar(position = "fill") + # Use position = "fill" to show proportions
#   scale_y_continuous(labels = scales::percent_format()) + # Convert y-axis to percentage
#   labs(y = "Percentage", x = "Year", title = "Category Distribution by Year") +
#   theme_minimal()
#
# ggplot(data_all_years, aes(x = YEAR, fill = MHREFERRAL)) +
#   geom_bar(position = "fill") + # Use position = "fill" to show proportions
#   scale_y_continuous(labels = scales::percent_format()) + # Convert y-axis to percentage
#   labs(y = "Percentage", x = "Year", title = "Category Distribution by Year") +
#   theme_minimal()
#
# ggplot(data_all_years, aes(x = YEAR, fill = ADMINSERV)) +
#   geom_bar(position = "fill") + # Use position = "fill" to show proportions
#   scale_y_continuous(labels = scales::percent_format()) + # Convert y-axis to percentage
#   labs(y = "Percentage", x = "Year", title = "Category Distribution by Year") +
#   theme_minimal()
#
# ggplot(data_all_years, aes(x = YEAR, fill = SETTINGIP)) +
#   geom_bar(position = "fill") + # Use position = "fill" to show proportions
#   scale_y_continuous(labels = scales::percent_format()) + # Convert y-axis to percentage
#   labs(y = "Percentage", x = "Year", title = "Category Distribution by Year") +
#   theme_minimal()
#
# ggplot(data_all_years, aes(x = YEAR, fill = SETTINGOP)) +
#   geom_bar(position = "fill") + # Use position = "fill" to show proportions
#   scale_y_continuous(labels = scales::percent_format()) + # Convert y-axis to percentage
#   labs(y = "Percentage", x = "Year", title = "Category Distribution by Year") +
#   theme_minimal()
#
# ggplot(data_all_years, aes(x = YEAR, fill = FACILITYTYPE)) +
#   geom_bar(position = "fill") + # Use position = "fill" to show proportions
#   scale_y_continuous(labels = scales::percent_format()) + # Convert y-axis to percentage
#   labs(y = "Percentage", x = "Year", title = "Category Distribution by Year") +
#   theme_minimal()
#
# ggplot(data_all_years, aes(x = YEAR, fill = MHCONSUMER)) +
#   geom_bar(position = "fill") + # Use position = "fill" to show proportions
#   scale_y_continuous(labels = scales::percent_format()) + # Convert y-axis to percentage
#   labs(y = "Percentage", x = "Year", title = "Category Distribution by Year") +
#   theme_minimal()
#
# ggplot(data_all_years, aes(x = YEAR, fill = MHEMGCY)) +
#   geom_bar(position = "fill") + # Use position = "fill" to show proportions
#   scale_y_continuous(labels = scales::percent_format()) + # Convert y-axis to percentage
#   labs(y = "Percentage", x = "Year", title = "Category Distribution by Year") +
#   theme_minimal()
#
# ggplot(data_all_years, aes(x = YEAR, fill = SED)) +
#   geom_bar(position = "fill") + # Use position = "fill" to show proportions
#   scale_y_continuous(labels = scales::percent_format()) + # Convert y-axis to percentage
#   labs(y = "Percentage", x = "Year", title = "Category Distribution by Year") +
#   theme_minimal()
#
# ggplot(data_all_years, aes(x = YEAR, fill = SPMI)) +
#   geom_bar(position = "fill") + # Use position = "fill" to show proportions
#   scale_y_continuous(labels = scales::percent_format()) + # Convert y-axis to percentage
#   labs(y = "Percentage", x = "Year", title = "Category Distribution by Year") +
#   theme_minimal()
#
# ggplot(data_all_years, aes(x = YEAR, fill = ALZHDEMENTIA)) +
#   geom_bar(position = "fill") + # Use position = "fill" to show proportions
#   scale_y_continuous(labels = scales::percent_format()) + # Convert y-axis to percentage
#   labs(y = "Percentage", x = "Year", title = "Category Distribution by Year") +
#   theme_minimal()
#
# ggplot(data_all_years, aes(x = YEAR, fill = POSTTRAUM)) +
#   geom_bar(position = "fill") + # Use position = "fill" to show proportions
#   scale_y_continuous(labels = scales::percent_format()) + # Convert y-axis to percentage
#   labs(y = "Percentage", x = "Year", title = "Category Distribution by Year") +
#   theme_minimal()
#
# ggplot(data_all_years, aes(x = YEAR, fill = TRAUMATICBRAIN)) +
#   geom_bar(position = "fill") + # Use position = "fill" to show proportions
#   scale_y_continuous(labels = scales::percent_format()) + # Convert y-axis to percentage
#   labs(y = "Percentage", x = "Year", title = "Category Distribution by Year") +
#   theme_minimal()
#
# ggplot(data_all_years, aes(x = YEAR, fill = FEESCALE)) +
#   geom_bar(position = "fill") + # Use position = "fill" to show proportions
#   scale_y_continuous(labels = scales::percent_format()) + # Convert y-axis to percentage
#   labs(y = "Percentage", x = "Year", title = "Category Distribution by Year") +
#   theme_minimal()





# vars_not_in_small <- setdiff(names(data_start_2014_filter), names(data_all_years))
# for (var in vars_not_in_small)
# {
#   browser()
#   p<- ggplot(data_start_2014_filter, aes(x = YEAR, fill = !!rlang::sym(var))) +
#     geom_bar(position = "fill") + # Use position = "fill" to show proportions
#     scale_y_continuous(labels = scales::percent_format()) + # Convert y-axis to percentage
#     labs(y = "Percentage", x = "Year", title = "Category Distribution by Year") +
#     theme_minimal()
#   print(p)
# }




# byState1 <- data_start_2014_filter %>%
#   arrange(LST) %>%  # Sort by state alphabetically
#   distinct(LST, .keep_all = TRUE) %>%  # Ensures states are unique before slicing
#   slice(5) %>%  # Selects the first 5 states
#   inner_join(data_start_2014_filter, by = "LST")
#
#
# byState2 <- data_start_2014_filter %>%
#   arrange(LST) %>%  # Sort by state alphabetically
#   distinct(LST, .keep_all = TRUE) %>%  # Ensures states are unique before slicing
#   slice(6:10) %>%  # Selects the first 5 states
#   inner_join(data_start_2014_filter, by = "LST")
