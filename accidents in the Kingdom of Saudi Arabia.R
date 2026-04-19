# load required library 
library(tidyverse)

#read dataset
data <- read_csv2("saudi-arabia-traffic-accidents-and-casualties-injured-dead-2008.csv")
View (data)

#select  required colmun
df<- select(data,"Year","Region" ,"Indicator" ,"Value")


##case 1:Number of Accidents by region 
region_summary <- df %>%
  group_by(Region) %>%
  summarise(Total_Accidents = Value/1000) 

# Bar chart
ggplot(data = region_summary) + geom_bar(mapping = aes(x = Region, y = Total_Accidents, fill = Region), stat = "identity") +
  labs( title = "Number of Accidents by Region", x = "Region", y = "Number of Accidents / 1000" )

ggplot(data = region_summary) + geom_bar(mapping = aes(x = reorder(Region, -Total_Accidents), y = Total_Accidents, fill = Region), stat = "identity") + labs( title = "Number of Accidents by Region", x = "Region", y = "Number of Accidents / 1000" ) 




##Case 2: Comparison of Accidents ,Deaths and Injuries by Year
summary_data <- df %>%
  group_by(Year, Indicator) %>%
  summarise(Total = sum(Value, na.rm = TRUE)) 
 
# Plot using geom_count
ggplot(summary_data, aes(x = Year, y = Indicator, size = Total)) +
  geom_count(alpha = 0.7, color = "steelblue") +
  scale_size_area(max_size = 15) +
 
  labs(title = "Comparison of  Accidents ,Deaths and Injuries by Year",
       x = "Year", y = "Type",
       size = "Total Count")



##Case 3: Number of Deaths by Region (2016–2019)
deaths_data <- data %>%
  filter(Indicator == "No. of Casualties - Dead") %>%
  group_by(Year, Region) %>%
  summarise(Total_Deaths = sum(Value, na.rm = TRUE))

#Polt use Plotting Categories and regression line
ggplot(data=deaths_data, aes(x=Year, y=Total_Deaths, colour=Region)) +
  geom_point(aes(colour = Region)) + stat_smooth(method=lm, se=FALSE)


#Case 4:
df_filtered <- df %>%
  mutate(
    Category = ifelse(Indicator == "No. of Casualties - Injured", "Injured",
                      ifelse(Indicator == "No. of Casualties - Dead", "Dead", NA)),
    Value = as.numeric(Value)
  ) %>%
  filter(!is.na(Category))# Keep only rows with Injured or Dead

# Summarize total Injured and Dead per Region
df_summary <- df_filtered %>%
  group_by(Region, Category) %>%
  summarise(Total = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Category, values_from = Total)

df_summary<-df_summary %>%  filter(Region != "Total")



# Plot Injured vs Dead per Region
ggplot(df_summary, aes(x = Injured, y = Dead, color = Region, label = Region)) +
  geom_point(size = 4) +
  geom_text(nudge_y = 200, size = 3) +
  labs(
    title = "Traffic Injuries vs Deaths by Region ",
    x = "Total Injured",
    y = "Total Dead"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  guides(color = "none")  # Hide legend because we label directly



 