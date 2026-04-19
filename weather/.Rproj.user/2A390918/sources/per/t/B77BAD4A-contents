library (tidyverse)
w <- read.csv("weather-statistics-daily.csv",sep=';')
View(w)
weath2 <- select(w, 'Country' = 'country', 'Precipitation' = 'precipitation_sum')
view(weath2)
weath2 <- group_by(weath2, Country)
weath2 <- summarise(weath2, max_Precipitation = max(Precipitation))
view(weath2)
ggplot(data = weath2) + 
  geom_col(mapping = aes(x = Country, y = max_Precipitation), fill = "blue")
#end to ch3

# ch4
weath3 <- filter(w, wind_gusts_10m_max > 50)
weath3 <- select(weath3, 'Country' = 'country', 'Wind Gusts' = 'wind_gusts_10m_max') %>% arrange(desc(`Wind Gusts`))
view(weath3)
#end of ch4


#ch5-6

weath4 <- filter(w, time == '2025-01-29', apparent_temperature_max > 35)
view(weath4)

ggplot(data = weath4) + geom_bar(mapping = aes(x = wind_speed_10m_max))

view(count(weath4, apparent_temperature_max))

w %>% select(country, temperature_2m_max) %>% filter(temperature_2m_max > 30) %>%
  ggplot() + geom_histogram(mapping = aes(x = temperature_2m_max), binwidth = 2) 

w %>% count(cut_width(temperature_2m_max, 2))

w %>% filter(apparent_temperature_max > 38) %>% group_by(country) %>%
  ggplot(mapping = aes(x = country, y = apparent_temperature_max)) + geom_boxplot()

w %>% filter(apparent_temperature_max > 38) %>% group_by(country) %>%
ggplot() + geom_count(mapping = aes(x = country, y = apparent_temperature_max))

w %>% filter(apparent_temperature_max > 38) %>% group_by(country) %>%
  ggplot() + geom_bin2d(mapping = aes(x = country, y = apparent_temperature_max), fill = "blue")

mean(weath4$apparent_temperature_max)
median(weath4$apparent_temperature_max)
summary(weath4$apparent_temperature_max)


#CH7
Wch7 <-  select(w,time, country, temperature_2m_max, temperature_2m_min, precipitation_sum,sunshine_duration)
Wch7$time <- as.Date(Wch7$time)
grp <- group_by(Wch7,time)

#Scatterplot
ggplot(data=Wch7, aes(x=time, y=temperature_2m_max)) + geom_point()

#Regression Line

ggplot(data=Wch7, aes(x=time, y=sunshine_duration)) + geom_point() + geom_smooth(method=lm, se=FALSE)
ggplot(data=Wch7, aes(x=time, y=sunshine_duration)) + geom_point() + geom_smooth(method=loess, se=TRUE)

wch7short <- Wch7[startsWith(Wch7$country, "A"), ]


ggplot(data=wch7short, aes(x=time, y=precipitation_sum, colour=country)) + geom_point(aes(colour = country)) + stat_smooth(method=lm, se=FALSE)


ggplot(data=Wch7, aes(x=time, y=log(temperature_2m_max))) + geom_point() + geom_smooth(method=loess, se=TRUE) + geom_text(aes(label=country), size=3, check_overlap = TRUE) 


#5
ggplot(data=wch7short, aes(x=time, y=temperature_2m_max, color=country)) + geom_point() + theme(legend.position="bottom")
ggplot(data=wch7short, aes(x=time, y=temperature_2m_max, color=country)) + geom_point() + theme(legend.position="bottom") + guides(color=guide_legend(nrow=2, override.aes=list(size=4)))


#6
ggplot(data=wch7short, aes(x=time, y=temperature_2m_max)) + geom_point() + facet_wrap(~country) + geom_smooth(method=loess, se=TRUE)


#7
ggplot(data=wch7short, aes(x=time, y=temperature_2m_max)) + geom_point() + facet_wrap(~country) + geom_smooth(method=loess, se=TRUE) + theme_dark()


#8
ggplot(data=wch7short, aes(x=country, y=temperature_2m_max, fill=country)) + geom_bar(stat="identity")



#9
ggplot(data=wch7short, aes(x=country, y=sunshine_duration)) + geom_violin()

ggplot(data=wch7short, aes(x=country, y=sunshine_duration)) + geom_violin() + geom_jitter(height=0, width=0.1)

ggplot(data=wch7short, aes(x=country, y=sunshine_duration)) + geom_violin() + geom_jitter(height=0, width=0.1) + stat_summary(fun=mean, geom="point", size=2, color="red")

ggplot(data=wch7short, aes(x=country, y=sunshine_duration)) + geom_violin() + geom_boxplot(width=0.1)



#10



ggplot(data=wch7short, aes(x=temperature_2m_max)) + geom_density()

ggplot(data=wch7short, aes(x=time, y=temperature_2m_max)) + geom_point() + geom_density_2d()

ggplot(data=wch7short, aes(x=time, y=temperature_2m_max)) + geom_density_2d() + stat_density_2d(geom="raster", aes(fill=..density..), contour=FALSE)