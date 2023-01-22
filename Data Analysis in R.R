rm(list = ls())
library(dplyr,xtable,ggplot2)

df_wb = read.csv("/Users/jppalmab/Desktop/NCI_National College of Ireland/Msc Data Analytics/01 Term/Database and Analytics Programming/00 Project/data/08 df_gpd_current.csv")
df = read.csv("/Users/jppalmab/Desktop/NCI_National College of Ireland/Msc Data Analytics/01 Term/Database and Analytics Programming/00 Project/data/07 df_earthquakes_clean.csv")

#View(df %>% count(Country)) 

df_plus15 = subset(df,Country=='Afghanistan'|Country=='Australia'|
                     Country=='Papua New Guinea'|Country=='Pakistan'|
                     Country=='Greece'|Country=='Russia'|Country=='Guinea'|
                     Country=='New Zealand'|Country=='Mexico'|
                     Country=='Philippines'|Country=='Italy'|Country=='Peru'|
                     Country=='Chile'|Country=='India'|Country=='Iran'|
                     Country=='Turkey'|Country=='Japan'|
                     Country=='United States of America'|Country=='China'|
                     Country=='Indonesia')

str(df)
cor(df$Mag, df$Deaths, use="complete.obs")
cor(df_plus15$Mag, df_plus15$Deaths, use="complete.obs")


xtable(df_plus15 %>% group_by(Country) %>% 
         summarise(Correlation = cor(Mag,Deaths, use="complete.obs"))%>%
         arrange(desc(Correlation)),
       caption = "Correlation between Magnitude of the earthquake and Deaths",
       label='cor_tab', digits = 2)


# AVERAGE DEATHS BY COUNTRY
xtable(df_plus15 %>% group_by(Country) %>% 
         summarise(mean = mean(Deaths, na.rm=T)) %>%
         arrange(desc(mean)),
       caption = "Average of Deaths by Country",
       label = "avg_deaths",
       digits = 0)


# Top 10 earthquakes in the world
xtable(df[ which(df$Mag >= 9),11:14],
       caption = "Top 10 Earthquakes with Magnitude Over 9.0",
       label = "top_10",
       digits = 1)

################################################################################
################################################################################
################################################################################
#########################         CHILE            #############################
################################################################################
################################################################################
################################################################################

#Chile

df_wb_cl = subset(df_wb, country == 'Chile')
df_wb_cl$Year = as.numeric(substr(df_wb_cl$date, 1, 4)) 
df_wb_cl$Country = df_wb_cl$country 
df_wb_cl = select(df_wb_cl, c(Country,Year, GDP_PCAP_CD))

df_cl = subset(df_plus15, Year >= 1960 & Country == 'Chile'  , select = Country: Deaths)

row.names(df_cl)= NULL
df_cl = df_cl %>% group_by(Country,Year) %>% summarise(Mag = max(Mag, na.rm = T), Deaths= sum(Deaths, na.rm = T))


joined_cl <- merge(df_cl, df_wb_cl, by.x = c("Country", "Year"), 
                   by.y = c("Country", "Year"), all.x = TRUE, all.y = TRUE)


#write.csv(joined_df,"/Users/jppalmab/Desktop/NCI_National College of Ireland/Msc Data Analytics/01 Term/Database and Analytics Programming/00 Project/data/10 df_joined.csv")

joined_cl[is.na(joined_cl)] = 0

#############################################################

# Value used to transform the data
coeff <- 10
library(hrbrthemes)

# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)
ID = 1960:2020
  
ggplot(joined_cl, aes(x=Year)) +
  
  geom_bar(aes(y=Mag*1000), stat="identity", 
           size=0.1, fill=temperatureColor, 
           color="black", alpha=.6) + 
  geom_line( aes(y=GDP_PCAP_CD), size=1) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "GDP Per capita Current US$",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./1000, name="Magnitude")
  ) + 
  theme_ipsum() +
  theme(
    axis.text.x = element_text(angle=0, size=12),
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
  ) +
  ggtitle("Chile")


################################################################################
################################################################################
################################################################################
#########################         INDONESIA        #############################
################################################################################
################################################################################
################################################################################

#INDONESIA

df_wb_ind = subset(df_wb, country == 'Indonesia')
#df_wb_c = subset(df_wb, country == c('Chile', 'Indonesia', 'Japan'))
df_wb_ind$Year = as.numeric(substr(df_wb_ind$date, 1, 4)) 
df_wb_ind$Country = df_wb_ind$country 
df_wb_ind = select(df_wb_ind, c(Country,Year, GDP_PCAP_CD))


df_ind = subset(df_plus15, Year > 1966 & Country == 'Indonesia', select = Country: Deaths)

row.names(df_ind)= NULL

df_ind = df_ind %>% group_by(Country,Year) %>% summarise(Mag = max(Mag, na.rm = T), Deaths= sum(Deaths, na.rm = T))



joined_df_ind <- merge(df_ind, df_wb_ind, by.x = c("Country", "Year"), 
                   by.y = c("Country", "Year"), all.x = TRUE, all.y = TRUE)

joined_df_ind = joined_df_ind[-c(1:7,62),]

joined_df_ind[is.na(joined_df_ind)] = 0

#############################################################

# Value used to transform the data
coeff <- 10
library(hrbrthemes)

# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)
#ID = 1960:2020

ggplot(joined_df_ind, aes(x=Year)) +
  
  geom_bar(aes(y=Mag*1000), stat="identity", 
           size=0.1, fill=temperatureColor, 
           color="black", alpha=.6) + 
  geom_line( aes(y=GDP_PCAP_CD), size=1) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "GDP Per capita Current US$",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./1000, name="Magnitude")
  ) + 
  theme_ipsum() +
  theme(
    axis.text.x = element_text(angle=0, size=12),
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
  ) +
  ggtitle("Indonesia")


View(df%>% group_by(Country) %>% subset(Year > 1960) %>% summarise(max_t = max(Mag, na.rm = T)))

################################################################################
################################################################################
################################################################################
#########################         MEXICO       #############################
################################################################################
################################################################################
################################################################################

#mexico

df_wb_mx = subset(df_wb, country == 'Mexico')

df_wb_mx$Year = as.numeric(substr(df_wb_mx$date, 1, 4)) 
df_wb_mx$Country = df_wb_mx$country 
df_wb_mx = select(df_wb_mx, c(Country,Year, GDP_PCAP_CD))


df_mx = subset(df_plus15, Year > 1960 & Country == 'Mexico', select = Country: Deaths)

row.names(df_mx)= NULL

df_mx = df_mx %>% group_by(Country,Year) %>% summarise(Mag = max(Mag, na.rm = T), Deaths= sum(Deaths, na.rm = T))



joined_df_mx <- merge(df_mx, df_wb_mx, by.x = c("Country", "Year"), 
                       by.y = c("Country", "Year"), all.x = TRUE, all.y = TRUE)

joined_df_mx = joined_df_mx[-62,]

joined_df_mx[is.na(joined_df_mx)] = 0

#############################################################

# Value used to transform the data
coeff <- 10
library(hrbrthemes)

# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)
#ID = 1960:2020

ggplot(joined_df_mx, aes(x=Year)) +
  
  geom_bar(aes(y=Mag*1000), stat="identity", 
           size=0.1, fill=temperatureColor, 
           color="black", alpha=.6) + 
  geom_line( aes(y=GDP_PCAP_CD), size=1) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "GDP Per capita Current US$",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./1000, name="Magnitude")
  ) + 
  theme_ipsum() +
  theme(
    axis.text.x = element_text(angle=0, size=12),
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
  ) +
  ggtitle("Mexico")
