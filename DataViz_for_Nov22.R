library(tidyverse)
install.packages("readxl")
library("readxl")
setwd("~/Desktop")
soybean2011_2015 <- read_excel("ExportSalesDataByCommodity (4) copy.xls")
soybean2015_2018 <- read_excel("ExportSalesDataByCommodity (4).xls")

soybean2011_2015 <- na.omit(soybean2011_2015)
soybean2015_2018 <- na.omit(soybean2015_2018)

soybean2011_2018 <- soybean2011_2015 %>% rbind(soybean2015_2018)
soybean2011_2018 <- soybean2011_2018 %>% mutate(Year = str_sub(Date, 1, 4)) 

Export_2011 <- soybean2011_2018 %>% filter(Year == 2011)
total_2011 <- sum(Export_2011$`Weekly Exports`)

Export_2012 <- soybean2011_2018 %>% filter(Year == 2012)
total_2012 <- sum(Export_2012$`Weekly Exports`)

Export_2013 <- soybean2011_2018 %>% filter(Year == 2013)
total_2013 <- sum(Export_2013$`Weekly Exports`)

Export_2014 <- soybean2011_2018 %>% filter(Year == 2014)
total_2014 <- sum(Export_2014$`Weekly Exports`)

Export_2015 <- soybean2011_2018 %>% filter(Year == 2015)
total_2015 <- sum(Export_2015$`Weekly Exports`)

Export_2016 <- soybean2011_2018 %>% filter(Year == 2016)
total_2016 <- sum(Export_2016$`Weekly Exports`)

Export_2017 <- soybean2011_2018 %>% filter(Year == 2017)
total_2017 <- sum(Export_2017$`Weekly Exports`)

Export_2018 <- soybean2011_2018 %>% filter(Year == 2018)
total_2018 <- sum(Export_2018$`Weekly Exports`)

Total_export_to_China <- c(total_2011, total_2012, total_2013,
                           total_2014, total_2015, total_2016,
                           total_2017, total_2018)
Year <- c(2011:2018)
Export_df <- data.frame(Year, Total_export_to_China)

Soybean_export_graph <- ggplot(data = Export_df, aes(x = Year, y = Total_export_to_China)) +
  geom_bar(stat = "identity") +
  labs(title = "...", subtitle = "...", caption = " Source: United States Department of Agriculture, Foreign Agricultural Service", 
       x = "Year", y = "Export Soybean sales Measured in Tons") +
  geom_text(aes(label = Total_export_to_China), size = 3, vjust = 1.5, color = "white") +
  theme_classic()
Soybean_export_graph

ggplot(data = soybean2011_2018) +
  geom_point(mapping = aes(
    x = `Weekly Exports`,
    y = `NMY Net Sales`,
    color = Year
  )) +
  labs(
    title = "In 2018, The Profibility of Soybean Exports from the United States to China Fell Significantly",
    caption = " Source: United States Department of Agriculture, Foreign Agricultural Service"
  ) +
  facet_wrap(~Year)

## China's Side
China_Soybeans_Import <- c(52640000, 58380000, 63380000, 71403100, 81691900, 83910000, 95530000, 88030000)
Year <- c(2011:2018)  
Import_df <- data.frame(Year, China_Soybeans_Import)

Soybean_import_graph <- ggplot(data = Import_df, aes(x = Year, y = China_Soybeans_Import)) +
  geom_bar(stat = "identity") +
  labs(title = "...", subtitle = "...", caption = " Source: National Bureau of Statistics of China", 
       x = "Year", y = "Import Soybean sales Measured in Tons") +
  geom_text(aes(label = Total_export_to_China), size = 3, vjust = 1.5, color = "white") +
  theme_classic()
Soybean_import_graph 


Trade_df <- data.frame(Year, China_Soybeans_Import, Total_export_to_China)
ggplot(Trade_df,aes(x = Year)) + 
  geom_bar(aes(y=China_Soybeans_Import),stat="identity",position="dodge", fill="#e86b6b") +
  geom_bar(position="dodge") +   
  geom_bar(aes(y=Total_export_to_China),stat="identity",position="dodge", fill = "#82c1ed") +
  labs(title = "A Steep Decrease in the US Soybeans Export accounting for China's Soybeans Import", subtitle = "...", 
       caption = " Source: United States Department of Agriculture, Foreign Agricultural Service;
       National Bureau of Statistics of China", 
       x = "Year", y = "Soybean sales Measured in Tons") +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) 

## Where does China import Soybeans from?
# In 2017
Import_origin_2017 <- data.frame(
  Country = c("Brazil", "Argentina", "Uruguay", "United States", "Russia", "Ukraine", "Hong Kong", 
            "Kazakhstan", "Other Asia", "Ethiopia", "Malawi", "Mozambique", "Canada"),
  Percentage = c(56, 6.6, 1.4, 34, 0.38, 0.016, 0.01, 0.0084, 0.0000087, 0.017, 0.00055, 0.00025, 2.1))
bp<- ggplot(Import_origin_2017, aes(x="", y=Percentage, fill=Country))+
  geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y", start=0/100)
pie
  




