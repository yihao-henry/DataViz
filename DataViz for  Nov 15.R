library(tidyverse)
setwd("~/Desktop")
US_soybean_export <- read_csv("ExportSalesDataByCommodity (2).csv")
US_soybean_export <- select(US_soybean_export, -X4)
US_soybean_export <- US_soybean_export %>% mutate(Year = str_sub(Date, 7, 10)) 



Export_2016 <- US_soybean_export %>% filter(Year == 2016) 
Export_2016_by_country <- Export_2016 %>% group_by(Country) %>% summarise(total_export_2016 = sum(`Weekly Exports`)) %>% filter(Country != "GRAND TOTAL")
Export_2016_by_country <- Export_2016_by_country %>% filter(Country != "GRAND TOTAL" & Country != "KNOWN" & Country != "UNKNOWN") %>% mutate(rank_2016 = rank(-total_export_2016)) %>% filter(rank_2016 <= 10)
Export_2017_by_country <- US_soybean_export %>% filter(Year == 2017) %>% group_by(Country) %>% summarise(total_export_2017 = sum(`Weekly Exports`)) %>% filter(Country != "GRAND TOTAL" & Country != "KNOWN" & Country != "UNKNOWN") %>% mutate(rank_2017 = rank(-total_export_2017)) %>% filter(rank_2017 <= 10)
Export_2018_by_country <- US_soybean_export %>% filter(Year == 2018) %>% group_by(Country) %>% summarise(total_export_2018 = sum(`Weekly Exports`)) %>% filter(Country != "GRAND TOTAL" & Country != "KNOWN" & Country != "UNKNOWN") %>% mutate(rank_2018 = rank(-total_export_2018)) %>% filter(rank_2018 <= 10)

US_2016_export_graph <- ggplot(data = Export_2016_by_country, aes(x = Country, y = total_export_2016)) +
  geom_bar(stat = "identity") +
  labs( title = "China Was the Top Buyer of U.S. Soybeans in 2016", subtitle = "In 2016, China was playing an crucial role on US agricultural export. This second-largest economy imported the most soybeans.", caption = " Source: United States Department of Agriculture, Foreign Agricultural Service", x = "Country", y = "Export Soybean sales Measured in Tons") +
  coord_flip() +
  geom_text(aes(label= total_export_2016), position=position_dodge(width=0.1), vjust=-0.1, hjust=0) +
  theme(panel.background = element_rect(fill = "white"))
US_2016_export_graph 

US_2017_export_graph <- ggplot(data = Export_2017_by_country, aes(x = Country, y = total_export_2017)) +
  geom_bar(stat = "identity") +
  labs(title = "China Remained the Top Buy of U.S. Soybeans in 2017", subtitle = "Comparing with the amount of U.S. soybeans imported by China in 2016, China's imports of U.S. soybeans fell 14.5% in 2017, but it still accounted for 58% of the US soybean export.", caption = " Source: United States Department of Agriculture, Foreign Agricultural Service", x = "Country", y = "Export Soybean sales Measured in Tons") +
  coord_flip() +
  geom_text(aes(label= total_export_2017), position=position_dodge(width=0.1), vjust=-0.1, hjust=0) +
  theme(panel.background = element_rect(fill = "white"))
US_2017_export_graph

US_2018_export_graph <- ggplot(data = Export_2018_by_country, aes(x = Country, y = total_export_2018)) +
  geom_bar(stat = "identity") +
  labs(title = "In 2018, China Lost Its Position of the Largest Importer of US Soybeans", subtitle = "A significant drop in soybean purchases by China is at least partially caused by the trade war. From the U.S. perspective, it was also looking for other buyers in the international community to compensate for China's reduced purchases.", caption = " Source: United States Department of Agriculture, Foreign Agricultural Service", x = "Country", y = "Export Soybean sales Measured in Tons") +
  coord_flip() +
  geom_text(aes(label= total_export_2018), position=position_dodge(width=0.1), vjust=-0.1, hjust=0) +
  theme(panel.background = element_rect(fill = "white"))
US_2018_export_graph


print("Intro: As the US-China trade war is still raging on with constantly instable attitudes exposed by Beijing and Washington, even nowadays neither of the players would like to show a honest concession. It is necessary to figure out why this trade war becomes very intense. This project is trying to evaluate how effective is the trade war. Could Donald Trump gain what he wants throught such fierce tactic? Since the trade of soybeans has been mentioned for several times throughout US-China bilateral negotiations, this project uses the market performance of US soybean export to assess the effectiveness of this trade war. This research project delve into such question by two point of views: US soybean exports and China's soybean imports. From the US perspective, the trade war does not trigger the decline of the soybean export to China. So far, the project has not checked the soybean imports to China. If the Chinese purchased soybeans from sources other than the US to compensate the lack of enough soybean to purchase and did not get worse off, stimulating more soybean import, as one of the goals Donald Trump would like to achieve by the trade war, would fail. Therefore, the trade war would not be completely effective.")






