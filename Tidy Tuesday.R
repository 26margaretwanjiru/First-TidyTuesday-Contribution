#Getting the data

wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

#Loading the packages

library(tidyverse)
library(tidytext)
library(ggrepel)
library(gganimate)
library(gridExtra)
library(magick)

str(wine_ratings)
summary(wine_ratings)

wine_ratings<- wine_ratings[ , -1]

summary(wine_ratings)
attach(wine_ratings)

sum(is.na(wine_ratings))
#204752 missing values


#SUMMARY
#variable	class	description
#country	character	Country of origin
#description	character	Flavors and taste profile as written by reviewer
#designation	character	The vineyard within the winery where the grapes that made the wine are from
#points	double	The number of points WineEnthusiast rated the wine on a scale of 1-100 (though they say they only post reviews for wines that score >=80)
#price	double	The cost for a bottle of the wine
#province	character	The province or state that the wine is from
#region_1	character	The wine growing area in a province or state (ie Napa)
#taster_name	character	The taster/reviewer
#title	character	The title of the wine review, which often contains the vintage (year)
#variety	character	Grape type
#winery	character	The winery that made the wine


wine_no_missing <- wine_ratings %>% 
  drop_na(points, price)

# Research shows these are the best ten grape varieties

best_10_varieties <- c("Cabernet Sauvignon", "Merlot", "Airen", "Tempranillo", 
                    "Chardonnay", "Syrah", "Garnacha", "Sauvignon Blanc", "Trebbiano", "Pinot Noir")


#Filtering only the ten best grape varieties
#Selecting only 5 variables

select_wine <- wine_no_missing %>% 
  filter(variety %in% best_10_varieties ) %>% 
  select(country, description, points, price, variety)

##Looking for most common words

wine_words <- select_wine %>% 
    unnest_tokens(output = word, input = description) 

#Removing stop words

wine_words  <- wine_words  %>%
    anti_join(stop_words)

#Word counts

wine_word_counts <- wine_words  %>% 
  count(word, sort = TRUE) 

#Filtering common description verbs

wine_word_counts <- wine_words  %>% 
  count(word, sort = TRUE) %>%
  filter(word %in% c("sweet", "acidity", "spice", "finish",
                     "fruit", "palate", "cherry", "oak", "black", "tannins", "ripe",
                     "red", "rich", "plum", "vanilla", "soft", "light", "apple", 
                     "blackberry", "fresh", "berry", "crisp", "dark", "green" ))
#Plotting counts

wine_word_counts <- wine_word_counts %>%
  mutate(word = reorder(word, n))

ggplot(wine_word_counts, aes(word, n)) +
  geom_col(fill = "#10ff00")+
  coord_flip()+
  labs(x = "Word", y= "Count", 
       title = "Frequent Words in Wine Description \n")+
  geom_text(aes(label = n), hjust = 1.2, 
            color = "white", fontface = "bold")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(face = "bold", color = "darkblue", size = 12),
        axis.title.y = element_text(face = "bold", color = "darkblue",
                                    size = 12)) 

###Filtering by the top 5 best grape varieties

Cabernet_Sauvignon <- wine_words %>%
  filter(variety == "Cabernet Sauvignon")


Cabernet_Sauvignon_word_counts <- Cabernet_Sauvignon %>% 
  count(word, sort = TRUE) 

#Filtering common description verbs

Cabernet_Sauvignon_word_counts <- Cabernet_Sauvignon_word_counts  %>%
  filter(word %in% c("black", "tannins", "fruit",
                     "cherry", "finish", "oak", "blackberry", "palate", "cassis", "chocolate"))
#Plotting Cabernet_Sauvignon_word_counts

Cabernet_Sauvignon_word_counts <- Cabernet_Sauvignon_word_counts %>%
  mutate(word = reorder(word, n))

Cabernet_Sauvignon_plot <- ggplot(Cabernet_Sauvignon_word_counts, aes(word, n)) +
  geom_col(fill = "#10ff00")+
  coord_flip()+
  labs(x = "Word", y= "Count", 
       title = "Frequent Words in Cabernet Sauvignon Variety \n",
       subtitle = "Top 10 frequent description words")+
  geom_text(aes(label = n), hjust = 1.2, 
            color = "white", fontface = "bold")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(face = "bold", color = "darkblue", size = 12),
        axis.title.y = element_text(face = "bold", color = "darkblue",
                                    size = 12)) 

Cabernet_Sauvignon_plot

## Merlot

Merlot <- wine_words %>%
  filter(variety == "Merlot")


Merlot_word_counts <- Merlot %>% 
  count(word, sort = TRUE) 

#Filtering common description verbs

Merlot_word_counts <- Merlot_word_counts %>%
  filter(word %in% c("fruit",
                     "cherry", "tannins", "black", "finish", "red", "plum", "palate", "soft", "spice"))
#Plotting Cabernet_Sauvignon_word_counts

Merlot_word_counts <- Merlot_word_counts  %>%
  mutate(word = reorder(word, n))

Merlot_plot <- ggplot(Merlot_word_counts, aes(word, n)) +
  geom_col(fill = "#ed5062")+
  coord_flip()+
  labs(x = "Word", y= "Count", 
       title = "Frequent Words in Merlot Variety \n",
       subtitle = "Top 10 frequent description words")+
  geom_text(aes(label = n), hjust = 1.2, 
            color = "white", fontface = "bold")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(face = "bold", color = "darkblue", size = 12),
        axis.title.y = element_text(face = "bold", color = "darkblue",
                                    size = 12)) 

Merlot_plot

##Third best
#Pinot Noir

Pinot_Noir <- wine_words %>%
  filter(variety == "Pinot Noir")


Pinot_Noir_word_counts <- Pinot_Noir %>% 
  count(word, sort = TRUE) 

#Filtering common description verbs

Pinot_Noir_word_counts <- Pinot_Noir_word_counts %>%
  filter(word %in% c("fruit",
                     "cherry", "acidity", "red", "tannins", "finish", 
                     "black", "palate", "raspberry", "ripe"))
#Plotting Cabernet_Sauvignon_word_counts

Pinot_Noir_word_counts<- Pinot_Noir_word_counts  %>%
  mutate(word = reorder(word, n))

Pinot_Noir_plot <- ggplot(Pinot_Noir_word_counts, aes(word, n)) +
  geom_col(fill = "#70c1ff")+
  coord_flip()+
  labs(x = "Word", y= "Count", 
       title = "Frequent Words in Pinot Noir Variety \n",
       subtitle = "Top 10 frequent description words")+
  geom_text(aes(label = n), hjust = 1.2, 
            color = "white", fontface = "bold")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(face = "bold", color = "darkblue", size = 12),
        axis.title.y = element_text(face = "bold", color = "darkblue",
                                    size = 12))+
  labs(caption  = "Data Source: Kaggle")

Pinot_Noir_plot

#Combining the three plots

grid.arrange(Cabernet_Sauvignon_plot, Merlot_plot, 
             Pinot_Noir_plot, ncol=1, nrow =3)

