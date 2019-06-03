# First-TidyTuesday-Contribution

#Getting the data

wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

#Loading the required packages

library(tidyverse)
library(tidytext)
library(ggrepel)
library(gganimate)
library(gridExtra)
library(magick)


#Structure of the data

str(wine_ratings)
summary(wine_ratings)

#The first column has serial numbers
#This is irrevelant based on my goal of the analysis
#Removing the first column

wine_ratings<- wine_ratings[ , -1]

attach(wine_ratings)


#Are there missing values?

sum(is.na(wine_ratings))
#204752 missing values

#Given the large size of the data set, there is no guilty feeling omitting the missing values :)

wine_no_missing <- wine_ratings %>% 
  drop_na(points, price)

#Research shows these are the best ten grape varieties

best_10_varieties <- c("Cabernet Sauvignon", "Merlot", "Airen", "Tempranillo", 
                    "Chardonnay", "Syrah", "Garnacha", "Sauvignon Blanc", "Trebbiano", "Pinot Noir")


#Filtering only the ten best grape varieties
#Selecting only 5 variables

select_wine <- wine_no_missing %>% 
  filter(variety %in% best_10_varieties ) %>% 
  select(country, description, points, price, variety)

#Looking for the most common words used to describe top 10 grape varieties

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

#Filtering by the top 3 best grape varieties
#Best --> Cabernet Sauvignon Grape Variety

Cabernet_Sauvignon <- wine_words %>%
  filter(variety == "Cabernet Sauvignon")


Cabernet_Sauvignon_word_counts <- Cabernet_Sauvignon %>% 
  count(word, sort = TRUE) 

#Filtering top 10 common description verbs

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

#2nd Best --> Merlot Grape Variety

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

#Third best--> Pinot Noir Grape Variety

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
