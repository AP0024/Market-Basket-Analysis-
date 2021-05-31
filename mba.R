library("tidyverse")
library("arules")
library(arulesViz)
###################Read_Dataset################

movie <- read.csv("https://assets.datacamp.com/production/repositories/5023/datasets/875c0537a382911110b3126fa5db30c4464b5d93/Movie_subset.csv")


##########EDA################3

# glimpse

movie %>%
  glimpse()


##########Describe about the data like missing columns discrete values,continous values########



movie %>% introduce()


#####Missing Values ----#
movie %>%
  is.na()

##########Count of movies Watched by the same user########

movie %>% head(5)

movie %>% 
  group_by(userId)%>% 
  count(userId) %>% arrange(desc(n)) %>% head(10) -> user_movie_watch

user_movie_watch %>%
  ggplot(aes(x = reorder(as.factor(userId), -n),y= n,fill = as.factor(userId)))+geom_col()+guides(fill=guide_legend(title="UserId"))+labs(
    x ="user_id",
    y= "Count",
    title =" Top 10 Frequence of Users who watched Movies "
  ) 


#########Genere########
movie %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(number = n()) %>%
  arrange(desc(number)) -> Genre


Genre %>% top_n(10) %>%
  ggplot(aes(x = reorder(as.factor(genres), -number),y= number,fill = as.factor(genres)))+geom_col()+guides(fill=guide_legend(title="genres"))+labs(
    x ="genres",
    y= "N",
    title =" Top 10 Frequence of Genres who watched Movies "
  ) 


#################Distinct USERS and Movies##########

n_distinct(movie$userId)
n_distinct(movie$movieId)



# Split dataset into movies and users
mba_list = split(movie$title,
                 movie$userId)

#######DATA into Transaction###
mba_list %>%
  as("transactions")->movie_transform

#Plot the relative and absolute item frequency plot
itemFrequencyPlot(movie_transform,
                  type = "relative",
                  topN = 10,
                  horiz = TRUE,
                  main = 'Relative item frequency')

itemFrequencyPlot(movie_transform,
                  type = "relative",
                  topN = 10,
                  horiz = TRUE,
                  main = 'Absolute item frequency')

#########most frequent itemset####

Freq_itemsets = apriori(movie_transform,
                   parameter = list(support = 0.4,
                                    target = 'frequent'))

inspect(sort(Freq_itemsets, by='support', decreasing = T)[1:5])







# Extract rules with the apriori
rules_movies = apriori(movie_trx,
                       parameter = list(supp = 0.3,
                                        conf = 0.9,
                                        minlen = 2, 
                                        target = "rules"))

# Summary of extracted rules
summary(rules_movies)

# Create redudant rules and filter from extracted rules
rules_red = is.redundant(rules_movies)
rules.pruned = rules_movies[!rules_red]
inspectDT(rules.pruned)
