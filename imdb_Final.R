#install.packages('rvest')
#install.packages('dplyr')
#warning messages ignored
options(warn=-1)

library(rvest)
library(dplyr)
#taking the webpage link and assigning it to link variable
link = "https://www.imdb.com/search/title/?genres=adventure&groups=top_1000&sort=user_rating,desc&count=250"
#reads the HTML Code from the webpage
page = read_html(link)
#selecting parts of a webpage using CSS Selectors(selector gadget)
name = page %>% html_nodes(".lister-item-header a") %>% html_text()
year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
year = substr(year,2,5)
rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
votes = page %>% html_nodes(".sort-num_votes-visible span:nth-child(2)") %>% html_text()
genre = page %>% html_nodes(".genre") %>% html_text()
class(movies$votes)
# removing extra spaces.
genre <- trimws(genre)
# creating data frame
movies = data.frame(name, year, rating, votes,genre, stringsAsFactors = FALSE)
View(movies)
# converting character variable into numeric
movies$year <- as.numeric(movies$year)
movies$rating <- as.numeric(movies$rating)
# removing unwanted space and commas from variables and converting it into numeric
movies$votes<-sub("\\, ","",as.character(movies$votes))
movies$votes<-sub("\\,","",as.character(movies$votes))
movies$votes<-sub("\\,","",as.character(movies$votes))
movies$votes <- as.numeric(movies$votes)
write.csv(movies,"C:/Users/Debaroon Saha/OneDrive - associates.scit.edu/Desktop/SCIT/imdb_2.csv")
#install.packages("tidyr")
library(tidyr)
# splitting "genre" column into "Genre1","Genre2" and "Genre3"
movies <- separate(movies,col = genre, into = c("Genre1","Genre2","Genre3"),sep = ", ",)
View(movies)
# creating box plot to check outliers in "rating" column .
boxplot(movies$rating)
# checking data points outliers
outlier_value <- boxplot(movies$rating)$out
rating_outlier <-which(movies$rating %in% outlier_value)
movies[rating_outlier,]
# creating box plot to check outliers in "year" column
boxplot(movies$year)
# checking data points outliers
outlier_value <- boxplot(movies$year)$out
year_outlier <-which(movies$year %in% outlier_value)
movies[year_outlier,]
# creating box plot to check outliers in "votes" column
boxplot(movies$votes)
outlier_value <- boxplot(movies$votes)$out
votes_outlier <-which(movies$votes %in% outlier_value)
movies[votes_outlier,]

# aggregating "Genre1" by sum of "votes"
Genre1 <- aggregate(movies$votes,by = list(Genre1=movies$Genre1),FUN = sum)
View(Genre1)
# aggregating "Genre2" by sum of "votes"
Genre2 <- aggregate(movies$votes,by = list(Genre2=movies$Genre2),FUN = sum)
View(Genre2)
# aggregating "Genre3" by sum of "votes"
Genre3 <- aggregate(movies$votes,by = list(Genre3=movies$Genre3),FUN = sum)
View(Genre3)

# merging aggregates of "Genre1" and "Genre2"
Genre1_Genre2 <- merge(Genre1,Genre2, by.x = "Genre1", 
      by.y = "Genre2", all.x = TRUE, all.y = TRUE)
View(Genre1_Genre2)
# merging aggregates of "Genre1", "Genre2" and "Genre3" .
Genre1_Genre2_Genre3 <- merge(Genre1_Genre2,Genre3, by.x = "Genre1", 
                              by.y = "Genre3", all.x = TRUE, all.y = TRUE)
View(Genre1_Genre2_Genre3)
# conerting null values into zero for calculation purpose
Genre1_Genre2_Genre3[is.na(Genre1_Genre2_Genre3)] <- 0
# renaming columns of merged genres data set to avoid confusion while adding them.
colnames(Genre1_Genre2_Genre3) <- c("Genre","A","B","C")
# adding votes of unique genres.
Genre1_Genre2_Genre3$votes <- Genre1_Genre2_Genre3$A +
  Genre1_Genre2_Genre3$B + 
  Genre1_Genre2_Genre3$C
# removing unwanted columns and storing it to new data set.
Genre_votes <- Genre1_Genre2_Genre3[,c(1,5)]
# fetching out top ten genres on the basis of votes.
Top10Genres <- head(Genre_votes[order(-Genre_votes$votes),],10)
View(Top10Genres)

# creating a new columns to count movies coming under different genres.
movies$cnt <- c(1)
# aggregating "Genre1" by counts .
Genre1_count <- aggregate(movies$cnt,by = list(Genre1=movies$Genre1),FUN = sum)
View(Genre1_count)
# aggregating "Genre2" by counts .
Genre2_count <- aggregate(movies$cnt,by = list(Genre2=movies$Genre2),FUN = sum)
View(Genre2_count)
# aggregating "Genre3" by counts .
Genre3_count <- aggregate(movies$cnt,by = list(Genre3=movies$Genre3),FUN = sum)
View(Genre3_count)
# merging aggregates of "Genre1" and "Genre2" .
Genre1_Genre2_count <- merge(Genre1_count,Genre2_count, by.x = "Genre1", 
                       by.y = "Genre2", all.x = TRUE, all.y = TRUE)
# merging aggregates of "Genre1", "Genre2" and "Genre3" .
Genre1_Genre2_Genre3_count <- merge(Genre1_Genre2_count,Genre3_count, by.x = "Genre1", 
                              by.y = "Genre3", all.x = TRUE, all.y = TRUE)
# converting null values into zero for calculation purpose
Genre1_Genre2_Genre3_count[is.na(Genre1_Genre2_Genre3_count)] <- 0
# renaming columns of merged genres data set to avoid confusion while adding them.
colnames(Genre1_Genre2_Genre3_count) <- c("Genre","A","B","C")
# adding counts of unique genres.
Genre1_Genre2_Genre3_count$count <- Genre1_Genre2_Genre3_count$A +
  Genre1_Genre2_Genre3_count$B + 
  Genre1_Genre2_Genre3_count$C
# removing unwanted columns and storing it to new data set.
Genre_count <- Genre1_Genre2_Genre3_count[,c(1,5)]
# fetching out top ten genres on the basis of votes.
Top10Genres_count <- head(Genre_count[order(-Genre_count$count),],10)
View(Top10Genres_count)
# plotting top 10 genres on the basis of votes

library(ggplot2)
#dividing the votes by millions
a <- data.frame(group=Top10Genres$Genre, values=Top10Genres$votes/1000000, frame=rep('a',10))
View(a)

ggplot(a, aes(x=group, y=values, fill=group)) + 
  geom_bar(stat='identity') +
  ggtitle("TOP 10 GENRES BASED ON VOTES")+
  xlab("Genres")+
  ylab("Votes in millions")
# plotting top 10 popular genres on the basis of the count of movies.
b <- data.frame(group=Top10Genres_count$Genre, values=Top10Genres_count$count, frame=rep('b',10))
ggplot(b, aes(x=group, y=values, fill=group)) + 
  geom_bar(stat='identity') +
  ggtitle("MOST POPULAR GENRES IN TOP RATED MOVIES")+
  xlab("Genres")+
  ylab("Counts")


# plotting scatterplot to find relationship between ratings and votes.
plot(movies$rating,movies$votes/1000,
     xlim=c(7.5,10) , ylim=c(0,2200), 
     pch=18, 
     cex=2, 
     col="#69b3a2",
     xlab="Ratings", ylab="Votes in thousands",
     main="Rating v/s Votes"
)
# plotting histograms to check the frequencies of movies from yeae 1920 to 2022.
ggplot(data=movies, aes(movies$year)) + 
  geom_histogram(aes(y =..density..), 
                 breaks=seq(1920,2022, by = 5), 
                 col="red", 
                 fill="green", 
                 alpha=.2) + 
  geom_density(col=2) + 
  labs(title="Year Histogram", x="Year :  1920 - 2022", y="Frequency"
         ) 




