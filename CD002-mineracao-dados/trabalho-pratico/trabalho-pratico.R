

# importando dados
library(readr)
spotify_data <- read_csv("C:/Dev/github/data-science-ufrgs/CD002-mineracao-dados/trabalho-pratico/Spotify 2010 - 2019 Top 100.csv")

View(spotify_data)

# conhecendo os dados

spotify_data1 <- spotify_data[, 6:8]

summary(spotify_data)
boxplot(spotify_data[,6:15],las=1)
spotify_genre <- table(spotify_data$"top genre")

barplot(table(spotify_data1$"top genre"))

distance <- get_dist(spotify_data1)
fviz_dist(distance, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))




# kmeans
kmi1 <- kmeans(spotify_data1, centers=3, nstart = 10)
kmi1
