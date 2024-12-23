#Loading Dataset
sent<-read.csv("D:/Prodigy Internship/twitter_training_data.csv")
str(sent)
dim(sent)
head(sent)
sent$sentiment<-as.factor(sent$sentiment)
sent$entity<-as.factor(sent$entity)
library(ggplot2)
library(cowplot)
unique(sent$entity)

data<-table(sent$sentiment)
#bar-graph
ggplot(data.frame(data), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "light blue") + geom_text(aes(label = Freq), vjust =-0.5) +
  labs(x = "Sentiment", y = "Count", title = "Sentiment Distribution of Data")
#pie-chart
pie_percent<- round(100 * table(sent$sentiment) / sum(table(sent$sentiment)), 1)
pie(table(sent$sentiment), labels = pie_percent,main = "Number of people survived", col=rainbow(length(data)))
legend("topright", c("Irrelavant","Negative","Neutral","Positive"),cex = 0.8,fill=rainbow(length(data)))

#MICROSOFT
microsoft<- table(sent$sentiment, sent$entity=="Microsoft")
df<-data.frame(microsoft)
df_true <- df[df$Var2 == TRUE, ]
# Visualize the sentiment distribution(MS)
g1<-ggplot(df_true, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "light pink") + geom_text(aes(label = Freq), vjust = 1.3) +
  labs(x = "Sentiment", y = "Count", title = "Sentiment Distribution of Microsoft")

#GOOGLE
google<- table(sent$sentiment, sent$entity=="Google")
df1<-data.frame(google)
df_true1 <- df1[df1$Var2 == TRUE, ]
# Visualize the sentiment distribution(Google)
g2<-ggplot(df_true1, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "light blue") + geom_text(aes(label = Freq), vjust = 1.3) +
  labs(x = "Sentiment", y = "Count", title = "Sentiment Distribution of Google")

#League of Legends
league<- table(sent$sentiment, sent$entity=="LeagueOfLegends")
df2<-data.frame(league)
df_true2 <- df2[df2$Var2 == TRUE, ]
# Visualize the sentiment distribution(League of Legends)
g3<-ggplot(df_true2, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "light green") + geom_text(aes(label = Freq), vjust = 1.3) +
  labs(x = "Sentiment", y = "Count", title = "Sentiment Distribution of League of Legends")

#Amazon
amazon<- table(sent$sentiment, sent$entity=="Amazon")
df3<-data.frame(amazon)
df_true3 <- df3[df3$Var2 == TRUE, ]
# Visualize the sentiment distribution(Amazon)
g4<-ggplot(df_true3, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "yellow") + geom_text(aes(label = Freq), vjust = 1.3) +
  labs(x = "Sentiment", y = "Count", title = "Sentiment Distribution of Amazon")

plot_grid(g1,g2, g3, g4, ncol = 2)






