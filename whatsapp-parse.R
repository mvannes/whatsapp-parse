require(dplyr)
require(ggplot2)
require(tm)
require(wordcloud2)
require(qdap)
require(webshot)
require(htmlwidgets)
require(stringr)

source("utility.R")

# Already done some alterations to
original.data <- readLines(
    ""
)
language <- "en"
chat.data <- initDataFrame(original.data)

friend.names = unique(chat.data$Friends)
media.rows <- grep("<media weggelaten>", chat.data$Content)
chat.data.media <- select(chat.data[media.rows,], -Content, -MessageLength, -WordCount)
chat.data.text <- chat.data[-media.rows,]

plot <- ggplot(chat.data.text, aes(x=Friends, fill=Friends)) +
  geom_bar(stat="count") +
  ylab("Amount of text messages") +
  ggtitle("Text sent")

savePlotPicture(plot, "amount-of-text-per-person.png")

plot <- ggplot(chat.data.media, aes(x = Friends, fill = Friends)) +
  geom_bar(stat="count") +
  ylab("Amount of media (images, video's, etc.) messages") +
  ggtitle("Media sent")

savePlotPicture(plot, "amount-of-media-per-person.png")

plot <- ggplot(chat.data.text, aes(y = MessageLength, x = Friends, fill = Friends)) +
    geom_boxplot()  +
    ylab("Length of message (number of characters)") +
    ggtitle("BoxPlot of message lengths")

savePlotPicture(plot, "message-length-boxplot.png")

plot <- ggplot(chat.data.text, aes(y = WordCount, x = Friends, fill = Friends)) +
    geom_boxplot()  +
    ylab("Length of message (number of characters)") +
    ggtitle("BoxPlot of message lengths")

savePlotPicture(plot, "word-count-boxplot.png")

plot <- ggplot(chat.data.text, aes(x = DateTimes, color = Friends, fill = Friends)) +
    geom_line(stat="count") +
  # geom_histogram(bins = length(unique(chat.data.text$DateTimes))) +
  ylab("Amount of text messages") +
  xlab("Time") +
  scale_x_date(breaks=date_breaks("1 day"), labels=date_format("%d %b")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

savePlotPicture(plot, "text-messages-over-time.png")

plot <- ggplot(chat.data.media, aes(x=DateTimes, fill=Friends)) +
  geom_histogram(bins = length(unique(chat.data.media$DateTimes))) +
  ylab("Amount of media (images, video's, etc.) messages") +
  xlab("Time")

savePlotPicture(plot, "media-messages-over-time.png")

plot <- ggplot(chat.data, aes(x = "", y = nrow(chat.data), fill = Friends)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar(theta = "y", start = 0 ) +
    labs(fill="Friends", x = NULL, y = NULL)

savePlotPicture(plot, "message-pie.png")

printMessageLengthData(chat.data.text, "Tuinkabouters")

# Copy over the text data to a matrix for Text Mining.
full.content <- chat.data.text$Content

full.content.corpus <- createCleanCorpus(full.content, language)
full.content.tdm <- TermDocumentMatrix(full.content.corpus)
createWordCloud(full.content.tdm, "full-content-cloud")


for (name in friend.names) {
  friend.data.text <- chat.data.text %>% filter(., Friends == name)
  friend.data.media <- chat.data.media %>% filter(., Friends == name)
  print(nrow(friend.data.text) + nrow(friend.data.media))
  print(nrow(friend.data.text))
  print(nrow(friend.data.media))
  printMessageLengthData(friend.data.text, name)

  friend.content <- friend.data.text$Content
  friend.corpus <- createCleanCorpus(friend.content, language)
  friend.tdm <- TermDocumentMatrix(friend.corpus)
  createWordCloud(friend.tdm, paste(name, "cloud", sep="-"))
}

# Boys vs girls
plot <- ggplot(chat.data.text, aes(x=Gender, fill=Gender)) +
    geom_bar(stat="count") +
    ylab("Amount of text messages") +
    ggtitle("Text sent by gender") +
    scale_fill_manual(values = c("pink", "lightblue"))

savePlotPicture(plot, "amount-of-text-per-gender.png")

plot <- ggplot(chat.data.media, aes(x = Gender, fill = Gender)) +
    geom_bar(stat="count") +
    ylab("Amount of media (images, video's, etc.) messages") +
    ggtitle("Media sent by gender") +
    scale_fill_manual(values = c("pink", "lightblue"))

savePlotPicture(plot, "amount-of-media-per-gender.png")

plot <- ggplot(chat.data.text, aes(y = MessageLength, x = Gender, fill = Gender)) +
    geom_boxplot()  +
    ylab("Length of message (number of characters)") +
    ggtitle("BoxPlot of message lengths by gender") +
    scale_fill_manual(values = c("pink", "lightblue"))

savePlotPicture(plot, "message-length-boxplot-per-gender.png")

plot <- ggplot(chat.data.text, aes(y = WordCount, x = Gender, fill = Gender)) +
    geom_boxplot()  +
    ylab("Length of message (number of words)") +
    ggtitle("BoxPlot of word count by gender") +
    scale_fill_manual(values = c("pink", "lightblue"))

savePlotPicture(plot, "word-count-boxplot-per-gender.png")


plot <- ggplot(chat.data, aes(x = "", y = nrow(chat.data), fill = Gender)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar(theta = "y", start = 0 ) +
    labs(fill="Gender", x = NULL, y = NULL) +
    scale_fill_manual(values = c("pink", "lightblue"))

savePlotPicture(plot, "message-pie-gender.png")

genders <- c("M", "F")
for (gender in genders) {
    gender.data.text <- chat.data.text %>% filter(., Gender == gender)
    gender.data.media <- chat.data.media %>% filter(., Gender == gender)
    print(nrow(gender.data.text) + nrow(gender.data.media))
    print(nrow(gender.data.text))
    print(nrow(gender.data.media))
    printMessageLengthData(gender.data.text, gender)

    gender.content <- gender.data.text$Content
    gender.corpus <- createCleanCorpus(gender.content, language)
    gender.tdm <- TermDocumentMatrix(gender.corpus)
    createWordCloud(gender.tdm, paste(gender, "cloud", sep="-"))
}

