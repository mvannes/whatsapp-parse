library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud2)
library(qdap)
library(webshot)
library(htmlwidgets)

source("utility.R")

# Already done some alterations to
original_data <- read.csv(
    "tuinchat.txt",
    header = FALSE,
    sep = " ",
    stringsAsFactors = FALSE,
    quote = "",
    encoding = "UTF-8"
)

chat_data <- initDataFrame(original_data)

friend_names = unique(chat_data$Friends)
foto_rows <- grep("<media weggelaten>", chat_data$Content)
chat_data_media <- select(chat_data[foto_rows,], -Content, -MessageLength, -WordCount)
chat_data_text <- chat_data[-foto_rows,]


plot <- ggplot(chat_data_text, aes(x=Friends, fill=Friends)) +
  geom_bar(stat="count") +
  ylab("Amount of text messages") +
  ggtitle("Text sent")

savePlotPicture(plot, "amount-of-text-per-person.png")

plot <- ggplot(chat_data_media, aes(x = Friends, fill = Friends)) +
  geom_bar(stat="count") +
  ylab("Amount of media (images, video's, etc.) messages") +
  ggtitle("Media sent")

savePlotPicture(plot, "amount-of-media-per-person.png")

plot <- ggplot(chat_data_text, aes(y = MessageLength, x = Friends, fill = Friends)) +
    geom_boxplot()  +
    ylab("Length of message (number of characters)") +
    ggtitle("BoxPlot of message lengths")

savePlotPicture(plot, "message-length-boxplot.png")

plot <- ggplot(chat_data_text, aes(y = WordCount, x = Friends, fill = Friends)) +
    geom_boxplot()  +
    ylab("Length of message (number of characters)") +
    ggtitle("BoxPlot of message lengths")

savePlotPicture(plot, "word-count-boxplot.png")

plot <- ggplot(chat_data_text, aes(x = DateTimes, fill = Friends)) +
  geom_histogram(bins = length(unique(chat_data_media$DateTimes))) +
  ylab("Amount of text messages") +
  xlab("Time")

savePlotPicture(plot, "text-messages-over-time.png")

plot <- ggplot(chat_data_media, aes(x=DateTimes, fill=Friends)) +
  geom_histogram(bins = length(unique(chat_data_media$DateTimes))) +
  ylab("Amount of media (images, video's, etc.) messages") +
  xlab("Time")

savePlotPicture(plot, "media-messages-over-time.png")

plot <- ggplot(chat_data, aes(x = "", y = nrow(chat_data), fill = Friends)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar(theta = "y", start = 0 ) +
    labs(fill="Friends", x = NULL, y = NULL)

savePlotPicture(plot, "message-pie.png")

printMessageLengthData(chat_data_text, "Tuinkabouters")

# Copy over the text data to a matrix for Text Mining.
full_content <- chat_data_text$Content

full_content_corpus <- createCleanCorpus(full_content)
full_content_tdm <- TermDocumentMatrix(full_content_corpus)
createWordCloud(full_content_tdm, "full-content-cloud")


for (name in friend_names) {
  friend_chat <- chat_data_text %>% filter(., Friends == name)
  friend_media <- chat_data_media %>% filter(., Friends == name)
  print(nrow(friend_chat) + nrow(friend_media))
  print(nrow(friend_chat))
  print(nrow(friend_media))
  printMessageLengthData(friend_chat, name)

  friend_content <- friend_chat$Content
  friend_corpus <- createCleanCorpus(friend_content)
  friend_tdm <- TermDocumentMatrix(friend_corpus)
  createWordCloud(friend_tdm, paste(name, "cloud", sep="-"))
}

# Boys vs girls
plot <- ggplot(chat_data_text, aes(x=Gender, fill=Gender)) +
    geom_bar(stat="count") +
    ylab("Amount of text messages") +
    ggtitle("Text sent by gender") +
    scale_fill_manual(values = c("pink", "lightblue"))

savePlotPicture(plot, "amount-of-text-per-gender.png")

plot <- ggplot(chat_data_media, aes(x = Gender, fill = Gender)) +
    geom_bar(stat="count") +
    ylab("Amount of media (images, video's, etc.) messages") +
    ggtitle("Media sent by gender") +
    scale_fill_manual(values = c("pink", "lightblue"))

savePlotPicture(plot, "amount-of-media-per-gender.png")

plot <- ggplot(chat_data_text, aes(y = MessageLength, x = Gender, fill = Gender)) +
    geom_boxplot()  +
    ylab("Length of message (number of characters)") +
    ggtitle("BoxPlot of message lengths by gender") +
    scale_fill_manual(values = c("pink", "lightblue"))

savePlotPicture(plot, "message-length-boxplot-per-gender.png")

plot <- ggplot(chat_data_text, aes(y = WordCount, x = Gender, fill = Gender)) +
    geom_boxplot()  +
    ylab("Length of message (number of words)") +
    ggtitle("BoxPlot of word count by gender") +
    scale_fill_manual(values = c("pink", "lightblue"))

savePlotPicture(plot, "word-count-boxplot-per-gender.png")


plot <- ggplot(chat_data, aes(x = "", y = nrow(chat_data), fill = Gender)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar(theta = "y", start = 0 ) +
    labs(fill="Gender", x = NULL, y = NULL) +
    scale_fill_manual(values = c("pink", "lightblue"))

savePlotPicture(plot, "message-pie-gender.png")

genders <- c("M", "F")
for (gender in genders) {
    gender_data_text <- chat_data_text %>% filter(., Gender == gender)
    gender_data_media <- chat_data_media %>% filter(., Gender == gender)
    print(nrow(gender_data_text) + nrow(gender_data_media))
    print(nrow(gender_data_text))
    print(nrow(gender_data_media))
    printMessageLengthData(gender_data_text, gender)

    gender_content <- friend_chat$Content
    gender_corpus <- createCleanCorpus(gender_content)
    gender_tdm <- TermDocumentMatrix(gender_corpus)
    createWordCloud(gender_tdm, paste(gender, "cloud", sep="-"))
}

