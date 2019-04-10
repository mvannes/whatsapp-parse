initDataFrame <- function(original.data) {
    full.messages <- tolower(original.data)
    rows.with.timestamp <- grep("^[0-9]{2}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2} - ", full.messages)
    i = length(full.messages)
    while (i > 0) {
        i <- i - 1;
        if (!(i %in% rows.with.timestamp)) {
            full.messages[i- 1] <- paste(full.messages[i- 1], full.messages[i])
        }
    }
    full.messages <- full.messages[rows.with.timestamp]
    date.times <- str_match(full.messages, regex('^[0-9]{2}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}')) %>%
        strptime("%d-%m-%y %H:%M") %>%
        as.Date.POSIXlt()
    message.author <- str_match(full.messages, regex(' - [a-z +0-9]+: ')) %>%
        gsub(" - ", "", .) %>%
        gsub(": ", "", .)
    message.content <- str_remove(
        full.messages,
        regex('^[0-9]{2}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2} - [a-z ]+: ')
    )

    chat.data <- data.frame(
        DateTimes = date.times,
        Friends = message.author,
        Content = message.content,
        stringsAsFactors = FALSE
    )

    # Also add message length and word count
    chat.data$MessageLength <- nchar(chat.data$Content)
    chat.data$WordCount<- sapply(
        chat.data$Content,
        function(x) { return(length(unlist(strsplit(x, " ")))) }
    )
    names(chat.data$WordCount) <- NULL

    # Assign everyone's gender.
    chat.data$Gender <- sapply(chat.data$Friends, assignGender)
    return(chat.data)
}

savePlotPicture <- function(plot_to_print, image_name) {
    png(paste(getwd(), "/img/", image_name, sep=""))
    print(plot_to_print)
    dev.off()
}

printMessageLengthData <- function(chat.data, name) {
    meanMessageLength <- mean(chat.data$MessageLength)
    minMessageLength <- min(chat.data$MessageLength)
    maxMessageLength <- max(chat.data$MessageLength)
    minMessage <- chat.data[which.min(chat.data$MessageLength),]$Content
    maxMessage <- chat.data[which.max(chat.data$MessageLength),]$Content

    meanWordCount <- mean(chat.data$WordCount)
    minWordCount <- min(chat.data$WordCount)
    maxWordCount <- max(chat.data$WordCount)

    print(
        paste(
            "Het gemiddelde bericht van ", name, " was ", round(meanMessageLength, 2), " karakters lang.",
            "Het kleinste bericht was ", minMessageLength, " karakters lang en was het volgende bericht: '", minMessage, "'",
            "Het grootste bericht was ", maxMessageLength, " karakters lang en was het volgende bericht: '", maxMessage, "'",
            "Het minst aantal woorden in een bericht was ", minWordCount, " het meeste aantal woorden in een bericht was ", maxWordCount,
            "gemiddeld zijn dit ", meanWordCount, " woorden",
            sep = ""

        )
    )
}

toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
alterApostrophe <- content_transformer(
    function(x) {
        x <- gsub("’", "'", x)
        return (x)
    }
)
decreaseLaughter <- content_transformer(
    function(x) {
        x <- str_replace_all(x, regex("a*ha+h[ha]*"), "hahaha")
        return (x)
    }
)

createCleanCorpus <- function(content, language) {
    content <- bracketX(content)
    corpus <- Corpus(VectorSource(content))
    corpus <- tm_map(corpus, toSpace, "-")
    corpus <- tm_map(corpus, toSpace, ":")
    corpus <- tm_map(corpus, alterApostrophe)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, decreaseLaughter)
    # Swap to switch between languages
    if (language == "nl") {
        dutch_stop_words <- readLines("dutch_stop_words.txt")
        corpus <- tm_map(corpus, removeWords, c(dutch_stop_words, "nee"))
    } else {
        stopwords_eng <- stopwords() %>% gsub("’", "", .) %>% gsub("'", "", .)
        corpus <- tm_map(corpus, removeWords, c(stopwords_eng, "just", "also", "really", "thats", "get"))
    }
    corpus <- tm_map(corpus, stripWhitespace)

    return(corpus)
}

createWordCloud <- function(cloud_tdm, title) {
    matrix <- as.matrix(cloud_tdm)
    sorted_matrix <- sort(rowSums(matrix), decreasing = TRUE)
    sorted_df <- data.frame(word = names(sorted_matrix), freq = sorted_matrix)

    print("top 20 woorden: ")
    print(head(sorted_matrix, 20))
    cloud <- wordcloud2(
        sorted_df,
        color ="random-light",
        backgroundColor = "black",
        rotateRatio = 0.15
    )
    saveWidget(cloud,paste(getwd(),"/img/clouds/", title, ".html", sep=""),selfcontained = F)
}

assignGender <- function(name) {
    women <- c("")
    gender <- if(name %in% women) 'F' else 'M'
    return(gender)
}
