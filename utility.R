initDataFrame <- function(original_data) {

    full_messages <- do.call(paste, c(original_data[,0:ncol(original_data)], sep=" ")) %>%
        gsub(" NA", "", .) %>%
        trimws %>%
        tolower()
    rows_no_timestamp <- grep("^\\D", full_messages) # Alles dat zonder digit begint. Gaan we in een loopje toepassen die over
    # de inverse van de data iterate.
    i = length(full_messages)
    while (i > 0) {
        i <- i - 1;
        if (i %in% rows_no_timestamp) {
            full_messages[i- 1] <- paste(full_messages[i- 1], full_messages[i])
        }
    }
    full_messages <- full_messages[-rows_no_timestamp]
    original_data <- strsplit(full_messages, " ")

    dates <- as.vector(sapply(original_data, function(x) { return(x[1]) }))
    timestamps <- as.vector(sapply(original_data, function(x) { return(x[2]) }))
    friends <- as.vector(sapply(original_data, function(x) { return(x[4]) }))

    content_columns <- sapply(original_data, function(x) {
        stringified <- paste(x[-(1:4)], collapse = " ")
        return(stringified)
    })

    #content <- do.call(paste, c(content_columns, sep=" ")) %>%
    #    gsub(" NA", "", .) %>%
    #    trimws

    # Real friends have ":" in their name.
    actual_friends <- grep(":", friends)

    #Transform the dates to actual dates instead of strings.
    date_times <- paste(dates,  timestamps) %>% strptime("%m/%d/%y %H:%M:%S") %>% as.POSIXlt()

    # We only want our real fr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    iends to end up in the data frame.
    chat_data <- data.frame(
        DateTimes = date_times[actual_friends],
        Friends = gsub(":", "", friends[actual_friends]),
        Content = content_columns[actual_friends],
        stringsAsFactors = FALSE
    )

    # Also add message length and word count
    chat_data$MessageLength <- nchar(chat_data$Content)
    chat_data$WordCount<- sapply(
        chat_data$Content,
        function(x) { return(length(unlist(strsplit(x, " ")))) }
    )
    names(chat_data$WordCount) <- NULL

    # Assign everyone's gender.
    chat_data$Gender <- sapply(chat_data$Friends, assignGender)
    return(chat_data)
}

savePlotPicture <- function(plot_to_print, image_name) {
    png(paste(getwd(), "/img/", image_name, sep=""))
    print(plot_to_print)
    dev.off()
}

printMessageLengthData <- function(chat_data, name) {
    meanMessageLength <- mean(chat_data$MessageLength)
    minMessageLength <- min(chat_data$MessageLength)
    maxMessageLength <- max(chat_data$MessageLength)
    minMessage <- chat_data[which.min(chat_data$MessageLength),]$Content
    maxMessage <- chat_data[which.max(chat_data$MessageLength),]$Content

    meanWordCount <- mean(chat_data$WordCount)
    minWordCount <- min(chat_data$WordCount)
    maxWordCount <- max(chat_data$WordCount)

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

createCleanCorpus <- function(content) {
    dutch_stop_words <- readLines("dutch_stop_words.txt")
    content <- bracketX(content)
    corpus <- Corpus(VectorSource(content))
    corpus <- tm_map(corpus, toSpace, "-")
    corpus <- tm_map(corpus, toSpace, ":")
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeWords, c(dutch_stop_words, "nee"))
    corpus <- tm_map(corpus, stripWhitespace)
    return(corpus)
}

createWordCloud <- function(cloud_tdm, title) {
    matrix <- as.matrix(cloud_tdm)
    sorted_matrix <- sort(rowSums(matrix), decreasing = TRUE)
    sorted_df <- data.frame(word = names(sorted_matrix), freq = sorted_matrix)

    print("top 10 woorden: ")
    print(head(sorted_matrix, 10))
    cloud <- wordcloud2(
        sorted_df,
        color ="random-light",
        backgroundColor = "black",
        rotateRatio = 0.15
    )
    saveWidget(cloud,paste(getwd(),"/img/clouds/", title, ".html", sep=""),selfcontained = F)
}

assignGender <- function(name) {
    women <- c("elja", "lisa", "kim", "irene", "linda", "amber", "jessica", "muriel",
               "renske", "sophie", "amarja", "eva")
    gender <- if(name %in% women) 'F' else 'M'
    return(gender)
}
