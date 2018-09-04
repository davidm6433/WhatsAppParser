#' @title Parsing exported WhatsApp Textfiles as Dataframes
#'
#' @description Creates a dataframe from an exported WhatsApp textfile containing one row per message
#' @param name the name of the exported Whatsapp textfile to be parsed as a character string
#' @param emoticons "text" or "uni" replace Emojis with a textual description or Unicode respectively
#' @param smilies 1 uses \code{\link[qdabRegex]{ex_emoticon}} to extract smilies, 2 uses a more inclusive custom list
#' of smilies containing all mentions from \link{https://de.wiktionary.org/w/index.php?title=Verzeichnis:International/Smileys}
#' and manually added ones
#' @param anon TRUE results in the vector of sender names bein anonimized, FALSE displays the actual names
#' @param emoDesc TRUE adds a textual identifier around replaced Emojis
#' @param media TRUE will extract filenames from messages including mediafiles
#' @param web  "domain" will shorten sent links to domains
#' @export
#' @import stringi stringr qdapRegex readr tokenizers
#' @return A dataframe containing:
#'      1) A timestamp of when the message was sent
#'      2) The name of the sender (can be anonymized)
#'      3) The body of the message. Linebreaks and Emojis are replaced with textual indicators
#'      4) A list of Emojis contained in the message
#'      5) A list of ASCII smilies contained in the message
#'      6) An indicator stating whether media files were included in the message
#'      7) A list of domains from sent links contained in the message
#'      8) A lowercase list of words contained in the message with removed punctuation, numbers, Emojis and single characters
#' @examples
#'  Df <- WhatsAppParse(system.file("Example.txt", package = "WhatsR"),web = "url", media = TRUE)
#'  View(Df)

# Function to import a WhatsApp Textmessage and parse it into a readable dataframe
WhatsAppParse <- function(name, os = "android", emoticons = "text", smilies = 1, anon = FALSE, emoDesc = TRUE, media = FALSE, web="domain"){

        # We use readChar so that we can do the splitting manually after replacing the
        # Emojis, special characters and newlines
        chat <- readChar(name,file.info(name)$size)

        # Function for replacing the Emojis
        ReplaceEM <- function(x) {


                # Setup: importing emoticon List
                EmoticonList <- read.csv(system.file("EmojiDictionary.csv", package = "WhatsR"), header = TRUE, stringsAsFactors = FALSE,strip.white = FALSE, colClasses = "character")

                # order the list by the length of the R.native string to avoid partial matching of shorter strings
                EmoticonList <- EmoticonList[rev(order(nchar(EmoticonList$R.native))),]

                # Setup: We need to assign x to a new variable so we can save the progress in the for loop
                New <- x

                # rm_default throws a useless warning on each iteration that we can ignore
                oldw <- getOption("warn")
                options(warn = -1)

                # cycle htrough the list and replace everything
                # we have to add clean = FALSE and trim = FALSE to not delete whitespaces that are part of the pattern.
                if(emoticons == "text"){

                        for (i in 1:dim(EmoticonList)[1]){

                                New <- rm_default(New, pattern=EmoticonList[i,1],replacement= paste("[[EMOJI:: ", EmoticonList[i,3], "]]"), fixed = TRUE, clean = FALSE, trim = FALSE)

                        }}

                else if (emoticons == "uni"){

                        for (i in 1:dim(EmoticonList)[1]){

                                New <- rm_default(New, pattern=EmoticonList[i,1],replacement= paste("[[EMOJI:: ", EmoticonList[i,2], "]]"), fixed = TRUE, clean = FALSE, trim = FALSE)

                        }}

                # turning warnings back on
                options(warn = oldw)

                # output result
                return(New)

        }

        # replacing EMOJI with text representations
        chat2 <- ReplaceEM(chat)

        # Replacing special characters
        chat3 <- parse_character(chat2)

        if(os == "android"){

                # Deleting string for omitted media <Medien weggelassen> and the positioning unicode for present media data
                chatA <- gsub(pattern = "<Medien weggelassen>",chat3,replacement = "[[ MEDIA:: OMITTED ]]")

                # Cutting the messages in front of the datetime indicator
                chat4 <- unlist(strsplit(gsub("(.*?\\n)(\\d+[.]\\d+[.]\\d+)","\\1SPLITHERE\\2",chatA),"SPLITHERE"))

                # deleting trailing Linebreakes
                chat5 <- substr(chat4,1,nchar(as.character(chat4))-1)

                # Replacing Linebreaks within messages
                pat <- "\n"
                chat6 <- rm_default(chat5, pattern=pat,replacement= " [START:: NEWLINE] ")

                ### We cut the message to form vectors for datetime, sender and the actual message

                # Splitting
                DateSplit <- stri_split_fixed(str = chat6, pattern = " - ", n = 2)
                SenderSplit <- stri_split_fixed(str = sapply(DateSplit, "[", 2), pattern = ": ", n = 2)

                # Extract DateTime and encode correctly
                DateTime <- sapply(DateSplit, function(x){x[1]})
                DateTime <- strptime(DateTime, format="%d.%m.%y, %H:%M")

                # Extract Sender and encode correctly
                Sender <- sapply(SenderSplit, "[", 1)
                Sender <- as.factor(Sender)

                # Extract Message
                Message <- sapply(SenderSplit, "[", 2)

                # Deleting Left-to-right marker in front of filenames if send files if present
                Message <- sub("\u200e","",Message)
        }

        else if (os == "iOS"){

                # Cutting the textblock into individual messages
                chat4 <- strsplit(chat3,"\\r\\n",perl=TRUE)
                chat4 <- unlist(chat4)

                # Replacing Linebreaks within messages
                pat <- "\\n" # maybe we have to remove one \ again
                chat5 <- rm_default(chat4, pattern = pat, replacement= " [START:: NEWLINE] ")
                chat5

                ### We cut the message to form vectors for datetime, sender and the actual message

                # Splitting
                DateSplit <- stri_split_fixed(str = chat5, pattern = "]", n = 2)
                SenderSplit <- stri_split_fixed(str = sapply(DateSplit, "[", 2), pattern = ": ", n = 2)

                # Extract DateTime and encode correctly
                DateTime <- sapply(DateSplit, function(x){x[1]})
                DateTime <- substring(DateTime,2)
                DateTime <- strptime(DateTime, format="%d.%m.%y, %H:%M:%S")

                # Extract Sender and encode correctly
                Sender <- sapply(SenderSplit, "[", 1)
                Sender <- as.factor(Sender)

                # Extract Message
                Message <- sapply(SenderSplit, "[", 2)

                # Deleting Left-to-right marker in front of filenames if send files if present
                Message <- sub("\u200e","",Message)
        }

        ### We create handy vectors for used Emojis, extracted links, extracted media data
        # and one containing the message without stopwords, Emojis, linebreaks, URLs and punctuation

        # extracting links
        URL <- (rm_url(Message, extract=TRUE))

        if(web == "domain"){

                # Reduce the links to domain-names
                B <- lapply(URL,strsplit,"(?<=/)",perl=TRUE)
                C <- rapply(B,function(x){x <- unlist(x)[1:3]},how="list")
                D <- rapply(C,function(x){x <- paste(x,collapse="")},how= "list")
                D <- lapply(D,unlist)
                D[D == "NANANA"] <- NA
                URL <- D

        }

        # Multimedia send

        if(media == TRUE){


                Media <- stri_split_fixed(str = Message, pattern = "(Datei angehängt)", n = 2)
                Media[which(sapply(Media,length) == 1)] <- NA
                Media <- sapply(Media, "[", 1)


        } else if(media == FALSE){

                Media <- str_extract_all(Message,fixed("[[ MEDIA:: OMITTED ]]"))
                Media <- sapply(Media,function(x){if(length(x)== 0){x <- NA} else if(length(x)!= 0){x <- x}})

        }

        # Emojis
        Emoji <- rm_between(Message,"[[EMOJI::","]]",extract = TRUE)

        # Function to add the descritpive text back if desired

        if(emoDesc == TRUE){

                AddBack <- function(x){

                        for(i in seq_along(x)){

                                x[i] <- paste("[[EMOJI::",x[i],"]]")
                        }
                        return(x)
                }

                # adding descriptive text back
                Emoji[is.na(Emoji) == FALSE] <- lapply(Emoji[is.na(Emoji) == FALSE],AddBack)
        }
        else if(emoDesc == FALSE){}


        #### Extracting Non-Emoji Smileys

        # lazy version with prebuild dictionary
        if(smilies == 1){

                Smilies <- ex_emoticon(Message)

        }

        # using our own dictionary + wikipedia
        else if(smilies ==2){

                smilies <- read.csv(system.file("Smilies.csv", package = "WhatsR"),stringsAsFactors = F)
                smilies <- smilies[,2]
                smilies <- trimws(smilies)

                # solution taken from :https://stackoverflow.com/questions/48947534/match-multiple-strings-containing-escape-characters-in-r
                Smilies <- sapply(strsplit(Message, " "), function(x) x[x %in% smilies])
                Smilies[lapply(Smilies,length) == 0] <- NA

        }

        ### Creating a flattened Message for text mining

        # removing Emojis,newlines, media indicators
        Flat <- rm_between(Message,"[[EMOJI::","]]",replacement = "")
        Flat <- rm_between(Flat,"[START::","]",replacement = "")
        Flat <- str_replace_all(Flat,pattern = fixed("[[ MEDIA:: OMITTED ]]"), replacement = "")

        # Deleting Smilies


        # deleting filenames from messages
        Flat <- stri_split_fixed(str = Flat, pattern = "(Datei angehängt)", n = 2)

        # We create a helper variable that indicates the lenght of the split up Flat vector
        # then replace those List elements that have more then two subelements with the second subelement
        # (which is the message) and drop the first one (which is the URL)
        Help <- sapply(Flat,length)
        Flat[Help >= 2] <- lapply(Flat[Help >= 2],
                                  function(x){
                                          for(i in seq_along(x)){
                                                  x[[i]][1] <- x[[i]][2]}
                                  }
        )

        # transfer it to a character vector while unlisting so the NULLs become NAs automatically
        Flat <- unlist(as.character(Flat))

        # deleting URLs from messages
        Flat <- rm_url(Flat)
        Flat[Flat=="" | Flat =="NULL"] <- NA

        # removing punctuation and special characters
        Flat <- gsub("[[:punct:]]", " ", Flat)

        # removing numbers
        Flat <- gsub("\\d", "",Flat)

        # tokenizing the flattened message
        TokVec <- tokenize_words(Flat,lowercase = TRUE, stopwords = "de")

        # removing single letters from flattened message
        RemoveSingleLetters <- function(x){

                B <- lapply(x,nchar)
                C <- unlist(x)
                D <- C[unlist(B) > 1]
                unlist(D)
        }

        TokVec <- lapply(TokVec,RemoveSingleLetters)

        # Including everything in dataframe
        DF <- data.frame(DateTime,Sender,Message,I(Emoji),I(Smilies),Media,I(URL),I(TokVec),stringsAsFactors = FALSE) # I(URL)

        # checking if the default whatsapp starting message is present
        if(DF$Sender[1] == "Nachrichten in diesem Chat sowie Anrufe sind jetzt mit Ende-zu-Ende-Verschlüsselung geschützt. Tippe für mehr Infos."){

                DF <- DF[-c(1),]

        }

        DF$Sender <- droplevels(DF$Sender)

        # anonymizing chat participants
        if (anon == TRUE){


                Anons <- paste(rep("Person", length(levels(DF$Sender))), seq(1,length(levels(DF$Sender)),1), sep = " ")
                levels(DF$Sender) <- Anons
        }

        # return datframe
        return(DF)
}
