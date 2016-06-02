# Load Initial Packages
library(shiny)
library(plyr)
library(reshape)

wins12 <- read.table("win12log_v2.txt", sep = "\t", stringsAsFactor = FALSE, fill = TRUE, comment.char = "", 
                     quote = "\"", dec = ".", header = TRUE)
cardList <- read.table("hearthstonecarddatabase.txt", sep = "\t", stringsAsFactor = FALSE, fill = TRUE, comment.char = "", 
                       quote = "\"", dec = ".", header = TRUE)

listCardsClean <- list()
notFit <- list() 

for (b in 1:ncol(wins12)) {
  # Clean up each column based on what format it's in
  columnNum <- b
  listCards_orig <- wins12[[columnNum]][wins12[[columnNum]] != 0 & wins12[[columnNum]] != ""]
  
  # Finds entries starting off with (3) Card Name (3 copies)
  if (sum(duplicated(c(grep("^\\(([0-9])\\) [[:alnum:]]", listCards_orig), grep(" \\(([0-9]) [[:alnum:]]+[[:punct:]]$", 
                                                                                listCards_orig)))) > 1) {
    listCards_orig <- gsub("^\\(([0-9]+)\\) ", "", listCards_orig)
    listCards_orig <- gsub(" \\(([0-9]) [[:alnum:]]+[[:punct:]]$", " x\\1", listCards_orig)
  }
  # Finds entires starting off wtih 2 CardName x2
  if (sum(duplicated(c(grep("^([0-9]) [[:alnum:]]", listCards_orig), grep("^\\(([0-9])\\) [[:alnum:]]", listCards_orig),
                       grep("([0-9])$", listCards_orig)))) > 1) {
    listCards_orig <- gsub("^([0-9]+) |^\\(([0-9]+)\\) ", "", listCards_orig)
    listCards_orig <- gsub("x([0-9])$", "x\\1", listCards_orig)
  }
  
  listCards <- listCards_orig[-c(1:2)]
  listCards <- gsub("^ ", "", listCards)
  listCards <- gsub("^-", "", listCards)
  listCards <- gsub("x ([0-9])$", "x\\1", listCards)
  listCards <- gsub("- ([0-9])$", "x\\1", listCards)
  listCards <- gsub("^([0-9]) x", "\\1x", listCards)
  listCards <- gsub("\\(([0-9])\\)", "x\\1", listCards)
  listCards <- gsub("^([0-9]) ", "\\1x ", listCards)
  listCards <- gsub("\\[([0-9])\\]$", "x\\1", listCards)
  listCards <- gsub("([0-9])x$", "x\\1", listCards)
  listCards <- gsub("([A-Za-z])x([0-9])$", "\\1 x\\2", listCards)
  
  # Format of the list is in "1x Card Name" ex: 1
  if (length(grep("[0-9]x", listCards)) >= 1 | length(listCards) >= 30) {
    listCardsSplit <- strsplit(listCards, " ")
    if (length(grep("[0-9]x", listCards)) == length(listCards)) {
      listCardsName <- unlist(lapply(listCardsSplit, function(x) paste(x[-1], collapse = " ")))
      listCardsNum <- unlist(lapply(listCardsSplit, function(x) x[1]))
    } else {
      whichEntNum <- grep("[0-9]x", listCards)
      listCardsName <- listCards
      listCardsName[whichEntNum] <- unlist(lapply(listCardsSplit[whichEntNum], function(x) paste(x[-1], collapse = " ")))
      listCardsNum <- rep(1, length(listCards))
      listCardsNum[whichEntNum] <- unlist(lapply(listCardsSplit[whichEntNum], function(x) x[1]))
    }
    
    # Clean up card names
    storeMatch <- list()
    for (a in 1:length(listCardsName)) {
      storeMatch[[a]] <- grep(listCardsName[a], cardList$Name, value = TRUE)[1]
      if (is.na(storeMatch[[a]])) {storeMatch[[a]] <- agrep(listCardsName[a], cardList$Name, value = TRUE)[1]}
    }
    storeMatch <- unlist(storeMatch)
    
    # Clean up card numbers
    listCardsNum <- gsub("x", "", listCardsNum)
    
    # Checks
    if(sum(as.numeric(listCardsNum)) != 30) {print(paste("Warning: Number of cards not equal to 30! Column ", b, sep = ""))}
    
    listCardsClean[[b]] <-list(listCards_orig[1], listCards_orig[2], storeMatch, listCardsNum)
    
  } else if (length(grep("\\(x[0-9]\\)", listCards)) >= 1 | length(grep("x([0-9])$", listCards)) >= 1) {
    # Format of the list is in "Card Name (2x)" with number only appear on card number greater than 2 ex: 2
    whichSeq <- c("\\(x[0-9]\\)","x([0-9])$")[which(c(length(grep("\\(x[0-9]\\)", listCards)) >= 1 ,length(grep("x([0-9])$", 
                                                                                                                listCards)) >= 1))]
    whichEntNum <- grep(whichSeq, listCards)
    listCardsSplit <- strsplit(listCards[c(whichEntNum)], " ")
    listCardsName <- listCards
    listCardsName[whichEntNum] <- unlist(lapply(strsplit(listCards[whichEntNum], " "), 
                                                function(x) paste(x[-length(x)], collapse = " ")))
    listCardsNum <- rep(1, length(listCards))
    listCardsNum[whichEntNum] <- gsub("\\(x|\\)|x", "", unlist(lapply(strsplit(listCards[whichEntNum], " "), 
                                                                      function(x) x[length(x)])))
    
    # Checks
    if(sum(as.numeric(listCardsNum)) != 30) {print(paste("Warning: Number of cards not equal to 30! Column ", b, sep = ""))}
    
    # Clean up card names
    storeMatch <- list()
    for (a in 1:length(listCardsName)) {
      storeMatch[[a]] <- grep(listCardsName[a], cardList$Name, value = TRUE)[1]
      if (is.na(storeMatch[[a]])) {storeMatch[[a]] <- agrep(listCardsName[a], cardList$Name, value = TRUE)[1]}
    }
    storeMatch <- unlist(storeMatch)
    
    listCardsClean[[b]] <-list(listCards_orig[1], listCards_orig[2], storeMatch, listCardsNum)
    
  } else {notFit[[b]] <- "Not Fit"}
  # which(lapply(notFit, is.null) == FALSE)
  
  
}

# Checks: How many card names didn't match up?
which(unlist(lapply(listCardsClean, function(x) sum(is.na(x[[3]])))) > 0)

# Annotate each entry with data from list with information from the table
for (c in 1:length(listCardsClean)) {
  listCardsClean[[c]][[5]] <- subset(cardList, Name %in% listCardsClean[[c]][[3]])
}

### Analysis Begin

ClassColTable <- list(Mage = "dodgerblue",
                      Shaman = "blue",
                      Warrior = "darkred",
                      Warlock = "darkorchid4",
                      Priest = "cyan3",
                      Hunter = "chartreuse4",
                      Druid = "chocolate4",
                      Rogue = "black",
                      Paladin = "goldenrod1")

MageColGrad <- colorRampPalette(c("white", "dodgerblue"))
ShamanColGrad <- colorRampPalette(c("white", "blue"))
WarriorColGrad <- colorRampPalette(c("white", "darkred"))
WarlockColGrad <- colorRampPalette(c("white", "darkorchid4"))
PriestColGrad <- colorRampPalette(c("white", "cyan3"))
HunterColGrad <- colorRampPalette(c("white", "chartreuse4"))
DruidColGrad <- colorRampPalette(c("white", "chocolate4"))
RogueColGrad <- colorRampPalette(c("white", "black"))
PaladinColGrad <- colorRampPalette(c("white", "goldenrod1"))

classList <- c("Mage", "Shaman", "Warrior", "Warlock", "Priest", "Hunter", "Druid", "Rogue", "Paladin")

rarityDict <- list(c("B", "Basic", "black"), c("C", "Common", "gray"), c("E", "Epic", "dodgerblue3"), 
                   c("R", "Rare", "darkorchid4"), c("L", "Legendary", "gold1"))

# Create a list of each win entry of the cards in the deck, duplicates are repeated
All_CardNums <- lapply(listCardsClean, function(x) data.frame(CardName = x[[3]], Num = x[[4]]))
All_CardNums_reps <- lapply(All_CardNums, function(x) data.frame(as.matrix(rep(as.character(x[[1]]), as.numeric(x[[2]])))))
All_CardNums_reps <- lapply(All_CardNums_reps, function(x) {names(x) = "Name"; return(x)})
All_CardNums_reps <- lapply(All_CardNums_reps, function(x) join(x, cardList, by = "Name"))

### Begin Shiny server output
shinyServer(function(input, output) {
    
  ### Win Summary
  
  output$ClassWinTotals_help <- renderText({"The plot below shows the number of 12 arena wins separated by class. 
    While there are still a small number of records, this plot shows some indication of which classes are strongest
    in the arena."                                      
  })
                                            
  output$ClassWinTotals <- renderPlot({
    
    ClassWinTotals <- sort(table(unlist(lapply(listCardsClean, function(x) x[[1]]))))
    par(mar=c(5.1, 6.1, 5.1, 2.1))
    ClassWinTotals_plot <- barplot(sort(table(unlist(lapply(listCardsClean, function(x) x[[1]])))), 
      main = paste("n = ", length(listCardsClean), sep = ""), xlab = "Wins", font.lab = 2,
      col = unlist((ClassColTable)[match(names(ClassWinTotals), names(ClassColTable))]), yaxt = "n", font = 2, horiz = TRUE)
    axis(at = ClassWinTotals_plot, labels = names(ClassWinTotals), las = 2, side = 2, font = 2)

  })
  
  ### Card Rarity
  
  output$ClassRarityOverview_help <- renderText({"The plot on the left shows the overall distribution of card rarity among all Hearthstone cards.
    The plot on the right shows the number of cards in a particular category.  For example, a point at 
    Count = 10,  Number of Records = 25 on the Common Cards plot indicates that in 25 logs, the decks were composed of
    10 Common Cards.  This plot helps players relate the importance of having rarer cards to the number of wins as
    well as the overall distribution of cards based on rarity."                                      
  })
  
  output$ClassRarityOverview <- renderPlot({
    
    cardDist <- data.frame(Var1 = c("B", "C", "E", "R", "L"))
    for (f in 1:length(All_CardNums_reps)) {
      cardDist <- join(cardDist, as.data.frame(table(All_CardNums_reps[[f]]$Rarity)), by = "Var1")
    }
    names(cardDist) <- c("Rarity", paste("Count", 1:(ncol(cardDist)-1), sep = ""))
    cardDist_unlist <- melt(cardDist, id = "Rarity")
    cardDist_unlist[is.na(cardDist_unlist)] <- 0
    cardDist_unlist <- subset(cardDist_unlist, !is.na(value))
  
    
    rarityInput <- reactive({rarityDict[which(lapply(rarityDict, function(x) x[[2]]  == input$rarity) == TRUE)][[1]][1]})
    rarityInputCol <- reactive({rarityDict[which(lapply(rarityDict, function(x) x[[2]]  == input$rarity) == TRUE)][[1]][3]})
    
    
    Basic_sum <- as.matrix(table(subset(cardDist_unlist, 
                                        Rarity == rarityInput())$value))
    
    par(mfrow = c(1,2))
    pie(table(cardList$Rarity[cardList$Rarity != ""]), c("Basic", "Common", "Epic", "Legendary", "Rare"),
        col = c("black", "gray", "dodgerblue3", "gold1", "darkorchid4"), lwd = 2, font = 2, main = "Overall Distribution")
    plot(rownames(Basic_sum), Basic_sum, xlim = c(0,max(as.numeric(rownames(Basic_sum)))), ylim = c(0,max(Basic_sum)), type = "l",
         xlab = "Count", ylab = "Number of Records", lwd = 2, main = paste(input$rarity, "Cards",sep = " "), col = rarityInputCol(), font = 2, font.lab = 2)
    
  })
  
  ### Top 6 Class Cards
  
  output$ClassTopCards6_help <- renderText({"The plot below shows the Top 6 used cards for each class.  The cards are not ordered
                                            from left to right.  Darker colors indicate logs where a higher number of that 
                                            card was drafted while white indicates one card drafted.  For example in Mage, there
                                            are a few instances where Fireball waas drafted 4-5 times. The purpose of this plot
                                            is to show some of the strongest class cards potentially contributing to more wins."                                      
  })
  
  output$ClassTopCards6 <- renderPlot({
    
    ClassColGrad <- reactive({get(paste(input$class_ClassTopCards6, "ColGrad", sep = ""))(5)})
    
    ClassCardsClean <- reactive({listCardsClean[which(lapply(listCardsClean, function(x) x[[1]]) == input$class_ClassTopCards6)]})
    TopClassCards <- reactive({names(sort(table(unlist(lapply(ClassCardsClean(), function(x) unique(subset(x[[5]], Class == input$class_ClassTopCards6)$Name)))), 
                                    decreasing = TRUE))[1:6]})
    ClassCardsClean_CardNums <- lapply(ClassCardsClean(), function(x) data.frame(CardName = x[[3]], Num = x[[4]]))

    ClassCardsClean_CardNums_Top6 <- lapply(ClassCardsClean_CardNums, function(x) subset(x, CardName %in% TopClassCards()))
    ClassCardsClean_CardNums_Top6_unlist <- table(do.call("rbind", ClassCardsClean_CardNums_Top6))
    
    class(ClassCardsClean_CardNums_Top6_unlist) <- "matrix"
    ClassCardsClean_CardNums_Top6_unlist <- as.data.frame(ClassCardsClean_CardNums_Top6_unlist)
    ClassCardsClean_CardNums_Top6_unlist <- subset(ClassCardsClean_CardNums_Top6_unlist, 
                                                   rowSums(ClassCardsClean_CardNums_Top6_unlist) > 1)
    # Class Plot Top Cards
    par(oma=c(2, 1, 1, 1), mar=c(9.1, 4.1, 4.1, 8.1), xpd = TRUE)
    ClassCards_Top6 <- barplot(t(ClassCardsClean_CardNums_Top6_unlist), xaxt = "n", ylab = "Count", 
                               main = paste("Top 6 ", input$class_ClassTopCards6," Cards, n = ", length(ClassCardsClean()), sep = ""),
                               col = ClassColGrad(), font = 2, font.lab = 2)
    axis(ClassCards_Top6, rownames(ClassCardsClean_CardNums_Top6_unlist), side = 1, las = 2, font = 2)
    legend("topright", inset = c(-0.2, 0), fill = ClassColGrad(), legend = c("1", "2", "3", "4", "5"),
           title = "# Drafted")
    
  })
  
  
  ### Top Neutral Cards
  
  output$TopNeutralCards_help <- renderText({"What about Neutral cards?  The table below shows the logs with neutral cards
                                             and differentiates between how many times that card was picked (1-5).  It is
                                             interesting to compare the number of logs where the card was picked and the total
                                             number of logs to gauge the importance and value of that card."                                      
  })
  
  output$TopNeutralCards <- renderDataTable({
    
    ### Neutral top cards
    TopNeutralCards <- sort(table(unlist(lapply(listCardsClean, function(x) unique(subset(x[[5]], Class == "")$Name)))), 
                            decreasing = TRUE)
    TopNeutralCards_Top10 <- head(TopNeutralCards, 50)
    NeutralCardsClean_CardNums_Top10 <- lapply(All_CardNums, function(x) subset(x, 
                                                                                CardName %in% names(TopNeutralCards_Top10)))
    NeutralCardsClean_CardNums_Top10_unlist <- table(do.call("rbind", NeutralCardsClean_CardNums_Top10))
    class(NeutralCardsClean_CardNums_Top10_unlist) <- "matrix"
    NeutralCardsClean_CardNums_Top10_unlist <- as.data.frame(NeutralCardsClean_CardNums_Top10_unlist)
    NeutralCardsClean_CardNums_Top10_unlist <- subset(NeutralCardsClean_CardNums_Top10_unlist, 
                                                      rowSums(NeutralCardsClean_CardNums_Top10_unlist) > 1)
    data <- data.frame(rownames(NeutralCardsClean_CardNums_Top10_unlist), NeutralCardsClean_CardNums_Top10_unlist)
    names(data) <- c("Card", "1 Copy", "2 Copies", "3 Copies", "4 Copies", "5 Copies")
    data
    
  })  
  
  ### Mana curve
  
  output$ManaCurve_help <- renderText({"Mana Curve is one of most important considerations when drating for Arena.  The plot below
                                       shows the mana curve distribution for all records of the chosen class.  This helps players determine
                                       an optimal mana curve to shoot for when drafting."                                      
  })
  
  output$ManaCurve <- renderPlot({
    
    ManaCurve <- list()
    
    for (d in 1:length(listCardsClean)) {
      
      ManaCurve[[d]] <- as.data.frame(t(c(listCardsClean[[d]][[1]], table(cardList$Mana[match(rep(as.character(All_CardNums[[d]][,1]), All_CardNums[[d]][,2]), 
                                                                                              cardList$Name)]))), stringsAsFactors = FALSE)
    }
    ManaCurve_unlist <- do.call("rbind.fill", ManaCurve)
    names(ManaCurve_unlist)[1] <- "Class"
    ManaCurve_unlist <- ManaCurve_unlist[c(1,(order(as.numeric(names(ManaCurve_unlist)[-1])))+1)]
    ManaCurve_unlist[is.na(ManaCurve_unlist)] <- 0
    ManaCurve_unlist <- data.frame(ManaCurve_unlist[,1:8], rowSums(data.matrix(ManaCurve_unlist[,9:14])))
    
    classCol <- reactive({ClassColTable[[input$class_ManaCurve]]})
    
    Class_ManaCurve <- subset(ManaCurve_unlist, Class == input$class_ManaCurve)
    plot(as.numeric(Class_ManaCurve[1,-1]), type = "l", ylim = c(0, 20), lwd = 2, xaxt = "n", ylab = "Count", xlab = "Mana Drop",
         col = rgb(col2rgb(classCol())[1], col2rgb(classCol())[2], col2rgb(classCol())[3], 75, maxColorValue = 255), 
         main = paste(input$class_ManaCurve, " Mana Curve, n = ", nrow(Class_ManaCurve), sep = ""))
    axis(at = 1:(ncol(ManaCurve_unlist)-1), labels = c(as.character(c(0:6)), "7+"), side = 1)
    for (e in 2:nrow(Class_ManaCurve)) {
      lines(as.numeric(Class_ManaCurve[e,-1]), lwd = 2, col = rgb(col2rgb(classCol())[1], col2rgb(classCol())[2], col2rgb(classCol())[3], 75, 
                                                                  maxColorValue = 255))
    }
    
  })
  
  ### SWM Ratio
  
  output$SWM_Ratio_help <- renderText({"SWM Ratio stands for Spell-Weapon-Minion Ratio.  This indicates the ratio between
                                       these categories of cards in the decks.  The plot below shows the average number of 
                                       spells, minions, and weapons for each class.  The values are plotted next to each other 
                                       for comparison."                                      
  })
  
  output$SWM_Ratio <- renderPlot({
    
    for (d in 1:length(listCardsClean)) {
      
      SMW_Ratio[[d]] <- c(listCardsClean[[d]][[1]], sum(rep(as.character(All_CardNums[[d]][,1]), All_CardNums[[d]][,2]) %in% 
                                                          subset(cardList, Minion == 1)$Name),
                          sum(rep(as.character(All_CardNums[[d]][,1]), All_CardNums[[d]][,2]) %in% 
                                subset(cardList, Spell == 1)$Name),
                          sum(rep(as.character(All_CardNums[[d]][,1]), All_CardNums[[d]][,2]) %in% 
                                subset(cardList, Weapon == 1)$Name))
    }
    
    SWA_Ratio_unlist <- data.frame(do.call("rbind", SMW_Ratio), stringsAsFactors = FALSE)
    names(SWA_Ratio_unlist) <- c("Class", "Minions", "Spells", "Weapons")
    SWA_Ratio_unlist <- ddply(SWA_Ratio_unlist, ~ Class, summarise, mean_Minion = mean(as.numeric(Minions)), 
                              mean_Spell = mean(as.numeric(Spells)), mean_Weapon = mean(as.numeric(Weapons)))
    SWA_Ratio_unlist_mat <- t(SWA_Ratio_unlist[,-1])
    colnames(SWA_Ratio_unlist_mat) <- SWA_Ratio_unlist[,1]
    
    par(mar=c(5.1, 4.1, 4.1, 2.1), xpd = TRUE)
    SMW_Ratio_plot <- barplot(SWA_Ratio_unlist_mat[c(2,1,3),], beside = TRUE, xaxt = "n", ylab = "Count", 
                              main = paste("Spell-Minion-Weapon Average Ratio Per Class, n = ", length(listCardsClean), sep = ""),
                              col = sapply(paste(colnames(SWA_Ratio_unlist_mat), "ColGrad", sep = ""), function(x) get(x)(3)), 
                              font = 2, font.lab = 2)
    axis(SMW_Ratio_plot[2,], colnames(SWA_Ratio_unlist_mat), side = 1, las = 2, font = 2)
    text(x=SMW_Ratio_plot, y = c(SWA_Ratio_unlist_mat[c(2,1,3),]), 
         labels=rep(c("S", "M", "W"), length(classList)), pos=3, col="black", cex=0.75)
    
  })
  
  ### CM Ratio
  
  output$CM_Ratio_help <- renderText({"CN Ratio stands for Class Card-Neutral Card Ratio.  This indicates the ratio between
                                       these categories of cards in the decks.  The plot below shows the average number of 
                                       class cards and neutral cards for each class.  The values are plotted next to each other 
                                       for comparison."                                      
  })
  
  output$CM_Ratio <- renderPlot({
    
    # Class Card to Neutral Card Ratio per Class
    CM_Ratio <- list()
    
    for (d in 1:length(listCardsClean)) {
      
      CM_Ratio[[d]] <- c(listCardsClean[[d]][[1]], sum(rep(as.character(All_CardNums[[d]][,1]), All_CardNums[[d]][,2]) %in% 
                                                         subset(cardList, Class != "")$Name),
                         sum(rep(as.character(All_CardNums[[d]][,1]), All_CardNums[[d]][,2]) %in% 
                               subset(cardList, Class == "")$Name))
    }
    
    CM_Ratio_unlist <- data.frame(do.call("rbind", CM_Ratio), stringsAsFactors = FALSE)
    names(CM_Ratio_unlist) <- c("Class", "ClassCard", "Neutral")
    CM_Ratio_unlist <- ddply(CM_Ratio_unlist, ~ Class, summarise, mean_ClassCard = mean(as.numeric(ClassCard)), 
                             mean_Neutral = mean(as.numeric(Neutral)))
    CM_Ratio_unlist_mat <- t(CM_Ratio_unlist[,-1])
    colnames(CM_Ratio_unlist_mat) <- CM_Ratio_unlist[,1]
    
    par(mar=c(5.1, 4.1, 4.1, 2.1), xpd = TRUE)
    CM_Ratio_plot <- barplot(CM_Ratio_unlist_mat, beside = TRUE, xaxt = "n", ylab = "Count", 
                             main = paste("Class-Neutral Card Average Ratio Per Class, n = ", length(listCardsClean), sep = ""),
                             col = sapply(paste(colnames(CM_Ratio_unlist_mat), "ColGrad", sep = ""), function(x) get(x)(2)), 
                             font = 2)
    axis(colSums(CM_Ratio_plot)/2, colnames(CM_Ratio_unlist_mat), side = 1, las = 2, font = 2)
    text(x=CM_Ratio_plot, y = c(CM_Ratio_unlist_mat), 
         labels=rep(c("CC", "NC"), length(classList)), pos=3, col="black", cex=0.75)
  })
  
  ### Charge/Taunt Summary
  
  output$ChargeTauntSum_help <- renderText({"Many people believe early aggressive decks are better.  The plot below shows information
                                            about the number of charge and taunt monsters in these decks.  The player can evaluate the
                                            importance of aggression in the form of charge monsters or maintaining board presence
                                            through the use of more taunt monsters."                                      
  })
  
  output$ChargeTauntSum <- renderPlot({
    
    # Charge and Taunt Monster Summary
    ChargeNum <- list()
    TauntNum <- list()
    
    for (d in 1:length(listCardsClean)) {
      classInt <- classList[d]
      
      ChargeNum[[d]] <- as.data.frame(table(subset(cardList, Minion == 1)$Charge[match(rep(as.character(All_CardNums[[d]][,1]),
                                                                                           All_CardNums[[d]][,2]), cardList$Name)]), stringsAsFactors = FALSE)
      
      TauntNum[[d]] <- as.data.frame(table(subset(cardList, Minion == 1)$Taunt[match(rep(as.character(All_CardNums[[d]][,1]), 
                                                                                         All_CardNums[[d]][,2]), cardList$Name)]), stringsAsFactors = FALSE)
    }
    
    ChargeNum <- lapply(ChargeNum, function(x) if(nrow(x) < 2) {rbind(x, c(1,0))} else {x})
    ChargeNum <- array(as.numeric(unlist(ChargeNum)), dim = c(nrow(ChargeNum[[1]]), ncol(ChargeNum[[1]]), length(ChargeNum)))
    
    TauntNum <- lapply(TauntNum, function(x) if(nrow(x) < 2) {rbind(x, c(1,0))} else {x})
    TauntNum <- array(as.numeric(unlist(TauntNum)), dim = c(nrow(TauntNum[[1]]), ncol(TauntNum[[1]]), length(TauntNum)))
    par(mfrow = c(1,2))
    barplot(table(ChargeNum[2,2,]), xlab = "Count", ylab = "Number of Records", main = "Charge Monsters Used", font = 2, font.lab = 2,
            col = "red")
    barplot(table(TauntNum[2,2,]), xlab = "Count", ylab = "Number of Records", main = "Taunt Monsters Used", font = 2, font.lab = 2,
            col = "blue")
    
  })
  
})
