install.packages("retrosheet")
require(retrosheet)
library(ggplot2)
library(tidyverse)
library(dplyr)

# For plotting
install.packages("reshape2")
library(reshape2)

cont_RD = function(team, year) {
  d4t4 <- getRetrosheet("game", year)
  subject <- subset(d4t4, d4t4$HmTm == team | d4t4$VisTm == team)
  v = c()
  run_diff = 0
  
  for (i in 1:nrow(subject)) {
    run_diff <- run_diff + ifelse(subject$HmTm[i] == team, 
                                    subject$HmRuns[i] - subject$VisRuns[i], 
                                    subject$VisRuns[i] - subject$HmRuns[i]) 
    v = c(v, run_diff)
  }
  print(v) # Prints each of the team's run differential over 162 games
}

Franchises <- c("ANA","BAL","BOS","CHA","CLE","DET","HOU","KCA","MIN","NYA",
                "OAK","SEA","TBA", "TEX","TOR","ARI","ATL","CHN","CIN","COL",
                "LAN","SDN","MIA","MIL","NYN","PHI","PIT","SFN","SLN","WAS")

# First dataframe test
team_rd1 <- lapply(Franchises, FUN = cont_RD, year = 2019)
names(team_rd1) <- Franchises 
rd_df1 <- as.data.frame(do.call(cbind, team_rd1)) 

## Warning Message of unequal row length
## Those played less games than others have random values 
## instead of continuing their run differential
## You can't just go in an individually change each value
## So,the data must be cleaned

# Old Manual Data Cleaning
# print(rd_df1[162, 4]) #CHA 162 (Did not play)
# rd_df1[162, 4] = -124
# print(rd_df1[162, 6]) # DET 162 (Did not play)
# rd_df1[162, 6] = -333


## WORKS PERFECTLY
team_rd2 <- vector(mode = "list", length = 30)
rd_df2 <- data.frame(matrix(ncol = 30, nrow = 162))
team_rd2 <- lapply(Franchises, FUN = cont_RD, year = 2019)

# Can only append one value at a time
for (i in 1:30) {
  ifelse(length(team_rd2[[i]]) < 162, 
         team_rd2[[i]] <- append(team_rd2[[i]], team_rd2[[i]][161]),
         print("All good."))
  ifelse(length(team_rd2[[i]]) == 162 & max(lengths(team_rd2)) == 163, 
         team_rd2[[i]] <- append(team_rd2[[i]], team_rd2[[i]][162]),
         print("All good."))
}
names(team_rd2) <- Franchises 
rd_df2 <- as.data.frame(do.call(cbind, team_rd2)) 


# Plotting Time
rd_df2_plot<- data.frame(x = 1:nrow(rd_df2),                            
                        y = c(rd_df2$ANA,rd_df2$BAL,rd_df2$BOS,rd_df2$CHA,rd_df2$CLE,
                              rd_df2$DET,rd_df2$HOU,rd_df2$KCA,rd_df2$MIN,rd_df2$NYA,
                              rd_df2$OAK,rd_df2$SEA,rd_df2$TBA,rd_df2$TEX,rd_df2$TOR,
                              rd_df2$ARI,rd_df2$ATL,rd_df2$CHN,rd_df2$CIN,rd_df2$COL,
                              rd_df2$LAN,rd_df2$SDN,rd_df2$MIA,rd_df2$MIL,rd_df2$NYN,
                              rd_df2$PHI,rd_df2$PIT,rd_df2$SFN,rd_df2$SLN,rd_df2$WAS),
                        group = c(rep("ANA", nrow(rd_df2)), rep("BAL", nrow(rd_df2)),
                                  rep("BOS", nrow(rd_df2)), rep("CHA", nrow(rd_df2)),
                                  rep("CLE", nrow(rd_df2)), rep("DET", nrow(rd_df2)),
                                  rep("HOU", nrow(rd_df2)), rep("KCA", nrow(rd_df2)),
                                  rep("MIN", nrow(rd_df2)), rep("NYA", nrow(rd_df2)),
                                  rep("OAK", nrow(rd_df2)), rep("SEA", nrow(rd_df2)),
                                  rep("TBA", nrow(rd_df2)), rep("TEX", nrow(rd_df2)),
                                  rep("TOR", nrow(rd_df2)), rep("ARI", nrow(rd_df2)),
                                  rep("ATL", nrow(rd_df2)), rep("CHN", nrow(rd_df2)),
                                  rep("CIN", nrow(rd_df2)), rep("COL", nrow(rd_df2)),
                                  rep("LAN", nrow(rd_df2)), rep("SDN", nrow(rd_df2)),
                                  rep("MIA", nrow(rd_df2)), rep("MIL", nrow(rd_df2)),
                                  rep("NYN", nrow(rd_df2)), rep("PHI", nrow(rd_df2)),
                                  rep("PIT", nrow(rd_df2)), rep("SFN", nrow(rd_df2)),
                                  rep("SLN", nrow(rd_df2)), rep("WAS", nrow(rd_df2))))

group.colors = c(ANA = "#ba0021", BAL = '#df4601', BOS = '#bd3039', 
                 CHA = '#000000', CLE = '#e31937', DET = '#182d55', 
                 HOU = '#f4911e', KCA = '#004687', MIN = '#cfac7a', 
                 NYA = '#c4ced3', OAK = '#115740', SEA = '#005C5C', 
                 TBA = '#8fbce6', TEX = '#003278', TOR = '#134a8e', 
                 ARI = '#a71930', ATL = '#eaaa00', CHN = '#0e3386', 
                 CIN = '#c6011f', COL = '#33006f', LAN = '#005a9c', 
                 SDN = '#FFC425', MIA = '#ff6600', MIL = '#FFC52F', 
                 NYN = '#ff5910', PHI = '#e81828', PIT = '#fdb827', 
                 SFN = '#fd5a1e', SLN = '#22205f', WAS = '#ab0003')

rd_plot <- ggplot(rd_df2_plot, aes(x = x, y = y, group = group)) + 
  geom_line(aes(color=group)) +
  scale_color_manual(values = group.colors)

rd_plot + ggtitle("Run Differential Distribution") + 
  xlab("Games Played") + 
  ylab("Run Differential")
  
##FIN

