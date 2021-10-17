install.packages("retrosheet")
require(retrosheet)
library(ggplot2)
library(tidyverse)
library(dplyr)

cont_RD = function(team, year) {
  d4t4 <- getRetrosheet("game", year)
  subject <- subset(d4t4, d4t4$HmTm == team | d4t4$VisTm == team)
    
  v = c(initial_rd)
    
  for (i in 1:nrow(subject)) {
    run_diff <- initial_rd + ifelse(subject$HmTm[i] == team, subject$HmRuns[i] - subject$VisRuns[i], subject$VisRuns[i] - subject$HmRuns[i]) 
    v = c(v, run_diff)
    initial_rd <- run_diff
  }
  ##print(v) in case you still want it there
}

Franchises = c("ANA","BAL","BOS","CHA","CLE","DET","HOU","KCA","MIN",'NYA','OAK',"SEA","TBA",
               "TEX","TOR","ARI","ATL","CHN","CIN","COL","LAN","SDN","MIA","MIL","NYN","PHI","PIT","SFN","SLN","WAS")


team_rd <- lapply(Franchises, FUN = cont_RD, year = 2019)
names(team_rd) <- Franchises 


## Realized you can't just go in an individually change each value
test <- as.data.frame(do.call(cbind, team_rd)) 
## Warning Message of unequal row length when running this, shows as the teams
## who played less games have random values instead of continuing their run differential
## so, you have to do it yourself

print(test[162, 4]) #CHA 162 (Did not play)
test[162, 4] = -124
print(test[162, 6]) # DET 162 (Did not play)
test[162, 6] = -333



## WORKS PERFECTLY
test_rd <- vector(mode = "list", length = 30)
test2 <- data.frame(matrix(ncol = 30, nrow = 162))
test_rd <- lapply(Franchises, FUN = cont_RD, year = 2019)


### can only append one value at a time in this case
for (i in 1:30) {
  ifelse(length(test_rd[[i]]) < 162, 
         test_rd[[i]] <- append(test_rd[[i]], test_rd[[i]][161]),
         print("All good."))
  ifelse(length(test_rd[[i]]) == 162 & max(lengths(test_rd)) == 163, 
         test_rd[[i]] <- append(test_rd[[i]], test_rd[[i]][162]),
         print("All good."))
}
names(test_rd) <- Franchises 
test2 = as.data.frame(do.call(cbind, test_rd)) 


##################Plotting Time

install.packages("reshape2")
library(reshape2)

test2_plot<- data.frame(x = 1:nrow(test2),                            # Reshape data frame
                       y = c(test2$ANA,test2$BAL,test2$BOS,test2$CHA,test2$CLE,
                             test2$DET,test2$HOU,test2$KCA,test2$MIN,test2$NYA,
                             test2$OAK,test2$SEA,test2$TBA,test2$TEX,test2$TOR,
                             test2$ARI,test2$ATL,test2$CHN,test2$CIN,test2$COL,
                             test2$LAN,test2$SDN,test2$MIA,test2$MIL,test2$NYN,
                             test2$PHI,test2$PIT,test2$SFN,test2$SLN,test2$WAS),
                       group = c(rep("ANA", nrow(test2)), rep("BAL", nrow(test2)),
                                 rep("BOS", nrow(test2)), rep("CHA", nrow(test2)),
                                 rep("CLE", nrow(test2)), rep("DET", nrow(test2)),
                                 rep("HOU", nrow(test2)), rep("KCA", nrow(test2)),
                                 rep("MIN", nrow(test2)), rep("NYA", nrow(test2)),
                                 rep("OAK", nrow(test2)), rep("SEA", nrow(test2)),
                                 rep("TBA", nrow(test2)), rep("TEX", nrow(test2)),
                                 rep("TOR", nrow(test2)), rep("ARI", nrow(test2)),
                                 rep("ATL", nrow(test2)), rep("CHN", nrow(test2)),
                                 rep("CIN", nrow(test2)), rep("COL", nrow(test2)),
                                 rep("LAN", nrow(test2)), rep("SDN", nrow(test2)),
                                 rep("MIA", nrow(test2)), rep("MIL", nrow(test2)),
                                 rep("NYN", nrow(test2)), rep("PHI", nrow(test2)),
                                 rep("PIT", nrow(test2)), rep("SFN", nrow(test2)),
                                 rep("SLN", nrow(test2)), rep("WAS", nrow(test2))))


rd_plot <- ggplot(test2_plot, aes(x, y, color = group)) + geom_line()
rd_plot + ggtitle("Run Differential Distribution") + xlab("Games Played") + ylab("Run Differential")

##FIN
