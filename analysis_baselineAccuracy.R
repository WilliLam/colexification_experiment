#
# Scripts to replicate the analyses in: Karjus et al 2021, 
# "Conceptual similarity and communicative need shape colexification: an experimental study"
# See also: the stimgenerator.R for the script to generate the stims, 
# and the labapp folder for the game app source code
# The pre-compiled results dataframe is also included as a csv in the git repo.
#
#
library(readr)
library(dplyr)
# trajectory_1_0_97_0_05 <- read_csv("test.csv")
# View(trajectory_1_0_97_0_05)
# Set these first:
source("expgen_scripts.R") # full path to the scripts file
resultsfolder = "RESULTS"  # full path to this unpacked folder, which contains the folders of RData files with the raw data (only needed if parsing from raw data), if not:

grab_Meanings = function(
  subfolder = "",
  dyadsuffix = NA,
  condprefix = "",
  bogus = c(),
  accthreshold = 0.59,
  edb=resultsfolder, 
  doentropy=F, doinfandcompl=F
){
  path = file.path(edb, subfolder)
  # generated_stims = readRDS(file.path(path,"generated_stims.RDS"))
  
  f = list.files(path, full.names = F, pattern="meaningTest.*")
  f = f[order(sapply(f, function(x) as.numeric(gsub("^([0-9]{1,3}).*","\\1",x) )))]
  res = lapply(f, function(x) data.frame(read_csv(file.path(path,x))) )
  return(res)
}


grab_accuracy = function(
  subfolder = "",
  dyadsuffix = NA,
  condprefix = "",
  bogus = c(),
  accthreshold = 0.59,
  edb=resultsfolder, 
  doentropy=F, doinfandcompl=F
){
  path = file.path(edb, subfolder)
  generated_stims = readRDS(file.path(path,"generated_stims.RDS"))
  
  f = list.files(path, full.names = F, pattern="trajectory.*.csv")
  f = f[order(sapply(f, function(x) as.numeric(gsub("^([0-9]{1,3}).*","\\1",x) )))]
  res = lapply(f, function(x) data.frame(read_csv(file.path(path,x))) )
  uniqueIters = unique(unlist(res[[1]]["iterationNum"], use.names=FALSE))
  accuracies = lapply(uniqueIters, function(x) nrow(res[[1]] %>% filter(iterationNum==x) %>% filter(correct==TRUE)) /
                        nrow(res[[1]] %>% filter(iterationNum==x))
                        )
  accDF =  data.frame(`accuracy` = unlist(accuracies, use.names = FALSE) )
  return(accDF)
}


#### Data parser ####

# Uncomment to load and parse from scratch instead
# 
baseline1 = grab_accuracy("csv/Baseline/baselinegoodalpha0uttcost/alpha1", 0, "0nouttcost_",bogus = c(15), accthreshold = 0.59, edb=resultsfolder)
baseline1$alpha = 1
baseline3 = grab_accuracy("csv/Baseline/baselinegoodalpha0uttcost/alpha3", 0, "0nouttcost_",bogus = c(15), accthreshold = 0.59, edb=resultsfolder)
baseline3$alpha = 3
# foo <- data.frame(`accuracy` = unlist(origdat_baseline3, use.names = FALSE) )
baseline5 = grab_accuracy("csv/Baseline/baselinegoodalpha0uttcost/alpha5", 0, "0nouttcost_",bogus = c(15), accthreshold = 0.59, edb=resultsfolder)
baseline5$alpha = 5
baseline7 = grab_accuracy("csv/Baseline/baselinegoodalpha0uttcost/alpha7", 0, "0nouttcost_",bogus = c(15), accthreshold = 0.59, edb=resultsfolder)
baseline7$alpha = 7
# origdat_baselineutt = do_expmdat_fromfile_csv("csv/Baseline/uttcost", 0, "1uttcost_",bogus = c(15), accthreshold = 0.59, edb=resultsfolder)
accuracies = rbind(baseline1, baseline3, baseline5, baseline7)


ggplot(accuracies, aes(alpha, accuracy )) +
  geom_jitter(position = position_jitter(width = 0.01)) +
  stat_summary(fun.data = 'mean_sdl', geom = 'errorbar', width = 0.3) +
  stat_summary(fun.y = mean, geom = 'point', size = 20, shape = '_') + 
  scale_x_continuous(breaks=c(1,3,5,7))+
  scale_y_continuous(breaks=seq(0,1,0.05))
#repldat = rbind(do_expmdat_fromfile("repli", 1000, "repli_", c(44,11),edb=resultsfolder), do_expmdat_fromfile("repli2", 2000, "repli_", edb=resultsfolder))
#weakdat = rbind(do_expmdat_fromfile("weakhyp", 3000, "weak_",edb=resultsfolder), do_expmdat_fromfile("weakhyp2", 4000, "weak_", bogus = c(5,24), edb=resultsfolder))
#newdat = do_expmdat_fromfile("new", 5000, "new_",edb=resultsfolder)
#megadat = rbind(origdat,repldat, weakdat, newdat)
#length(table(megadat$dyad))


# Plot probability of null
meaningTest1 <- grab_Meanings("csv/Baseline/baselinegoodalpha0uttcost/alpha1", 0, "0nouttcost_",bogus = c(15), accthreshold = 0.59, edb=resultsfolder)[[1]]

lastRounds = meaningTest1[meaningTest1$trialNum > 90,]
lastRounds = lastRounds[lastRounds$word == "null",]

mean(lastRounds$val)
sd(lastRounds$val)

meaningTest2 <- grab_Meanings("csv/Baseline/baselinegoodalpha0uttcost/alpha7", 0, "0nouttcost_",bogus = c(15), accthreshold = 0.59, edb=resultsfolder)[[1]]

lastRounds = meaningTest2[meaningTest2$trialNum > 90,]
lastRounds = lastRounds[lastRounds$word == "null",]

mean(lastRounds$val)
sd(lastRounds$val)
