# For getting data from trial runs - note that the generated_stims were lost so values were replaced for each run.
# EXPGEN####################################################################################

library(readr)
library(dplyr)
# trajectory_1_0_97_0_05 <- read_csv("test.csv")
# View(trajectory_1_0_97_0_05)
# Set these first:
source("expgen_scripts.R") # full path to the scripts file





do_expmdat_fromfile_modified = function(
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
  
  f = list.files(path, full.names = F, pattern="2022")
  f = f[order(sapply(f, function(x) as.numeric(gsub("^([0-9]{1,3}).*","\\1",x) )))]
  res = lapply(f, function(x) readRDS(file.path(path,x)) )
  res =  res[!(sapply(res, function(x) x$stimn[1]) %in% bogus) ]   # filter out obvious cheaters etc
  
  # quick report:
  accs = sapply(res, function(x) sum(x$correct[!x$burnin])/sum(!x$burnin) )
  ok = which(accs>=accthreshold)
  print(paste0("bogus: ", length(bogus), " ok: ", length(accs), " but accurate enough: ", length(ok)))
  
  # warning and fix if game engine messed up stim numbers (at least once)
  if(any(duplicated(sapply(res, function(x) x$stimn[1])))){
    isdup = which(duplicated(sapply(res, function(x) x$stimn[1]), fromLast = T))
    for(d in isdup){
      res[[d]]$stimn = res[[d]]$stimn+300
    }
    warning(paste("Duplicated stim IDs found, adding big number (500) to id to avoid confusing dyads (shouldn't interfere with dyad suffixes which are 1000+)"))
  }
  which(duplicated(sapply(res, function(x) x$stimn[1]), fromLast = T))
  
  if(!doentropy){ # normal
    
    dat5 = do_expmdat_binary4(res,generated_stims, accthreshold=accthreshold, 
                              alsoburnin=F, dyadsuffix=dyadsuffix, doinfandcompl=doinfandcompl)
    levels(dat5$condition) = paste0(condprefix, levels(dat5$condition))
    dat5 %>% count(condition, dyad) %>% count(condition) %>% as.data.frame() %>% print()
    
    
    return(dat5)
    
  } else { # entropy one instead
    res = res[sapply(res, function(x) sum(x[!x$burnin,"correct"], na.rm = T)/nrow(x[!x$burnin,]) )>=accthreshold ]
    y = lapply(res, function(x) data.frame(condition = 
                                             paste0(condprefix, ifelse(x$isbaseline[1], "baseline","target") ), 
                                           entropy = entropy(table(x[!x$burnin,"sent"])),
                                           dyad = x$stimn[1] + dyadsuffix,
                                           acc = sum(x[!x$burnin,"correct"], na.rm = T)/nrow(x[!x$burnin,])
    ) 
    ) %>% do.call(rbind, .)
    y$condition = as.factor(y$condition)
    return(y)
  }
}


#### data analysis ####
do_expmdat_binary4 = function(res,generated_stims, accthreshold, alsoburnin=F, dyadsuffix=NA, doinfandcompl=F){
  # digs out target pairs again: 
  # (should have saved separately right away)
  targs = lapply(generated_stims, function(x) x$targets)
  tarx = vector("list", length(generated_stims))
  for(i in seq_along(targs)){
    for(j in seq(1,length(targs[[i]]),2 ) ){
      tarx[[i]] = c( tarx[[i]] ,     # homogenize alphabetically
                     sort( c(targs[[i]][j],targs[[i]][j+1])) %>% 
                       paste0(collapse="-" ) )
    }
  }
  # tarx is the list of pairs - as generated_stims is broken, we add our own
  tarx <- append(tarx, c("stomach-belly", "buddy-pair"))
  tarx = unlist(tarx) %>% unique() %>% sort()
  # another hacky version for later lookup:
  tmp=strsplit(tarx, "-")
  tarx2=c()
  for(i in seq_along(tmp)){
    x = tmp[[i]]; names(x) = rev(tmp[[i]])
    tarx2=c(tarx2, x)
  }
  if(doinfandcompl){ # additional structures if doing this
    tmptarx = sapply(tarx2, function(x) tarx[grepl(x, tarx)]) # grepl hack works bc stims all different
    dat2 = vector("list", length(res))
  }
  
  
  dat = vector("list", length(res))
  accs = sapply(res, function(x) sum(x$correct[!x$burnin])/sum(!x$burnin) )
  ok = which(accs>=accthreshold)
  generated_stims[[1]]$words <- list("author" , "danger" , "couple"  ,"barn"   , "stomach" ,"brow"  ,  "airport" ,"threat",  "creator", "pair")
  generated_stims[[1]]$targets <- list("author" , "danger" , "couple" , "stomach" ,"threat",  "creator", "pair")
  
  generated_stims[[2]]$words <- list( "chapel",  "trip"  ,  "abdomen" ,"belly" ,  "gossip" , "couple" , "journey", "temper" , "pair" )
  generated_stims[[2]]$targets <- list(  "trip"  ,  "abdomen" ,"belly"  , "couple" , "journey" , "pair")
  
  `11a_2022-02-28_14-46-21--2022-02-28_15-29-34` <- readRDS("~/R colex participant experiment/RESULTS/william/11a_2022-02-28_14-46-21--2022-02-28_15-29-34.RDS")
  `11a_2022-02-28_14-46-21--2022-02-28_15-29-34`$pair1 %>% unique
  generated_stims[[3]]$words <- list("purse", "film",  "bag",   "booth", "task" , "tree"  ,"job",   "shore", "coast")
  generated_stims[[3]]$targets <- list("purse",  "bag",    "task" , "job",   "shore", "coast")
  
  generated_stims[[4]]$words <- list( "motor" ,  "drizzle" ,"rain"   , "fashion", "diamond" ,"style",   "meat"   ,"buddy"  , "engine" )
  generated_stims[[4]]$targets <- list( "motor" ,  "drizzle" ,"rain"   , "fashion","style"  , "engine")
 
  for(i in ok){
    burnin = which(res[[i]]$burnin)
    x = res[[i]] %>%
      mutate(rown=1:nrow(.))   # proper trial numbers (can be rescaled later)
    # have to +1 for .csv because 0-indexing
    meanings = generated_stims[[ i ]]$words
    stimtargets = generated_stims[[ i ]]$targets
    # -> need to check against stim set directly not tarx2, as weakhyp uses target words in distractors
    
    if(doinfandcompl){ # additional structures if doing this
      x$xpair = tmptarx[x$say]
      df2 = tibble()
    }
    
    df = tibble()
    for(j in 1:nrow(x)){
      meaning = x$say[j]; signal = x$sent[j]; sender = x$sender[j]
      
      if(!doinfandcompl){
        last = tail(which(x$sender == sender & x$sent == signal & x$rown < j), 1)
        # only if colexification:
        if(length(last)==1 &&           # signal has been used before by same sender
           meaning %in% stimtargets &&  # sent meaning is target in this stim set
           x$say[last] != meaning       # but it's a colexification event
        )
        {                             # using && so it won't check others if false
          a1 = x$say[last] == tarx2[meaning] # match against target pair member
          a2 = ifelse(a1, "yes", "no")
          d = tibble(colextarget  = a2,
                     copair=x$say[last],
                     meaning=meaning, 
                     signal=signal,
                     sender=x$sender[j], 
                     row=x$rown[j]
          )
          df = rbind(df, d) 
        }
      } # or:
      
      if(doinfandcompl){   # if doing the info-compl calc
        # treats as collaborative language, ignoring speaker now
        if(meaning %in% stimtargets){ # only look at targets
          lastself  = tail(which(x$say == meaning & x$rown < j), 1) 
          lastother = tail(which(x$say == tarx2[meaning] & x$rown < j), 1) 
          if(length(lastself)==1 & length(lastother)==1){ # if meaning and pairmeaning have been used
            comp = ((x$sent[lastself] != signal)+(x$sent[lastother] != signal)) # plus complexity
            cost = ((x$sent[lastself] != signal)+(x$sent[lastother] == signal)) # plus comm.cost
            d = tibble(complexity  = comp,
                       commcost = cost,
                       meaning=meaning,
                       signal=signal,
                       #sender=x$sender[j],
                       row=x$rown[j],
                       xpair=x$xpair[j]
                       #acc=accs[i]
            )
            df2 = rbind(df2, d)
          }
        }
        # old simplicity: if target xpair comes up, chech if other member last used same signal
        #last = tail(which(x$sender == sender & x$say == tarx2[meaning] & x$rown < j), 1) # only other
        #last = tail(which(x$sender == sender & x$xpair == x$xpair[j] & x$rown < j), 1) 
        # get last meaning pair ref
        # the irrelevalt distractor ones will na out, and which() will not match
        #
      }
    } # done with looping over 1 experiment
    
    if(!doinfandcompl){
      # fix experiment df:
      if(!alsoburnin){
        df$isburnin = df$row %in% burnin
        df = df %>% dplyr::filter(!(row %in% burnin) ) 
      } else {
        df$isburnin = df$row %in% burnin
      }
      df$condition = ifelse(x$isbaseline[1],"baseline", "target")
      df$dyad = x$stimn[1]
      
      if(!is.na(dyadsuffix)){
        # if concatenating multiple datasets, will need to distinguish expm/dyad numbers somehow
        # so just adding a large number there
        df$dyad = df$dyad + dyadsuffix
      }
      df$sender = paste0(df$dyad, "_",df$sender)
      dat[[i]] = df
      rm(df)
    }
    
    if(doinfandcompl){ 
      # fix experiment df:
      if(!alsoburnin){
        df2$isburnin = df2$row %in% burnin
        df2 = df2 %>% dplyr::filter(!(row %in% burnin) ) 
      } else {
        df2$isburnin = df2$row %in% burnin
      }
      df2$condition = ifelse(x$isbaseline[1],"baseline", "target")
      df2$dyad = x$stimn[1]
      df2$sender = paste0(df2$dyad, "_",df2$sender)
      df2$acc = accs[i]
      dat2[[i]] = df2
    }
    
  } # done with looping over all experiments, concatenate all:
  
  if(!doinfandcompl){ 
    dat = do.call(rbind, dat) 
    dat$condition = as.factor(dat$condition)
    
    dat$dyad = as.factor(dat$dyad)
    dat$sender = as.factor(dat$sender)
    dat$colextarget=as.factor(dat$colextarget)
    # dat$relatedness=as.factor(dat$relatedness)
    dat$meaningpair=NA
    for(jj in 1:nrow(dat)){ # horribly inefficient but small data whatev
      dat$meaningpair[jj] = paste0(sort(unlist(dat[jj, c("copair", "meaning")],use.names = F,recursive = F)),collapse="-")
    }
    dat$row2 = range01(dat$row, 68, 135)  # might as well do the rescale here; all experiments have same number of rounds so ok to hard-code. now centered at middle, so -1...1
    #
    return(dat)
    #
  } else { 
    # if doing complexity posthoc, do other stuff
    dat2 = do.call(rbind, dat2)
    dat2$dyad = as.factor(dat2$dyad)
    
    dat3 = dat2 %>% group_by(dyad, xpair) %>%
      summarise(dyad=first(dyad), xpair=first(xpair),
                condition=first(condition),
                acc = first(acc),
                commcost = mean(commcost),
                complexity = mean(complexity)
      ) %>% ungroup()
    return(dat3)
  }
}






# Set these first:
#debugSource("expgen_scripts.R") # full path to the scripts file
resultsfolder = "RESULTS"  # full path to this unpacked folder, which contains the folders of RData files with the raw data (only needed if parsing from raw data), if not:
megadat = as_tibble(read.table("parsed_results_for_glmm.csv", header = T, quote=""))  # if not parsing from raw, set full path to this file, it has all the data



#### Data parser ####

# Uncomment to load and parse from scratch instead
# 
origdat = do_expmdat_fromfile_modified("william", 0, "first_",bogus = c(15), accthreshold = 0.59, edb=resultsfolder)

# Results for 11a : 13 yes 19 no (13/(13+29))
# View(filter(origdat, colextarget=="no"))


#repldat = rbind(do_expmdat_fromfile("repli", 1000, "repli_", c(44,11),edb=resultsfolder), do_expmdat_fromfile("repli2", 2000, "repli_", edb=resultsfolder))
#weakdat = rbind(do_expmdat_fromfile("weakhyp", 3000, "weak_",edb=resultsfolder), do_expmdat_fromfile("weakhyp2", 4000, "weak_", bogus = c(5,24), edb=resultsfolder))
#newdat = do_expmdat_fromfile("new", 5000, "new_",edb=resultsfolder)
#megadat = rbind(origdat,repldat, weakdat, newdat)
#length(table(megadat$dyad))

# - data collection took place over multiple months and required generating stimuli multiple times; the function above collects all the data from all the folders and makes sure IDs don't overlap.

#### Custom experiment and plot ####

origmodel = glmer(colextarget ~ row2*condition   +
                    (1 | signal) + 
                    (1  | sender ), 
                  data=origdat,  
                  family="binomial",
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
summary(origmodel) 




# ...at the end of a game, the pooled probability estimate of that is only
c(predict(origmodel,newdata=data.frame(condition="first_target", row2=1), re.form=NA, type="response"),
  predict(origmodel,newdata=data.frame(condition="first_baseline", row2=1), re.form=NA, type="response")) %>% round(2)

# This procedure, repeated for all players across all 41 games, yields a dataset of...
nrow(origdat)
# median of .. per dyad
origdat %>% group_by(dyad) %>% summarise(n=n()) %>% pull(n) %>% median


# the intercept value of  therefore stands for the log odds of target meanings being colexified in the baseline condition, mid-game (i.e. a
summary(origmodel)$coefficients[1,1] %>% plogis() %>% round(2)

#  indicating participants were less likely to colexify related meanings in the target condition (by the end of a game, the pooled probability estimate of that is only  
predict(origmodel,newdata=data.frame(condition="first_target", row2=1), re.form=NA, type="response") %>% round(2)
# compared to 
predict(origmodel,newdata=data.frame(condition="first_baseline", row2=1), re.form=NA, type="response")%>% round(2)
# in the baseline condition)


ggplot(origdat %>% group_by(dyad) %>%  mutate(row2 = 1:n()) %>% transform(condition = c("Baseline condition", "Target condition")[as.numeric(condition)]) , aes(y=dyad,x=row2,fill=colextarget, color=colextarget )) + 
  geom_tile( width=0.3, height=0.5, linejoin="round", size=1.1) +
  facet_wrap(~condition, scales="free_y" ) +
  scale_fill_manual(values= brewer_pal()(9)[c(3,6)], name="Target\nmeanings\ncolexified?") +
  scale_color_manual(values=brewer_pal()(9)[c(3,6)], name="Target\nmeanings\ncolexified?") +
  theme_minimal()+
  annotate("text", x = 3, y = -0.2, label = "Total:", size=10/.pt) +
  coord_cartesian(ylim=c(1,NA), clip = "off") +
  scale_x_continuous( expand = c(0.01,0.01), breaks=seq(10,50,10)) +
  xlab("Totalwdwdwd:") +
  ylab("wdwadawwdd") + 
  theme(legend.position = "right",
        legend.justification=c(1, 1), 
        legend.margin = margin(0,0,0,1),
        strip.background = element_blank(),
        strip.text = element_text(size=13,hjust=0),
        plot.margin = unit(c(0,0,0,1),"mm"),
        panel.background = element_rect(fill="white", color=NA),
        panel.grid.minor = element_blank(),  # color="gray12"
        panel.grid.major = element_blank(),
        #panel.border = element_blank(),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_blank(),
        #axis.title.x=element_text(hjust=0, vjust=7, size=11),
        #panel.border = element_rect(color="darkgray",fill=NA, size=0.2),
        panel.spacing = unit(1, "lines")
  )  +
  NULL


origdat <- rbind(origdat[origdat$condition=="first_baseline",],origdat[origdat$condition=="first_target",])

#### experiment 1 model and plot ####

origmodel = glmer(colextarget ~ condition*row2   +
                    (1  + condition | meaning) + 
                    (1  | dyad/sender ), 
                  data=origdat,  
                  family="binomial",
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
summary(origmodel) 




# ...at the end of a game, the pooled probability estimate of that is only
c(predict(origmodel,newdata=data.frame(condition="first_target", row2=1), re.form=NA, type="response"),
  predict(origmodel,newdata=data.frame(condition="first_baseline", row2=1), re.form=NA, type="response")) %>% round(2)

# This procedure, repeated for all players across all 41 games, yields a dataset of...
nrow(origdat)
# median of .. per dyad
origdat %>% group_by(dyad) %>% summarise(n=n()) %>% pull(n) %>% median


# the intercept value of  therefore stands for the log odds of target meanings being colexified in the baseline condition, mid-game (i.e. a
summary(origmodel)$coefficients[1,1] %>% plogis() %>% round(2)

#  indicating participants were less likely to colexify related meanings in the target condition (by the end of a game, the pooled probability estimate of that is only  
predict(origmodel,newdata=data.frame(condition="first_target", row2=1), re.form=NA, type="response") %>% round(2)
# compared to 
predict(origmodel,newdata=data.frame(condition="first_baseline", row2=1), re.form=NA, type="response")%>% round(2)
# in the baseline condition)


ggplot(origdat %>% group_by(dyad) %>%  mutate(row2 = 1:n()) %>% transform(condition = c("Baseline condition (each row represents a dyad)", "Target condition")[as.numeric(condition)]) , aes(y=dyad,x=row2,fill=colextarget, color=colextarget )) + 
  geom_tile( width=0.6, height=0.7, linejoin="round", size=1.1) +
  facet_wrap(~condition, scales="free_y" ) +
  scale_fill_manual(values= brewer_pal()(9)[c(3,6)], name="Target\nmeanings\ncolexified?") +
  scale_color_manual(values=brewer_pal()(9)[c(3,6)], name="Target\nmeanings\ncolexified?") +
  theme_minimal()+
  annotate("text", x = 3, y = -0.2, label = "Total:", size=10/.pt) +
  coord_cartesian(ylim=c(1,NA), clip = "off") +
  scale_x_continuous( expand = c(0.01,0.01), breaks=seq(10,50,10)) +
  xlab("Total:") +
  theme(legend.position = "right",
        legend.justification=c(1, 1), 
        legend.margin = margin(0,0,0,1),
        strip.background = element_blank(),
        strip.text = element_text(size=13,hjust=0),
        plot.margin = unit(c(0,0,0,1),"mm"),
        panel.background = element_rect(fill="white", color=NA),
        panel.grid.minor = element_blank(),  # color="gray12"
        panel.grid.major = element_blank(),
        #panel.border = element_blank(),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_blank(),
        #axis.title.x=element_text(hjust=0, vjust=7, size=11),
        #panel.border = element_rect(color="darkgray",fill=NA, size=0.2),
        panel.spacing = unit(1, "lines")
  )  +
  NULL


