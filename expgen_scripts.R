#
#
# All the functions used in the stim generation process and later analysis
# Sourced in the respective scripts (see stimgenerator.R and analysis.R)
#
# Installs these packages if missing on first run:
#
reqpac = setdiff(c("tidyverse", "Hmisc", "data.table", "stringdist", "lme4", "entropy", "gtools", "patchwork", "ggbeeswarm", "ggrepel", "shadowtext", "scales", "colorspace"), installed.packages()[,"Package"] )
if(length(reqpac)>0){
  install.packages(reqpac)
}

# Load packages
# the important stuffs
library(tidyverse)
library(Hmisc)
library(data.table)
library(stringdist)
library(lme4)
library(entropy)
library(gtools)
library(word2vec)
# only for plotting
library(patchwork)
library(ggbeeswarm)
library(ggrepel)
library(shadowtext)
library(scales)
library(colorspace)


# these analyses were run using these version:
# tidyverse  "1.3.0"  
# Hmisc      "4.4-2"  
# data.table "1.13.6" 
# stringdist "0.9.6.3"
# lme4       "1.1-26" 
# entropy    "1.2.1"  
# gtools     "3.8.2"  
# patchwork  "1.1.1"  
# ggbeeswarm "0.6.0"  
# ggrepel    "0.9.1"  
# shadowtext "0.0.7"  
# scales     "1.1.1"  
# colorspace "2.0-0"



# mturk code gen

codesgen = function(n, p) {
  x = rep(NA, n)
  for (i in 1:n){
    x[i]= paste0("e",i,"p",p,"c",
                   paste0( sample(c(letters,0:9),5,replace = T),collapse="" ) )
  }
  return(x)
}





### plotting ####

plotpilot = function(y, targ, ppmi=F, onlycorrect=F, vmax=11){
  vmin=ifelse(ppmi, 0,2)
  ly=c(0.56,2.44)
  y = y[!y$burnin,]
  y$say = toupper(y$say)
  titl = ifelse(y$isbaseline[1], "baseline condition, ", "target condition, ")
  titl = paste0("Expm no. ",y$stimn[1],", ", titl, round(sum(y$correct[!y$burnin])/sum(!y$burnin)*100),"%" )
  stims = targ[[y$stimn[1]]]$stims
  targ = targ[[y$stimn[1]]]$targets %>% toupper()
  ord2 = table(y$say, y$sent) %>% reshape2::melt() %>% group_by(Var2) %>% summarise(sum=sum(value)) %>% arrange(desc(sum)) %>% pull(Var2) %>% as.character() %>% union(., stims)
  if(onlycorrect){
    y = y[y$correct,]
  }
  if(ppmi){
    sims = ppmisim(y, sim=T,removesingles = T, removeallsingles = F)
    tmp = ppmisim(y, sim=F, removesingles = T, removeallsingles = F)
    titl = paste0(titl, ", PPMI")
  } else {
    tmp = table(y$say, y$sent) %>% reshape2::melt() 
    titl = paste0(titl, ", counts")
  }
  ord = c(targ, setdiff(levels(tmp$Var1), targ))
  ord = lapply(seq(1,length(ord),2), function(x) sort(c(ord[x],ord[x+1])) ) %>% unlist(F,F)

  cls = c("gray85", brewer_pal()(9)[c(4,5,7)]) # %>% show_col()
  if(ppmi){
    tmp = tmp %>% mutate(value=ifelse(value==0,NA,value)) %>% mutate(value=round(value,2))
    sims = sapply(c(1,3,5), function(x) sims$value[sims$lab==paste0(ord[x],"-", ord[x+1])])
    sims2 = rescale(sims, ly,0:1)
             
  } else {
    tmp = tmp %>% mutate(value=ifelse(value==0,NA,value))# %>% mutate(value=as.factor(value))
  }
  g = 
  ggplot(tmp, aes(y=Var1,x=Var2,fill=value, color=value))+ 
    #annotate("rect", xmin=-Inf,ymin = 0.7,xmax = Inf,ymax = 6.3, fill="forestgreen",color=NA, alpha=0.1)+
    geom_tile(width=0.7,height=0.7, linejoin="round", size=1.5)+
    geom_text(aes(label=value), color="black", size=3.5) +
    scale_y_discrete(limits=ord)+
    scale_x_discrete(limits=ord2)+
    coord_cartesian(clip = 'off', xlim=c(1,length(ord2)+0.2)) +
    theme_bw()+theme(legend.position = "none", 
                     plot.margin = unit(c(0,1,0,0),"mm"),
                     panel.background = element_rect(fill="white"),
                     panel.grid.major = element_blank(),  # color="gray12"
                     panel.border = element_blank(),
                     axis.text = element_text(size=14),
                     axis.text.y = element_text(colour = c(rep("black",6),rep("darkgray",4)),size=rel(0.8)),
                     axis.text.x = element_text(colour = "black" ),
                     axis.ticks = element_blank(),
                     axis.title = element_blank()
                     )+
    xlab("")+ylab("")+ggtitle(titl) +
    scale_fill_gradientn(colours =  cls, limits=c(vmin,vmax),na.value="gray97" )+
    scale_color_gradientn(colours =  cls, limits=c(vmin,vmax),na.value="gray97" )
  
  if(!ppmi){
   g = g +
    annotate("segment", arrow=grid::arrow(angle=90,ends="both",length=unit(0,"lines")),
              x = rep(0.4,3), xend = rep(0.4,3), y=(ly[1])+c(0,2,4),yend=(ly[2])+c(0,2,4), size=0.25)  +
     annotate("segment", arrow=grid::arrow(angle=90,ends="both",length=unit(0,"lines")), 
              x = rep(length(stims)+0.5,3), xend = rep(length(stims)+0.5,3), y=(ly[1])+c(0,2,4),yend=(ly[2])+c(0,2,4), size=0.25)
  } else {
    g = g + 
      annotate("segment", arrow=grid::arrow(angle=90,ends="both",length=unit(0,"lines")),
               x = rep(0.4,3), xend = rep(0.4,3), y=(ly[1])+c(0,2,4),yend=(ly[2])+c(0,2,4), size=0.25) +
      annotate("segment", arrow=grid::arrow(angle=90,ends="both",length=unit(0,"lines")), 
               x = rep(7.5,3), xend = rep(7.5,3), y=(ly[1])+c(0,2,4),yend=(ly[2])+c(0,2,4), size=0.25) + 
    annotate("text", x = rep(7.5,3), y=sims2+c(0,2,4), size=2, shape=45, label="-",vjust=0.3) +
      annotate("text", x = rep(7.6,3), y=sims2+c(0,2,4), label=round(sims,2), size=4,hjust=0)
  }
  g
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
  for(i in ok){
    burnin = which(res[[i]]$burnin)
    x = res[[i]] %>%
      mutate(rown=1:nrow(.))   # proper trial numbers (can be rescaled later)
    # have to +1 for .csv because 0-indexing
    meanings = generated_stims[[ res[[i]]$stimn[1]+1 ]]$words
    stimtargets = generated_stims[[ res[[i]]$stimn[1]+1 ]]$targets
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
  #   
  #   ### simulate possible values
  #   #meaning = rep(c(1,2,1, 2,1,2), 4)
  #   tmp = permutations(3, 6, 1:3, F, T)
  #   tmp = cbind(tmp, tmp, tmp, tmp)
  #   n=ncol(tmp)
  #   # add more random combos to fill out the space:
  #   tmp2 = matrix(rep(rep(1, 24), 100), nrow=100)
  #   tmp3 = matrix(rep(rep(1:2, 12), 100), nrow=100)
  #   tmp4 = cbind(matrix(rep(rep(1, 12), 100), nrow=100), matrix(rep(rep(1:2, 6), 100), nrow=100)  )
  #   tmp = rbind(tmp,
  #               {tmp2[sample(ncol(tmp2)*nrow(tmp2), 100 )]=sample(2:3,100,T); tmp2},
  #               {tmp3[sample(ncol(tmp3)*nrow(tmp3), 100 )]=sample(1:3,100,T); tmp3},
  #               {tmp3[sample(ncol(tmp2)*nrow(tmp3), 100 )]=sample(1:10,100,T); tmp3},
  #               {tmp3[sample(ncol(tmp3)*nrow(tmp3), 100 )]=sample(1:10,100,T); tmp3},
  #               {tmp4[sample(ncol(tmp4)*nrow(tmp4), 100 )]=sample(1:2,100,T); tmp4},
  #               {tmp4[sample(ncol(tmp4)*nrow(tmp4), 1000 )]=sample(1:2,1000,T); tmp4},
  #               {tmp4[sample(ncol(tmp4)*nrow(tmp4), 100 )]=sample(1:10,100,T); tmp4},
  #               {tmp[sample(ncol(tmp)*nrow(tmp), 100 )]=sample(1:3,100,T); tmp},
  #               {tmp[sample(ncol(tmp)*nrow(tmp), 1000 )]=sample(1:3,1000,T); tmp},
  #               {tmp[sample(ncol(tmp)*nrow(tmp), 5000 )]=sample(1:3,5000,T); tmp},
  #               {tmp[sample(ncol(tmp)*nrow(tmp), 10000 )]=sample(1:3,10000,T); tmp},
  #               matrix(sample(1:2, n*1000, T), nrow=1000, byrow = T),
  #               matrix(sample(1:3, n*1000, T), nrow=1000, byrow = T),
  #               matrix(1:(n*1000), nrow=1000, byrow = T)
  #   )
  #   dim(tmp)
  #   
  #   compl=commc=rep(NA, nrow(tmp))
  #   for(i in 1:nrow(tmp)){                # self                             # other
  #     compl[i] = mean((tmp[i, 3:n] != tmp[i, 1:(n-2)]) + (tmp[i, 3:n] != tmp[i, 2:(n-1)]))
  #     commc[i] = mean((tmp[i, 3:n] != tmp[i, 1:(n-2)]) + (tmp[i, 3:n] == tmp[i, 2:(n-1)]))
  #   }
  #   tmp = data.frame(complexity=compl, commcost=commc, dyad=NA, xpair=".", condition="xsimu")
  #   tmp = tmp[!duplicated(tmp),]
  #   
  # dat4 = rbind(dat3, tmp)
  # return(dat4) # return compl infor data plus simus
  # }
  
  # ggplot(dat3, aes(y=commcost, x=complexity, color=condition, label=paste(dyad,xpair)))+geom_text()
  # plotpilot(res[[9]],generated_stims, F, F,11 )
  # dat2 %>% filter(dyad=="9") %>%  group_by(xpair) %>% summarise(commcost = mean(commcost)/2, complexity = mean(complexity)/2)
  # res[[9]] %>%  filter(say %in% c("coast", "shore"))
}








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



range01 <- function(x,mn,mx){(x-mn)/(mx-mn)}


#### simulation for the gray blobs ####



# rational and intentionally misleading agents
lawful_agent = function(j=NULL,i=NULL, alignment="good"){
  tmpl = matrix(0.1, 10, 7) # +1 to avoid /0
  x = data.frame(say = sample(rep(1:10, each=14))[1:135], 
                 sent=c(sample(1:j,1), rep(NA,134)), 
                 rown=1:135, dyad=paste(j,i,alignment, sep="_"))
  tmpl[x$say[1], x$sent[1]]=1.1
  if(alignment=="good"){
    for(i in 2:135){
      m = (tmpl[ x$say[i], 1:j] / colSums(tmpl[, 1:j]))
      mm = sample(which(m==max(m)),1) # if multiple fit, just pick one
      tmpl[x$say[i], mm] = round(tmpl[x$say[i], mm]+1,5) # round to avoid fl point errors
      x$sent[i] = mm
    }
  }
  
  if(alignment=="good2"){
    for(i in 2:135){
      m = (tmpl[ x$say[i], 1:j] / colSums(tmpl[,1:j]))
      mm = sample(which(m==max(m)),1) # if multiple fit, just pick one
      tmpl[x$say[i], -mm] = 0.1
      tmpl[x$say[i], mm] = round(tmpl[x$say[i], mm]+1,5) # round to avoid fl point errors
      x$sent[i] = mm
    }
  }
  if(alignment=="evil"){  # intentionally misleading
    for(i in 2:135){
      m = (tmpl[ x$say[i], 1:j] / colSums(tmpl[,1:j]))
      mm = sample(which(m==min(m)),1) # gamechanger
      tmpl[x$say[i], mm] = round(tmpl[x$say[i], mm]+1,5) # round to avoid fl point errors
      x$sent[i] = mm
    }
  }
  if(alignment=="evil2"){  # intentionally misleading
    for(i in 2:135){
      m = (tmpl[ x$say[i], 1:j] / colSums(tmpl[,1:j]))
      mm = sample(which(m==min(m)),1) # gamechanger
      tmpl[x$say[i], -mm] = 0.1
      tmpl[x$say[i], mm] = round(tmpl[x$say[i], mm]+1,5) # round to avoid fl point errors
      x$sent[i] = mm
    }
  }
  # tmpl %>% reshape2::melt() %>% mutate(value=na_if(value,0.1)) %>% ggplot(aes(Var1, Var2, fill=value))+geom_tile()
  table(x$say, x$sent) %>% reshape2::melt() %>% mutate(value=na_if(value,0.1)) %>% ggplot(aes(Var1, Var2, fill=value))+geom_tile()
  return(x)
}


gray_agent_simu = function(nrep=20){
  print(Sys.time())
  simus = list()
  ii=1
  
  x = data.frame(say = sample(rep(1:10, each=14))[1:135], sent=1, rown=1:135, dyad="d1") # degenerate case, no variation, neutral evil
  simus[[ii]] = x
  ii=ii+1
  
  # degen + some random to smooth out plot
  for(i in seq(5,40,5)){
    x = data.frame(say = sample(rep(1:10, each=14))[1:135], sent=1, rown=1:135, dyad=paste(1,i,"d", sep="_")) 
    ns=round(i/135*100)
    x$sent[sample(1:135, ns)] = sample(1:7, ns, T)
    simus[[ii]] = x
    ii=ii+1
  }
 
  # randos to "fill the gaps" in the space between the strategies below; chaotic neutral/neutral evil
  for(i in 1:(nrep*3) ){  # replications
    for(j in 2:7){
      x=data.frame(say = sample(rep(1:10, each=14))[1:135], 
                   sent=sample(1:j,135, T), rown=1:135, 
                   dyad=paste(j,i,"r", sep="_"))
      simus[[ii]] = x
      ii=ii+1
    }
  }
  
  # strict matching, randoms for gaps; chaotic neutral
  for(i in 1:nrep){  # replications
    for(j in 2:7){
      x = data.frame(say = sample(rep(1:10, each=14))[1:135], 
                     sent=NA, rown=1:135, dyad=paste(j,i,"s", sep="_"))
      x$sent = ifelse(x$say <= j, x$say, sample(1:j, 135, T))  # 1 to 1 lexification, otherwise random
      if(i %% 2 ==0 ){  # add some variation by adding noise to half of them
        ns = ((i/nrep*40+20))
        x$sent[sample(40:135, ns)] = sample(1:7, ns, T)
      }
      simus[[ii]] = x
      ii=ii+1
    }
  }
  
  
  # strict matching but colexifies, random for gaps; chaotic good 
  cotmp = c(1,1,2,2,3,3,4,5,6,7)
  for(i in 1:nrep){  
    for(j in 2:7){
      x = data.frame(say = sample(rep(1:10, each=14))[1:135], 
                     sent=NA, rown=1:135, dyad=paste(j,i,"sc", sep="_"))
      # 1 to 1 lexification, otherwise random
      x$sent = ifelse(x$say <= (j*2) & j<=3 | x$say <= (6+(j-3)) & j>=4, cotmp[x$say], 
                      sample(1:j, 135, T))  
      simus[[ii]] = x
      ii=ii+1
    }
  }
  # strict matching but colexifies, trimmed randoms for gaps; neutral good
  for(i in 1:nrep){ 
    for(j in 4:7){
      x = data.frame(say = sample(rep(1:10, each=14))[1:135], 
                     sent=NA, rown=1:135, dyad=paste(j,i,"scr", sep="_"))
      x$sent = ifelse( x$say <= (6+(j-3)) , cotmp[x$say], 
                       sample(4:j, 135, T))  
      if(i %% 2 ==0 ){  # add some noise by degenererating half of them
        ns = (135-(i/nrep*50+20))
        x$sent[ns:135] = 1
      }
      simus[[ii]] = x
      ii=ii+1
    }
  }
  
  # lawful/rational good and lawful evil (misleading) agents:
  for(i in 1:nrep){  # replications
    for(j in 2:7){
      x = lawful_agent(j,i,"good")
      simus[[ii]] = x
      ii=ii+1
      x = lawful_agent(j,i,"good2")
      simus[[ii]] = x
      ii=ii+1
      # add random degeneration and random behaviour to smooth out plot a bit
      x$dyad = paste(j,i,"dgood2") 
      x2 = x
      x2$sent[(135-(i/nrep*50+20)) :135]=1
      simus[[ii]] = x2
      ii=ii+1
      
      x$dyad = paste(j,i,"rgood2") 
      x2 = x
      ns = (135-(i/nrep*50+20))
      x2$sent[ns:135]=sample(1:7,length(ns:135),T)
      simus[[ii]] = x2
      ii=ii+1
      
      x = lawful_agent(j,i,"evil")
      simus[[ii]] = x
      ii=ii+1
      x = lawful_agent(j,i,"evil2")
      simus[[ii]] = x
      ii=ii+1
    }
  }
  return(simus)
}


gray_result_analyzer = function(simus){
  print(Sys.time())
  dfs=vector("list", length(simus))
  for(i in 1:length(simus)){
    #print(i)
    x = simus[[i]]
    df2=tibble()
    # randomly decide which were supposed to be agent's "similar" meaning pairs
    if(runif(1) <= 0.5){
      prs = c(4:6, 1:3) # "target condition" type for some agent strategies
    } else {
      prs = c(NA,NA,NA,NA,8,9,10, 5,6,7) # sort of "baseline condition" for some
    }
    x$xpair = as.character(pmin(x$say, prs[x$say]))
    for(j in 45:135){
      meaning=x$say[j]
      signal =x$sent[j]
      if(meaning %in% prs){ 
        lastself  = tail(which(x$say == meaning & x$rown < j), 1) 
        lastother = tail(which(x$say == prs[meaning] & x$rown < j), 1) 
        if(length(lastself)==1 & length(lastother)==1){ # if meaning and pairmeaning have been used
          comp = ((x$sent[lastself] != signal)+(x$sent[lastother] != signal)) # plus complexity
          cost = ((x$sent[lastself] != signal)+(x$sent[lastother] == signal)) # plus comm.cost
          d = tibble(complexity  = comp,
                     commcost = cost,
                     meaning=meaning,
                     signal=signal,
                     xpair=x$xpair[j],
                     dyad=x$dyad[1]
          )
          df2 = rbind(df2, d)
        }
      }
    }
    dfs[[i]] = df2 %>% group_by(xpair) %>% 
      summarise(dyad=first(dyad), xpair=first(xpair), 
                commcost = round(mean(commcost),4),  # don't need ~overlapping so filter
                complexity = round(mean(complexity),4)
                )
  }
  dfs = do.call(rbind, dfs)
  dfs2 = dfs[!duplicated(dfs %>% ungroup() %>%  select(commcost, complexity)), ]
  print(paste(Sys.time(), nrow(dfs), "parsed", nrow(dfs2), "unique" ))
  return(dfs2)
}


