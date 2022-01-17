
# Script to generate stimuli for the colexification experiment
# Uses a combination of computational similarity (wikipedia embeddings) and crowd-sourced (Simlex999) data


SCRIPTPATH = "expgen_scripts.R" # full path to where the expgen_scripts.R file is
DROPDIR    = "experimentdb"     # full path to where the stims should be saved

source(SCRIPTPATH)  # load required scripts
library(text2vec)

# Define  paths
simlexpath = "SimLex-999.txt"   # path to the Simlex999  dataset txt file
dictpath   = "en_GB-large.dic"  # path to some English wordlist
# we used https://sourceforge.net/projects/wordlist/files/speller/2019.10.06/hunspell-en_GB-large-2019.10.06.zip/download
vectorspath = "cc.en.300.vec" # path to embeddings; we used fastext trained on wikipedia dump 2019
freeassocpath = "norms2.csv"
# path to a single file containing the The University of South Florida word association, rhyme, and word fragment norms
# Nelson, D. L., McEvoy, C. L., & Schreiber, T. A. (1998). The University of South Florida word association, rhyme, and word fragment norms. http://w3.usf.edu/FreeAssociation/



# function to sample suitable sets from simlex
wordsampler = function(params 
                       , simlex
                       , simmat
                       , freenorms
                       , excludes=c()
                       , balancetimes=1
                       , balanceweighted=T
){
  
  nwords  =  params$nwords
  nstims  =  params$nstims
  nsimilar  =  params$nsimilar
  simthreshold_high  =  params$simthreshold_high
  simthreshold_low  =  params$simthreshold_low
  assocmax  =  params$assocmax
  editmin  =  params$editmin
  allowduplicates  =  params$allowduplicates
  allowsameinitial  =  params$allowsameinitial
  poslist  =  params$poslist
  wordlen_min  =  params$wordlen_min
  wordlen_max  =  params$wordlen_max
  freeassoc_threshold  =  params$freeassoc_threshold
  
  if(nsimilar*2 > nwords){stop("Requested number of similar words exceeds number of total words")}
  if(wordlen_min > wordlen_max){stop("Min word length cannot be > max word length!")}
  
  simlex = simlex %>% 
    dplyr::filter(nchar(word1) %in% (wordlen_min:wordlen_max)  & 
                    nchar(word2) %in% (wordlen_min:wordlen_max) ) %>% 
    dplyr::filter(POS %in% poslist) #%>% 
  #filter(Assoc.USF. < assocmax) %>% 
  #filter({stringdist(word1,word2)>=editmin })
  vocab = unique(unlist(simlex[,1:2]))
  vocab = setdiff(vocab, excludes)  # exclude some words incl proper nouns which appear as nouns in Simlex
  simlex = simlex[!(simlex$word1 %in% excludes | simlex$word2 %in% excludes),  ] # same filter for target set
  
  
  is_substring = rep(F, nrow(simlex))
  # in_freenorms = rep(F, nrow(simlex))
  for(i in 1:nrow(simlex)){
    is_substring[i] = grepl(simlex$word1[i], simlex$word2[i]) | grepl(simlex$word2[i], simlex$word1[i])
    # in_freenorms[i] = all(simlex$word1[i] %in% freenorms$CUE & simlex$word2[i] %in% freenorms$CUE)
  }
  
  # subset where targets will be drawn from:
  s = simlex[
    which(
      simlex$SimLex999 >= simthreshold_high &  # similar
        simlex$Assoc.USF. < assocmax &           # but not associated
        stringdist(simlex$word1, simlex$word2) >= editmin & # and not form-similar
        !is_substring # &   # AND also one word should not be substring in other, eg hero-heroine
      # in_freenorms       # And has free assoc norms score (for at least something)
    ) , ,drop=F
  ]
  # leaves 13 suitable candidate pairs only
  if(!allowsameinitial){
    s = s[which(
      substr(s$word1,1,1) != substr(s$word2,1,1)
    ),,drop=F]
  } # then 15
  
  # make labels
  s$labs = apply(s[, c("word1", "word2"), drop=F], 1, paste0,collapse=" " )
  ns = 1:nrow(s)
  
  if(balanceweighted){
    # weighted sampling: to have more uniform target pair distribution
    ssample=function(s,ns,globalx,sq=1){ 
      w = table(globalx)[s$labs] %>% as.numeric() %>% replace_na(0)
      w = (max(w)+1-w)^sq
      return( s[sample(ns, 1, prob = w), c("word1", "word2"), drop=F] )
    }
  } else{   # hacky patch to retain past compatibility
    ssample = function(s,ns,globalx=NULL,sq=NULL){
      return( s[sample(ns, 1), c("word1", "word2"), drop=F] )
    }
  }
  
  stims = vector("list", nstims)
  vocabdist = stringdistmatrix(vocab, useNames = T) %>% as.matrix()
  globalx = c() # storage for all targets from all stims; used for allowduplicates check
  adup2 = rep(ceiling(allowduplicates/(rev(1:balancetimes))), 
              each=ceiling(nstims/balancetimes))
  
  for(i in 1:nstims){
    # print(" ", quote=F); print(i)
    if(nsimilar>0){
      # Basic case, just one pair:
      # if required try first if n duplicates in stims already
      if(allowduplicates<nstims & i>1){
        x = ssample(s,ns,globalx)
        it=0
        while( sum(globalx %in% paste(x, collapse=" ")) >= adup2[i] ){
          it=it+1
          #cat(".") # debug
          if(it>1000){
            stop(paste("Found", i, "stims, but now stuck at finding more suitable target pairs, so stopping; adjust allowduplicates to higher value and restart."))
          }
          x = ssample(s,ns,globalx)
        }
      } else {
        x = ssample(s,ns,globalx)
      }
      
      
      # More than one pair within single stim:
      if(nsimilar>1){
        npairs = 1
        it=0
        it3=0
        while(npairs<nsimilar){ # skips if only 1 nsimilar pair requested  
          # look for similar pairs that would not be inter-similar, keep at it until found:
          it=it+1
          if(it>100){
            # to avoid local minima:
            print("Looking for 1+ pairs per stim, stuck, tried 100 times, seeding with a new one to get out of dead end")
            it=0; npairs = 1; it2=0; it3=it3+1
            if(it3>100){stop("Re-seeded still 100x couldn't find solution")}
            x = ssample(s,ns,globalx,2)
            while( sum(globalx %in% paste(x, collapse=" ")) >= adup2[i] ){
              it2=it2+1
              if(it2>100){stop("Restarted but can't find solution; adjust allowduplicates!")}
              x = ssample(s,ns,globalx,2)
            }
          }
          xnew = ssample(s,ns,globalx)
          # avoid duplicates, skip if found:
          xnewtmp = unlist(xnew, F,F)
          if( any(xnewtmp %in% unlist(x, F,F) ) ){ next }
          # if stims must have unique pairs, also this check:
          if(allowduplicates<nstims){
            if( sum(globalx %in% paste(xnewtmp, collapse=" ") ) >= adup2[i] ){ next }
            # (checking for first member is enough, given simlex has no duplicate pairs
            # so words are currently allowed to recur with allowduplicates, but pairs not
          }
          
          x2 = rbind(x, xnew)
          #print(simmat)
          #print(x2[,2])
          
          test1 =simmat[x2[,1],x2[,1]]
          test2 =simmat[x2[,2],x2[,2]]

          diag(test2) <- 0
          diag(test1) <- 0
          xm = max(
            c( test1, 
               test2
            ))
          xe = min(c(stringdistmatrix(unlist(x2, use.names = F)))) # dist object, diagonal excluded
          # if new pairs are ok, add (and keep looking for more if necessary):
          if(xm <= simthreshold_low & xe >= editmin){
            x = x2
            npairs=npairs+1
            gtmp = c(globalx, unname(apply(x,1, paste, collapse=" ")) )
            if(any(table(gtmp)>allowduplicates)){next}
          }
        } # end while
      } # end if
      
      globalx = c(globalx, unname(apply(x,1, paste, collapse=" ")) ) 
      x = c(t(x), use.names = F) # dataframe to vector
      
    } else {
      x = c() # if no pairs should be similar
    } # done finding target pairs, now:
    
    
    ### keep adding y distractors until limit reached:
    nneeded = nwords-length(x)
    y = rep(NA, nneeded)  # distractor array
    # see if actually possible:
    if(nneeded>length(vocab)){  
      stop("Requested word set is larger than Simlex, adjust or use larger pool")
    } else {
      if(nneeded>(length(vocab)/2)){
        print("Requested word set is half the size of Simlex, solution might be hard to find")
      }
    }
    # start:
    it=0
    while(any(is.na(y))){ # if the loop below fails forever, stop; brute force search basically
      if(it>1000){ 
        stop("Restarted distractor set search 1000 times already, couldn't find solution so stopping, maybe adjust something? (probably relax sim and association thresholds if larger word sets requested")
      }
      it=it+1
      candidates = sample(setdiff(vocab, x) ) # shuffle the vector so no need to keep sampling
      cl=length(candidates)
      i2=1
      for(ii in 1:nneeded){
        ok = c(F,F,F,F, F)
        while(!all(ok) & i2 <= cl ){
          y2 = candidates[i2]
          # test that new one has low similarity and form similarity to rest of y set:
          if(ii>1){
            ok[1] = all(simmat[y2, na.omit(y), drop=T] < simthreshold_low  )
            #if(!ok[1]) i2 = i2+1; next()
            ok[3] = all(vocabdist[y2, na.omit(y), drop=T] >= editmin )
            # if(!ok[3]) i2 = i2+1; next()
          } else {ok[c(1,3)]=T} # skip on first word onbviously
          # these assume there is actually an x target list btw --> change if that would be empty!
          # test that new one in y set is not similar to any in the x set:
          ok[2] = all(simmat[y2, x, drop=T] < simthreshold_low )
          # if(!ok[2]) i2 = i2+1; next()
          ok[4] = all(vocabdist[y2, x, drop=T] >= editmin )
          # if(!ok[4]) i2 = i2+1; next()
          tmp = unlist(freenorms[which(freenorms$CUE == y2 & 
                                         freenorms$TARGET %in% c(x,na.omit(y))), "score"])
          if(length(tmp)>0){ # logic: if no score, then no free association i.e. implicit 0 score 
            ok[5] = all(tmp<freeassoc_threshold) 
          } else {
            ok[5] = T 
          }
          i2 = i2+1
          #print(i2) # debug
        }
        if(!all(ok) | i2>cl ){
          break  # if couldn't find next word or ran out of candidates, the restart, try until works basically
        } else { # if candidate matches conditions then add and move on
          y[ii] = y2
        }
      }
      # print(it, quote=F) # debug
    } # this ends if required number of words found
    stims[[i]] = c(x,y)
    
  } # end stims for-loop
  stims = stims[sample(1:length(stims))] 
  # shuffle the word samples (target distro somewhat biased otherwise, depending on max duplicates value)
  attr(stims, "globalx") = globalx
  #stims = lapply(stims, unlist, use.names=F) # concatenate targets and distractors
  return(stims)
}





# function to generate an artificial lexicon
langgen = function(langlen, engdict, maxinitial, vowels, cons, transl){
  # homogenize kpt-gbd, w-v, z-s -- so filters also out similar sounding ones
  if(!is.null(transl)) engdict = translate(engdict, transl[1], transl[2])
  vowels = strsplit(vowels, "")[[1]]
  cons   = strsplit(cons, "")[[1]]
  
  syllables = expand.grid(cons, vowels, stringsAsFactors = F) %>%  
    apply(1,paste0, collapse="")
  view(syllables)
  # 60 combos
  if (langlen >= 4) {
    lang = character()
    print("magic")
    for (i in 1:5000) {
      #print(i)
      #print("wtf")
      #x = paste(sample(syllables, langlen, replace = TRUE), sep="", collapse="")
      print(paste(sample(syllables, langlen, replace = TRUE), sep="", collapse=""))
      lang = append(lang, (paste(sample(syllables, langlen, replace = TRUE), sep="", collapse="")))
    }
    #view(lang)
    #print(lang)
    return(lang)
  } else {
    partlist = replicate(langlen, syllables) %>% 
      as.data.frame(stringsAsFactors=F) %>% 
      as.list()
    view(partlist)
    lang = do.call(expand.grid, partlist) %>% 
      apply(1,paste0, collapse="") %>%
      setdiff(engdict) %>%   # exclude english and english-sounding words
      head(5000)
    view(lang)
    # 2998 left
    print(paste("Generated a language of", length(lang), "unique words"))
    return(lang)
  }

}

# function to go through stim word sample sets, and find a suitable artificial language lexicon
stimgen = function(stims_words, lang, nlang, editmin, cons, maxinitial, constraininitials){
  #lang = artificial language, generates english words
  stimlist = vector("list", length(stims_words))
  vocabdist = stringdistmatrix(lang, unique(unlist(stims_words, use.names = F)), useNames = T )
  # langdist  = stringdistmatrix(lang, useNames = T) %>% as.matrix()
  cons=strsplit(cons, "")[[1]]
  ncons = length(cons)
  if(constraininitials){ # if initials should not overlap with language stims
    initials = substr(lang, 1,1)
  }
  for(s in seq_along(stims_words)){
    candidates = lang
    if(constraininitials){ # if initials should not overlap with language stims
      candidates = candidates[ which(!(initials %in% substr(stims_words[[s]],1,1) ) ) ]
    }
    candidates = sample(candidates) # reshuffle language
    cl=length(candidates)
    i2=1
    y = rep(NA, nlang)
    for(ii in 1:nlang){
      ok = c(F,F,F)  # not super efficient, could speed up by next'ing loop at any fail
      while(!all(ok) & i2 <= cl ){
        y2 = candidates[i2]
        if(ii>1){
          # test that new one has low form similarity to rest of y set:
          ok[1] = all(as.vector(stringdistmatrix(y2, na.omit(y))) >= editmin )
          # and that initial consonant is not repeated too many times:
          ok[3] = all( table(substr(c(y,y2),1,1)) <= maxinitial)
        } else {ok[c(1,3)]=T} # skip on first word obviously
        # test that new one in y set is not form-similar to any in the x set:
        ok[2] = all(vocabdist[y2, stims_words[[s]], drop=T] >= editmin )
        i2 = i2+1
        #print(i2) # debug
      }
      if(!all(ok) | i2>cl ){
        stop(paste("Could not find artificial lang solution for stim no", s,
                   "(which is weird, the list should be huge - unless the input letter strings are tiny, or the constraints conflict, see above)"))
      } else { # if candidate matches conditions then add and move on
        y[ii] = y2
      }
    }
    stimlist[[s]] = y
  }
  return(stimlist)
}


# for each experiment, generate the dataframe of stimuli to be shown

stimcombinator = function(stims, params, actualtargets=NULL){
  # NB! The script below uses "-" as a reserved character for dealing with meaning pairs,
  # i.e. the English nouns (meanings) should not include hyphens.
  
  pm = params$pairmultiplier
  generated_stims = vector("list", length(stims$words))
  isbaseline = c(rbind(  rep(F, params$nstims-params$nbaseline),
                         rep(T, params$nbaseline) 
  )) # assumes equal n
  for(s in seq_along(stims$words)){
    # for the weak-hyp extra condition:
    if(!is.null(actualtargets)){
      ti = actualtargets*2   # select first n pairs; other target pairs will be "others"
      nsimilar2 = actualtargets
    } else {
      ti = (params$nsimilar*2)  # all targets are targets
      nsimilar2 = params$nsimilar
    }
    
    # save those in random order for display later:
    generated_stims[[s]]$stims = sample(stims$stims[[s]]) 
    generated_stims[[s]]$words = (stims$words[[s]])
    generated_stims[[s]]$targets = stims$words[[s]][1:ti] # save targets (assumes there are >0)
    targs = generated_stims[[s]]$targets
    targs2=rep(NA, length(targs)/2)
    for(j in seq(1,length(targs),2 ) ){
      targs2[j] = sort( c(targs[j],targs[j+1]) ) %>% 
        paste0(collapse="-" ) 
    }
    generated_stims[[s]]$targetlabels = targs2[!is.na(targs2)]
    
    # prepare meaning combos
    targs = rep(NA, nsimilar2); j=1
    for(i in 1:nsimilar2){
      targs[i] = paste(stims$words[[s]][j:(j+1)], collapse="-")
      j=j+2
    }
    twords = unlist(strsplit(targs, "-"))
    com = combn(stims$words[[s]], 2)
    # this is a bit hacky but works:
    others = apply(com, 2, paste, collapse="-") # everything first
    others = setdiff(others, targs)
    halftargs = strsplit(others, "-") %>% sapply(., function(x) any(x %in% twords) ) %>% others[.]
    others = setdiff(others, halftargs)   # actual final others list now
    if(isbaseline[s]){
      pairs = c(rep(targs, each=pm["baseline"]), 
                rep(others, each=pm["baseline"] ), 
                rep(halftargs, each=pm["baseline"] ) 
      )
    } else {
      pairs = c(rep(targs, each=pm["targets"]), 
                rep(others, each=pm["distractors"] ), 
                rep(halftargs, each=pm["halftargets"] ) 
      )
    }
    
    pairs2 = as.data.frame(cbind( sapply(strsplit(pairs, "-"), `[[`, 1),
                                  sapply(strsplit(pairs, "-"), `[[`, 2) ),stringsAsFactors = F)
    colnames(pairs2) = c("pair1", "pair2")
    nr = nrow(pairs2)
    # add burnin period column
    burn = c(F,T, rep(F, params$burnin-2))
    pairs2$burnin = rep(burn, ceiling(nr/params$burnin))[1:nr]
    
    
    # next block: try to also balance the distribution of prompts, what they're told to say
    #
    nx = 0; sdx=Inf
    while(sdx>1){  # not this might need adjusting if different sample size, will not try forever:
      if(nx>100){stop("balancing prompt distribution has failed 100x times, fix something")}
      # randomize order while preserving balance in and out of burnin, and burnin first
      pairs2 = rbind(
        {pairs2[pairs2$burnin,] %>% .[sample(1:nrow(.)),] },
        {pairs2[!pairs2$burnin,] %>% .[sample(1:nrow(.)),] }
      )
      
      # which of the pair to message
      pairs2$say = as.character(NA)
      t1 = pairs2[!pairs2$burnin,1:2] %>% unlist(use.names = F) %>% table %>% {./2} %>% as.matrix() %>% .[,1] %>% mean
      t2 =  pairs2[!pairs2$burnin,1:2] %>% unlist(use.names = F) %>% table %>% {./2} %>% as.matrix() %>% .[,1]; 
      t2[] = 0
      for(i in which(!pairs2$burnin)){
        px = unlist(pairs2[i, 1:2],use.names = F,recursive = F)
        px = px[order(t2[px], decreasing = F)]
        x = which( t2[px] <= t1 )[1] %>% names()
        x = ifelse(is.na(x), sample(px,1), x) # shouldn't be needed but failsafe
        pairs2$say[i] = x
        t2[x] = t2[x]+1
      }
      sdx = table(pairs2$say[!pairs2$burnin]) %>% sd
      nx=nx+1
    }
    
    # lazy, will just copypaste above block for the burnin section (notice lack of "!"):
    nx = 0; sdx=Inf
    while(sdx>1){  # note this might need adjusting if different sample size, will not try forever:
      if(nx>100){stop(paste("balancing prompt distribution has failed 100x times in",s,"fix something"))}
      # randomize order while preserving balance in and out of burnin
      pairs2 = rbind(
        {pairs2[pairs2$burnin,] %>% .[sample(1:nrow(.)),] },
        {pairs2[!pairs2$burnin,] %>% .[sample(1:nrow(.)),] }
      )
      pairs2$say[pairs2$burnin] = NA
      t1 = pairs2[pairs2$burnin,1:2] %>% unlist(use.names = F) %>% table %>% {./2} %>% as.matrix() %>% .[,1] %>% mean
      t2 =  pairs2[pairs2$burnin,1:2] %>% unlist(use.names = F) %>% table %>% {./2} %>% as.matrix() %>% .[,1]; 
      t2[] = 0
      for(i in which(pairs2$burnin)){
        px = unlist(pairs2[i, 1:2],use.names = F,recursive = F)
        px = px[order(t2[px], decreasing = F)]
        x = which( t2[px] <= t1 )[1] %>% names()
        x = ifelse(is.na(x), sample(px,1), x) # shouldn't be needed but failsafe
        pairs2$say[i] = x
        t2[x] = t2[x]+1
      }
      sdx = table(pairs2$say[pairs2$burnin]) %>% sd
      nx=nx+1
    }
    
    pairs2$label = ""   # new - make labels, abc-sorted so easier to deal with
    for(i in 1:nrow(pairs2)){
      pairs2$label[i] = paste0(sort(c(pairs2$pair1[i], pairs2$pair2[i])),
                               collapse="-")
    }
    
    # one last doublecheck, make sure the prompts are ok:
    if(any(!(pairs2$say %in% pairs2$pair1 | pairs2$say %in% pairs2$pair2))){
      stop(paste("misalignment in prompts in",s,"something is very wrong"))
    }
    
    
    # print summary and examples on first one
    if(s==1){  
      ptabl = as.data.frame(sort(table(pairs),decreasing = T),stringsAsFactors = F)
      print(paste("The meanings are, for example:", paste(stims$words[[s]],collapse=" "), ". The game will have a total of", sum(ptabl[,2]), "rounds:" ))
      print(head(ptabl,15))
      print("...")
      print(paste("Target pair(s) make up", 
                  round(length(rep(targs, pm[1]))/length(pairs)*100,1)
                  , "% of the game,",  round(length(rep(targs, pm[1]))/length(pairs)/params$nsimilar*100,1)
                  ,"% per each pair though. The frequencies of individual meanings in the game are:"))
      print(strsplit(pairs, "-") %>% unlist() %>% table() %>% as.data.frame() %>% .[match(stims$words[[s]],.[,1], ),] )
      print("In the burnin period:")
      print(strsplit(pairs[burn], "-") %>% unlist() %>% table() %>% as.data.frame() %>% .[match(stims$words[[s]],.[,1], ),] )
      print("In the scored part of the game:")
      print(strsplit(pairs[!burn], "-") %>% unlist() %>% table() %>% as.data.frame() %>% .[match(stims$words[[s]],.[,1], ),] )
      print("How many times each gets prompted to say in the burn-in part:")
      print(table(pairs2$say[pairs2$burnin]))
      print("How many times each gets prompted to say in the scored part:")
      print(table(pairs2$say[!pairs2$burnin]))
    }
    
    generated_stims[[s]]$pairs = pairs2
    # doublecheck
    if(any(is.na(generated_stims[[s]]$pairs)) |
       nr != nrow(generated_stims[[s]]$pairs)
    ){
      stop(paste("Stim assignement failed for some reason for game no",i))
    }
    
    
    # add columns for experiment results:
    generated_stims[[s]]$pairs$sender = rep(1:2, ceiling(nr/2))[1:nr] # for bookkeeping
    generated_stims[[s]]$pairs$sent    = as.character(NA)
    generated_stims[[s]]$pairs$guess   = as.character(NA)
    generated_stims[[s]]$pairs$correct = as.logical(NA)
    generated_stims[[s]]$pairs$sendtime = as.POSIXct(NA)
    generated_stims[[s]]$pairs$guesstime = as.POSIXct(NA)
    generated_stims[[s]]$pairs$isbaseline = isbaseline[s]
    generated_stims[[s]]$pairs$stimn = s   # stim number
  }
  attr(generated_stims, "params") = params
  return(generated_stims)
}

# Set parameters
params = list(
  wordlen_min       = 3     , # length of words from simlex to use
  wordlen_max       = 7     ,
  assocmax          = 1     , # filter out "free-associated" words ([0,10] but most are <1)
  poslist           = "N"   , # which POS to use
  nwords            = 10    , # size of natural language lexicon/meanings to use per stim set (see also nlang below!)
  nstims            = 80    , # how many stim sets to create (i.e. how many dyads will be run)
  nbaseline         = 40    , # how many stims sets (out of nstims) are in the baseline/control condition
  nsimilar          = 3     , # how many pairs to have which are > simthreshold_high
  allowduplicates   = 24,     # how many max duplicate target pairs across stims 
                              # 3 for 40 stims if 1 pair, 5 if 2)
                              # making it equal to nstims will disable the check alltogether
  simthreshold_high = 8     , # above this is a "similar" word [0,10] - this is for simlex
  simthreshold_low  = 0.2   , # below this is a "not similar" word [0,1] - this is for the vector cosine
  freeassoc_threshold=0.02  , # below this is "not freely associated" [0,1] - for the USF norms
                              # with nwords=20 and nsimilar=2 0.3 was minimum feasible, can do less if smaller lexicon  
  editmin          = 3      , # edit distance threshold for both natural and artificial words; 
                              # uses Optimal String Alignment (Levenshtein + transposition)
  allowsameinitial  = F     , # allow target pairs to have the same initial letter? (substrings are not allowed though, so e.g. hero-heroine won't be in there)
  
  #### artificial language and stims:
  #
  nlang     = c(5,2),        # size of artificial language lexicon (see also nwords above!)
  langlen   = c(2,4), # uniform length of artificial words, in number of CV syllables
  vowels    = "aeoui"       , # string of allowed vovels
  cons      = "qwtpsfhnmrl" , # string of allowed consonants
  transl    = c("vdbz", 
                "wtps")       , # letters in the English wordlist to homogenize to catch stims that are 
  # similar-sounding o English words; must be 2 strings of equal(!) length, 
  # those in 1 transformed into those in 2
  maxinitial = 2          , # times each consonant is allowed to start an artificial word per stim set (high=disables)
  constraininitials = T     , # Constrain artificial words: initial letter cannot overlap with English stims
  
  ### experiment options
  #
  #nrounds   = 114           , # number of rounds for each dyad
  pairmultiplier = c(targets=11,     # in each game in expm condition, how many pairs will be targets
                     distractors=5,  # ...distractor pairs (don't include target meanings)
                     halftargets=2,  # ...pairs of mixed target+distractor (also targets from different pairs)
                     baseline=3      # multiplier for all pairs/uniform distribution in the baseline
                     ), 
  burnin    = 3   # first (1/burnin)*(n rounds) will be training period; will attempt to balance pairs
) 



# LOAD DATA
simlex = read.csv(simlexpath, sep="\t", header=T, stringsAsFactors = F); w=unique(unlist(simlex[simlex$POS %in% params$poslist,1:2]))
engdict = readLines(dictpath) %>% gsub("/.*", "", .) %>% tolower()
freenorms = read.csv(freeassocpath, sep=",", header=T, stringsAsFactors = F, strip.white = T) %>% filter(QPS=="N" ) %>% .[,c(1:2,6:7)] %>% mutate(CUE=tolower(CUE), TARGET=tolower(TARGET), BSG=as.numeric(BSG), FSG=as.numeric(FSG)) %>% filter(CUE %in% w | TARGET %in% w) %>% mutate(score=pmax(FSG, BSG,na.rm = T)) %>% .[c(1:2,5)]
freenorms = rbind(freenorms, setnames(freenorms[, c(2,1,3)], c("CUE", "TARGET", "score"))  )

## Import and subset vectors; data.table speeds the import up
# 
simmat = fread(vectorspath, sep=" ", header=F, skip = 1, quote="")
simmat[which(is.na(simmat[,1])),1] = as.character(runif(1)) # one missing rowname for some reason
simmat = data.frame(simmat, row.names = 1)
simmat = simmat[unique(unlist(simlex[,1:2])), ]
simmat = data.matrix(simmat)
simmat = sim2(simmat)   # symmetric similarity matrix the size of the simlex lexicon


## Run stim generator:
# 
set.seed(1)
stims = list()
stims$words = wordsampler(
   params
 , simlex
 , simmat
 , freenorms
 , excludes=c("bible", "log", "suds", "god", "mood") # exclude these from simlex
 , balancetimes = 2
 )
sort(table( attr( stims$words, "globalx" ))) # how many pairs repeated in set

# artificial language - list of all possible words
genlangs = function() {
  
}

# stims$lang = list(
# 
#  langgen(
#    #params$langlen
#    4
#    , engdict
#    , params$maxinitial
#    , params$vowels
#    , params$cons
#    , params$transl)
# 
# )

stims$lang =
  
  list(langgen(
    #params$langlen
    2
    , engdict
    , params$maxinitial
    , params$vowels
    , params$cons
    , params$transl) ,
    
    langgen(
    #params$langlen
    4
    , engdict
    , params$maxinitial
    , params$vowels
    , params$cons
    , params$transl)
  )
    
    
  



stimgenvariable = function(stims_words, lang, nlang, editmin, cons, maxinitial, constraininitials){
  combi = list()
  #print(stims$lang[1])
  for (i in range(1, length(lang))) {
    #print("lang")
    #print(stims$lang[[i]])
    seti = stimgen(
      stims$words
      , stims$lang[[i]]
      , params$nlang[[i]]
      , params$editmin
      , params$cons
      , params$maxinitial
      , params$constraininitials)
    if (length(combi) == 0) {
      combi = seti
    } else{
      combi = Map(c, combi, seti)  
    }
    
  }
  return(combi)
}

stims$stims = stimgenvariable(
   stims$words
 , stims$lang
 , params$nlang
 , params$editmin
 , params$cons
 , params$maxinitial
 , params$constraininitials)

# stims$stims = stimgen(
#   stims$words
#   , stims$lang
#   , params$nlang
#   , params$editmin
#   , params$cons
#   , params$maxinitial
#   , params$constraininitials)

#view(stims)

for(i in seq_along(stims$words)){
  print(paste(stims$words[[i]], collapse=" "), quote=F)
  print(paste(stims$stims[[i]], collapse=" "), quote=F)
  print(" ", quote=F)
}
# 

generated_stims = stimcombinator(stims, params)
view(generated_stims)
c(sapply(generated_stims, function(x) x$targets)) %>% table() %>% sort()

saveRDS(generated_stims, file=file.path(DROPDIR, "generated_stims.RDS"))


## Generate bookkeeping file
#
bookkeeping = data.frame(nexp = 1:length(generated_stims),
                         isbaseline=sapply(generated_stims, function(x) x$pairs$isbaseline[1]),
                         starttime = as.POSIXct(NA), 
                         endtime = as.POSIXct(NA),
                         f1 = as.character(NA),
                         f2 = as.character(NA),
                         names = as.character(NA),
                         code1 = codesgen(length(generated_stims), 1),
                         code2 = codesgen(length(generated_stims), 2),
                         stringsAsFactors = F
                         )
saveRDS(bookkeeping, file="bookkeeping.RDS")


