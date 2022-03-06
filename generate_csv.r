generated_stims <- readRDS("~/colexification_experiment/generated_stims.RDS")


firstTrial <- generated_stims[[1]]

pairs = data.frame(firstTrial[5])



x <- data.frame(pairs["pairs.pair1"], pairs["pairs.pair2"], pairs["pairs.say"])
stims <- firstTrial["stims"]
words <- firstTrial["words"]

write.csv(stims, file="~/colexification_experiment/stims.csv")
write.csv(words, file="~/colexification_experiment/words.csv")
write.csv(x, file="~/colexification_experiment/pairs.csv")
