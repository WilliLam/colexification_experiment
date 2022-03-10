generated_stims <- readRDS("~/R colex participant experiment/generated_stims.RDS")


firstTrial <- generated_stims[[1]]

pairs = data.frame(firstTrial[5])



x <- data.frame(pairs["pairs.pair1"], pairs["pairs.pair2"], pairs["pairs.say"])
stims <- firstTrial["stims"]
words <- firstTrial["words"]

write.csv(stims, file="~/R colex participant experiment/stims_first.csv")
write.csv(words, file="~/R colex participant experiment/words_first.csv")
write.csv(x, file="~/R colex participant experiment/pairs_first.csv")

secondTrial <- generated_stims[[2]]

pairs = data.frame(secondTrial[5])



x <- data.frame(pairs["pairs.pair1"], pairs["pairs.pair2"], pairs["pairs.say"])
stims <- secondTrial["stims"]
words <- secondTrial["words"]

write.csv(stims, file="~/R colex participant experiment/stims_target.csv")
write.csv(words, file="~/R colex participant experiment/words_target.csv")
write.csv(x, file="~/R colex participant experiment/pairs_target.csv")
