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

write.csv(stims, file="~/R colex participant experiment/stims_second.csv")
write.csv(words, file="~/R colex participant experiment/words_second.csv")
write.csv(x, file="~/R colex participant experiment/pairs_second.csv")

thirdTrial <- generated_stims[[3]]

pairs = data.frame(thirdTrial[5])



x <- data.frame(pairs["pairs.pair1"], pairs["pairs.pair2"], pairs["pairs.say"])
stims <- thirdTrial["stims"]
words <- thirdTrial["words"]

write.csv(stims, file="~/R colex participant experiment/stims_3.csv")
write.csv(words, file="~/R colex participant experiment/words_3.csv")
write.csv(x, file="~/R colex participant experiment/pairs_3.csv")

