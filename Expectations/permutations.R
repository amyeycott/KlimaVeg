library(permute)

n_mountains <- 14
mountains <- gl(n = n_mountains, k = 2)# must be factor - safer to use real mountain names
old_new <- gl(n = 2, k = 1, length = n_mountains * 2)#should be factor



## blocks
CTRL <- how(blocks = mountains, complete = TRUE, maxperm = Inf)
check(1:length(mountains), control = CTRL) # how many possible permutations



#Some example permutations
set.seed(42)
shuffleSet(1:length(mountains), nset = 10, control = CTRL)

#expected ordination code
mod <- rda(spp ~ old_new + Condition(mountains))#partial out effect of mountain
anova(mod, permutations = CTRL)

## strata in plots - gives similar permutations
CTRL <- how(plots = Plots(strata = mountains))
check(1:length(mountains), control = CTRL)
set.seed(42)
shuffleSet(1:length(mountains), nset = 10, control = CTRL)

##Patryk's
h<-how(within=Within(type="series", constant=TRUE), plots=Plots(strata=mountains, type="free"))
shuffleSet(1:length(mountains), nset = 10, control = h)#not appropriate

