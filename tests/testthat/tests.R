context("Correct Data Structures Returned")

# reg
reg(ChickWeight, weight ~ Time + Diet)

# reg
reg(ChickWeight, weight ~ Time + Diet, vcov_type = "const")

# reg, vce(robust)
reg(ChickWeight, weight ~ Time + Diet, vcov_type = "HC0")

# reg, vce(boot)
reg(ChickWeight, weight ~ Time + Diet, vcov_type = "boot")

# reg, vce(cluster Chick)
reg(ChickWeight, weight ~ Time + Diet, vcov_cluster = ~ Chick)

# bootstrap, cluster(Chick) reps(5000): reg
#reg(ChickWeight, weight ~ Time + Diet, vcov_cluster = ~ Chick, vcov_type = "boot")

# svy: reg
library("survey")
data(api)
dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
reg(dstrat, api00 ~ ell + meals + mobility)

