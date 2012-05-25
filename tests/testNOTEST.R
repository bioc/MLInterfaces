
library(MLInterfaces)

    rf1 = try(MLearn(sp~CW+RW, data=crabs, randomForestI, xvalSpec("NOTEST"), ntree=600 ))
    if (inherits(rf1, "try-error")) stop("xvalSpec('NOTEST') not working as intended with randomForestI")

    rf2 = try(MLearn(sp~CW+RW, data=crabs, randomForestI, 1:nrow(crabs), ntree=600 ))
    if (inherits(rf2, "try-error")) stop("xvalSpec('NOTEST') not working as intended with randomForestI")

library(ALL)
data(ALL)
#
# restrict to BCR/ABL or NEG
#
bio <- which( ALL$mol.biol %in% c("BCR/ABL", "NEG"))
#
# restrict to B-cell
#
isb <- grep("^B", as.character(ALL$BT))
kp <- intersect(bio,isb)
all2 <- ALL[,kp]
mads = apply(exprs(all2),1,mad)
kp = which(mads>1)  # get around 250 genes
vall2 = all2[kp, ]
vall2$mol.biol = factor(vall2$mol.biol) # drop unused levels

rd1 = MLearn(mol.biol~., vall2, rdacvI, xvalSpec("NOTEST"))
if (inherits(rd1, "try-error")) stop("xvalSpec('NOTEST') not working as intended with rdacvI")

# following can be used to check whether we warn suitably for unanticipated NOTEST usage in xvalSpec
#owarn = options()$warn
#options(warn=2)
#lk = try(MLInterfaces:::.check_NOTEST_COMPATIBILITY( svmI, xvalSpec("NOTEST") ))
#if (!inherits(lk, "try-error")) stop(".check_NOTEST_COMPATIBILITY does not flag svmI")
#options(warn=owarn)
