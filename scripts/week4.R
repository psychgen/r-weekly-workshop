rm(list = ls())

set.seed(080318)

# Example for today:
# Polygenic scores from parent-offspring trios can be used to address importance of direct / indirect genetic effects
# Kong et al. (The nature of nurture: Effects of parental genotypes) proposed a method based on transmitted/non-transmitted alleles
# In a recent meeting with many of you, we discussed how this approach is different from multiple regression with all trio genotypes
# I don't know, but it may help to simulate it. I would do it like this:

# simulation parameters
ntrio = 20000 # number of trios
nsnp = 500 # number of snps
maf = 0.2 # allele frequency
g = 0.7 # genetic effects
e = 0.4 # environmental effect
l = 0.15 # nurture effect

# allele effects
var_gi = 2 * maf * (1 - maf)
var_bi = 1 / nsnp / var_gi
b = sqrt(var_bi) * rnorm(nsnp)

# maternal alleles (sample number of copies of "reference allele")
# T alleles are transmitted, NT alleles are not
T_m = matrix(rbinom(ntrio * nsnp, 1, maf), ntrio, nsnp)
NT_m = matrix(rbinom(ntrio * nsnp, 1, maf), ntrio, nsnp)

# paternal alleles
T_p = matrix(rbinom(ntrio * nsnp, 1, maf), ntrio, nsnp)
NT_p = matrix(rbinom(ntrio * nsnp, 1, maf), ntrio, nsnp)

# polygenic scores
poly_T_m = T_m %*% b # maternal transmitted pgs
poly_T_p = T_p %*% b # paternal transmitted pgs
poly_NT_m = NT_m %*% b # maternal non-transmitted pgs
poly_NT_p = NT_p %*% b # paternal non-transmitted pgs

poly_T = poly_T_m + poly_T_p # transmitted pgs (offspring pgs)
poly_NT = poly_NT_m + poly_NT_p # non-transmitted pgs

# make phenotypes
# offspring phenotype depends on their pgs, parental pgs and error
y_o = g * poly_T + l * (poly_T + poly_NT) + e * rnorm(ntrio)

# The question is now how these two alternative approaches recovers the generating parameters

# T / NT method
mo1 = lm(y_o ~ poly_T + poly_NT)
est1 = c(coef(mo1)[2] - coef(mo1)[3],
         coef(mo1)[3])
vc1 = vcov(mo1)
se1 = c(sqrt(vc1[2, 2] + vc1[3, 3] - 2 * vc1[3, 2]),
        sqrt(vc1[3, 3]))

# control method
poly_m = poly_T_m + poly_NT_m # maternal pgs
poly_p = poly_T_p + poly_NT_p # paternal pgs
mo2 = lm(y_o ~ poly_T + I(poly_m + poly_p))
est2 = coef(mo2)[2:3]
se2 = sqrt(diag(vcov(mo2)[2:3, 2:3]))

# compare results
res = data.frame(est = c(est1, est2),
                 se = c(se1, se2),
                 effect = rep(c("direct", "indirect"), times = 2),
                 method = rep(c("T/NT", "control"), each = 2))
# Looks similar
res
