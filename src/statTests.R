
library(FSA)

mannWhitneyUTestWrapper <- function(variables, cond1, cond2, route) {

  output1 <- droplevels(variables[variables$condition == cond1,])
  output2 <- droplevels(variables[variables$condition == cond2,])
  variables_ <- rbind(output1, output2)

  # Otherwise, if both x and y are given and paired is FALSE, a Wilcoxon rank sum test
  # (equivalent to the Mann-Whitney test: see the Note) is carried out.
  # In this case, the null hypothesis is that the distributions of x and y differ by a
  # location shift of mu and the alternative is that they differ by some other location shift
  # (and the one-sided alternative "greater" is that x is shifted to the right of y).
  # https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/wilcox.test
  m1 <- wilcox.test(eval(as.symbol(route)) ~ condition, data = variables_, na.rm = TRUE, paired = FALSE, exact = FALSE, conf.int = TRUE)

  return(m1)
}





kruskalwallisWrapper <- function(df) {
  res <- df %>% kruskal_test(Likelihood ~ Route)
  PT <- dunnTest(Likelihood ~ Route, data=df, method="bh")
  return(res)
}