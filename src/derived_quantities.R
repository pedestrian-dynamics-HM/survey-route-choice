library(FSA)


round_df <- function(df, prec=4){

  df <- data.frame(lapply(df, function(x) if(is.numeric(x)) round(x, prec) else x))
  return(df)

}


get_route_probabilities_from_data_frame <- function(data_frame) {

  data_frame <- data_frame[, c("RouteA", "RouteB", "RouteC")]

  for (ii in colnames(data_frame)) {
    data_frame[[ii]] <- as.numeric(data_frame[[ii]])
  }

  return(data_frame)
}

fav_routes.get_all_routes <- function(route_probs) {

  max_val <- matrixStats::rowMaxs(as.matrix(route_probs))
  is_max <- route_probs == max_val

  favoriteRoute <- c("")
  for (row in seq_len(nrow(is_max))) {
    favoriteRoute[row] <- toString(names(Filter(any, is_max[row,])))
  }

  return(favoriteRoute)
}

fav_routes.get_nr_fav_routes <- function(route_probs) {

  max_val <- matrixStats::rowMaxs(as.matrix(route_probs))
  is_max <- route_probs == max_val
  nr_favs <- as.integer(rowSums(1 * (is_max)))
  return(nr_favs)

}

get_single_route <- function(route_probs, favorite = "last") {
  # last corresponds to shortest (default bahvior)
  return(colnames(route_probs)[max.col(route_probs, ties.method = favorite)])
}


fav_routes.get_shortest_route <- function(route_probs) {
  return(get_single_route(route_probs = route_probs, favorite = "last"))
}

fav_routes.get_longtest_route <- function(route_probs) {
  return(get_single_route(route_probs = route_probs, favorite = "first"))
}


derive_favorite_route_from_likelihoods <- function(df) {

  route_probs <- get_route_probabilities_from_data_frame(data_frame = df)


  route_choice <- data.frame(fav_routes.counts = fav_routes.get_nr_fav_routes(route_probs),
                             fav_routes.choice = fav_routes.get_all_routes(route_probs),
                             fav_routes.shortest_pref = fav_routes.get_shortest_route(route_probs),
                             fav_routes.longest_pref = fav_routes.get_longtest_route(route_probs)
  )

  order <- c("RouteA", "RouteB", "RouteC", "RouteA, RouteB", "RouteB, RouteC", "RouteA, RouteC", "RouteA, RouteB, RouteC")
  route_choice$fav_routes.choice = factor(route_choice$fav_routes.choice, levels = order)

  return(route_choice)
}


rename_Routex_to_route <- function(output){

  output <- output %>% dplyr::rename(long = RouteA)
  output <- output %>% dplyr::rename(medium = RouteB)
  output <- output %>% dplyr::rename(short = RouteC)
}

anaylze_ranking_df <- function(df, condition="uninformed"){

  alpha <- 0.05


  if (condition!="uninformed"){
    df <- droplevels(df[df$condition == condition,])
  }

  df <- df %>% dplyr::select("RouteA" | "RouteB" | "RouteC" )
  df <- rename_Routex_to_route(df)

  means <- df %>% summarise_if(is.numeric, mean)
  means2 <- means

  df <-melt(df, time.var = colnames(df), variable.name = 'Route', value.name = "Likelihood")



  dunntest <- FSA::dunnTest(Likelihood ~ Route, data=df)

  combinations <- list( c("short", "long"), c("short", "medium"), c("medium", "long") )
  combinations <- dunntest$res$Comparison

  equal <- ""

  res <- data.frame(combination = c(), means = c(),pval_MannWhitneyU = c(), isDifferent_MannWhitneyU = c())
  for (combi in combinations){
    c1 <- str_split(combi, " - ")[[1]][1]
    c2 <- str_split(combi, " - ")[[1]][2]

    res1 <- wilcox.test(Likelihood ~ Route, data = df, na.rm = TRUE, paired = FALSE, exact = FALSE, conf.int = TRUE, subset = Route %in% c(c1, c2))
     # Wilcoxon rank sum test = Mann-Whitney test is performed if "paired = FALSE" in wilcox.test
    val <- res1$p.value
    is_different <- dunntest$res[dunntest$res$Comparison == combi, ]$P.adj <= alpha # use Dunns result instead of MannWhitneyU

    if (is_different == FALSE){
      equal <- paste(equal, paste(c1, c2, sep=" = " ), sep=", ")

      if(c2 %in% colnames(means2))
      {
        means2 <- dplyr::select(means2, -c(c2))
      }
      else{
         if(c1 %in% colnames(means2))
            {
              means2 <- dplyr::select(means2, -c(c1))
            }
      }




    }



    mean_str <- str_flatten(c( round(means[c1], 2) , round(means[c2], 2)), collapse = ", ")

    df_ <- data.frame(combination = c(combi),
                      means = c(mean_str),
                      pval_MannWhitneyU = c(val),
                      isDifferent_MannWhitneyU = c(val <= alpha)
    )

    res <- rbind(res, df_)
  }

  res$pval_Dunn = dunntest$res$P.adj
  res$isDifferent_Dunn = dunntest$res$P.adj <= alpha


  kruskal <- df %>% kruskal_test(Likelihood ~ Route)
  res$pval_KruskalWallis <-kruskal$p
  res$isDifferent_KruskalWallis <-kruskal$p <= alpha


  if (ncol(means2)== 1 & all(c(res$isDifferent_KruskalWallis) == TRUE)){
    temp_var <- res$combination[res$isDifferent_Dunn == TRUE]

    c1 <- str_split(temp_var, " - ")[[1]][1]
    c2 <- str_split(temp_var, " - ")[[1]][2]
    means2 <- dplyr::select(means, c(c1,c2))
  }

  prio <- sort(means2, decreasing = TRUE)

  res$prio <- str_flatten(colnames(prio),collapse = " > ")

  res$equal <- substring(equal, 3)

  res$posthocERROR <- res$isDifferent_Dunn != res$isDifferent_MannWhitneyU

  res$posthocERROR[res$posthocERROR == TRUE] <- "MannWhitney and Dunn's test differ."
  res$posthocERROR[res$posthocERROR == FALSE] <- ""


  if (all(c(res$isDifferent_KruskalWallis) == FALSE)){
    res$prio <- ""
    res$equal <- ""
  }

  res$condition <- condition
  res <- res[,order(ncol(res):1)]
  res$mean.short <- means$short
  res$mean.medium <- means$medium
  res$mean.long <- means$long

  return(res)
}

