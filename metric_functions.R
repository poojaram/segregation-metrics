library('dplyr')

# reffered to https://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# function to read all the data files into a list
read_all_files <- function() {
  files <- list.files("data/prepped")
  all_csv <- lapply( paste0("data/prepped/",files),read.csv)
  names(all_csv) <- gsub(".csv","",
                         list.files("data/prepped",full.names = FALSE),
                         fixed = TRUE)
  return(all_csv)
}

# converts list to matrix
model_data <-  function(list) {
  df = as.data.frame(list)
  new = t(df)
  colnames(new) = c('Value')
  return (new)
}

#converts matrix to df, and remodels the data
to_df = function(df) {
  a = data.frame(df)
  a = mutate(a, City = unlist(lapply(gsub("_race", "", row.names(df)), simpleCap))) %>% arrange(-Value)
  row.names(a) <- lapply(gsub("_race", "", row.names(df)), simpleCap)
  a = data.frame(a)
  return(a)
}

# function to compute dissimilarity index
compute_dissimilarity <- function(df) {
  Total = sum(df$total_pop)
  X = sum(df$pop_not_white)
  P = X/Total
  return(
    (sum(df$total_pop * abs((df$pop_not_white/df$total_pop) - P)))/(2*Total*P*(1-P))
  )
}

# function to compute interaction index
compute_interaction <- function(sub_data) {
  X = sum(sub_data$pop_not_white)
  return (sum((sub_data$pop_white/ sub_data$total_pop) * (sub_data$pop_not_white/X )))
}

# function to compute isolation index
compute_isolation <-  function(df) {
  X = sum(df$pop_not_white)
  return (sum( (df$pop_not_white/ df$total_pop) * (df$pop_not_white/X )  ))
}

# function to correlation index
compute_correlation <- function(df) {
  Total = sum(df$total_pop)
  X = sum(df$pop_not_white)
  P = X/Total
  I = compute_isolation(df)
  return((I-P)/(1-P))
}

# function to compute new index
compute_new <- function(df) {
  return(0.5 * compute_correlation(df) + 0.5 * compute_dissimilarity(df))
}





