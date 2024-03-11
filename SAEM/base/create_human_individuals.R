
create_human_individuals <- function(dataset,
                                     sep = ",",
                                     file_encoding = "UTF-8",
                                     weight_unit = c("kg", "g"),
                                     height_unit = c("m", "dm", "cm"),
                                     age_unit = c("year(s)", "day(s)"),
                                     progress = TRUE,
                                     fromFile = TRUE) {
 
  weight_unit <- match.arg(weight_unit)
  height_unit <- match.arg(height_unit)
  age_unit <- match.arg(age_unit)
  
  if(fromFile) data <- read.table(dataset, header = TRUE, sep = sep, encoding = file_encoding)
  else data <- dataset
  header <- colnames(data)
  header <- toupper(header)
  header <- trimws(header)
  
  # find columns
  id_col <- grepl("^ID$", header)
  age_col <- grepl("^AGE", header)
  sex_col <- grepl("^SEX|^GENDER", header)
  weight_col <- grepl("^WEIGHT", header)
  height_col <- grepl("^HEIGHT", header)
  pop_col <- grepl("^POPULATION$", header)
  
  # error handling
  if (sum(id_col) != 1)
    stop("Error identifying ID column", call. = FALSE)
  
  if (sum(age_col) != 1)
    stop("Error identifying AGE column", call. = FALSE)
  
  if (sum(sex_col) != 1)
    stop("Error identifying SEX/GENDER column", call. = FALSE)
  
  if (sum(weight_col) != 1)
    stop("Error identifying WEIGHT column", call. = FALSE)
  
  if (sum(height_col) != 1)
    stop("Error identifying HEIGHT column", call. = FALSE)
  
  if (sum(pop_col) != 1)
    stop("Error identifying POPULATION column", call. = FALSE)
  
  id_col <- which(id_col)
  age_col <- which(age_col)
  sex_col <- which(sex_col)
  weight_col <- which(weight_col)
  height_col <- which(height_col)
  pop_col <- which(pop_col)
  
  # create individuals
  data <- data %>% 
    group_by_at(id_col) %>% 
    slice(1) %>% 
    ungroup() %>%
    arrange_at(id_col)
  
  sex_matches <- toupper(unname(unlist((ospsuite::Gender))))
  pop_matches <- toupper(unname(unlist((ospsuite::HumanPopulation))))
  
  error_ids <- c()
  good_ids <- c()
  
  
  .create_ind <- function(x) {
    
    # TODO: Error handling here
    age <- as.numeric( x[age_col] )
    id <- as.numeric( x[id_col] )
    weight <- as.numeric( x[weight_col] )
    height <- as.numeric( x[height_col] )
    sex <- toupper( trimws( x[sex_col] ) )
    pop <- toupper( trimws( x[pop_col]) )
    
    sex_id <- which(grepl(paste0("^", sex, "$"), sex_matches))
    pop_id <- which(grepl(pop, pop_matches))
    
    sex <- ospsuite::Gender[[sex_id]]
    pop <- ospsuite::HumanPopulation[[pop_id]]
    
    res <- tryCatch(
     create_human_individual(gender = sex,
                              age = age,
                              age_unit = age_unit,
                              population = pop,
                              weight = weight,
                              weight_unit = weight_unit,
                              height = height,
                              height_unit = height_unit),
      error = function(e) { 
        error_ids <- c(error_ids, id)
        return(NULL) 
      }
    )
    
    if (progress)
      pb$tick()
    
    return(res)
  }
  
  if (progress)
    pb <- progress_bar$new(total = nrow(data), 
                           format = "  Individuals [:bar] :percent eta: :eta",
                           width = 80)
  
  individuals <- apply(data, 1, .create_ind)
  
  ids <- data[[id_col]]
  if (length(error_ids) > 0) {
    warning(paste("Could not create individuals with ids:", paste(error_ids, collapse = ", ")))
    ids <- setdiff(ids, error_ids)
  }
  
  names(individuals) <- paste("ID", ids)
  
  return(individuals)
}


#' Convenience function to avoid testing for null. It returns the first object if it is not null otherwise the second object
#' @name OR
#'
#' @param lhs Object that will be returned if not NULL
#' @param rhs Object that will be returned if \code{lhs} is NULL. It maybe well be NULL
#'
#' @return The first parameter if it is not NULL otherwise the second parameter
#' @export
`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) {
    lhs
  } else {
    rhs
  }
}