library(dplyr)

#' Download posterior files associated with PEcAn
#'
#' @param pft the name of the pft that this data is applicable to
#' @param hostname
#' @return data.frame with the id, filename and pathname of the posterior that is requested
#' @author Nihar Sanda
#* @get /pft/<pft>/hostname/<hostname>/
get.posterior.file <- function(req, pft, hostname, res) {
  #Finding the appropriate pft
  Pft <- tbl(global_db_pool, "pfts") %>%
    select(id, name) %>%
    filter(name == !!pft)
  
  pft <- Pft %>% collect()
  pftid <- pft$id
  print(pftid)
  
  #find appropriate posterior
  Posterior <- tbl(global_db_pool, "posteriors") %>%
    select(id, pft_id) %>%
    filter(pft_id == !!pftid)
  
  posterior <- Posterior %>% collect()

  if (nrow(posterior) == 0) {
    res$status <- 404
    return(list(error="Posterior with specified information was not found"))
  }
  else {
    type <- "Posterior"
    containerid <- posterior$id
    
    # Finding the host_id from the machines table
    Host <- tbl(global_db_pool, "machines") %>%
      select(everything()) %>%
      filter(hostname == !! hostname)
    
    host <- Host %>% collect()
    hostid <- host$id
    
    # Finding file in dbfiles
    Dbfile <- tbl(global_db_pool, "dbfiles") %>%
      select(everything()) %>%
      filter(container_type == !!type  && container_id == 523 && machine_id == !!hostid)
    
    dbfiles <-  Dbfile %>% collect()  
    
    #for (dbfile in dbfiles) {
      #print(paste0(dbfile$file_path), "/", dbfile$file_name)
      #if(! dbfile.exists(paste0(dbfile$file_path), "/", dbfile$file_name)){
      #  return(list(status = "Error", message = "File not found"))
      #}
    #}
    
    # Check if the file path is valid
    #if(! file.exists(filepath)){
    #  return(list(status = "Error", message = "File not found"))
    #}
    
    res <- list(
      pft_id = pftid,
      machine_id = hostid,
      container_id = containerid,
      container_type = type,
      dbfiles = dbfiles,
      host = host
    )
  }
}
