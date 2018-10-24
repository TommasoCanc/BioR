################################################################################
osf.bior <- function(file_name, ext = "csv", destination_path, object = F, savobj = T)
{
  require(httr)
  require(rjson)
  
  ### Creation code to save data-connection ####################################
  num <- sample(seq(from = 0, to = 9), 4)
  LET <- sample(LETTERS[seq( from = 1, to = 26 )], 2)
  let <- sample(letters[seq( from = 1, to = 26 )], 2)
  
  cod <- paste(num, LET, let, collapse = "", sep="")
  
  
  setwd(destination_path)
  Access_Token <- "https://osf.io/fwj7s/?view_only=5abadd002944472c9912fae21cf1526a"
  all_file_name <- paste0(paste("db_"),file_name,paste(".",sep="",ext))
  
  ### Checking if the parameters object and saveobj are good ##################
  if (object == T & savobj == T | object == F & savobj == F) {
    print("Please check again your object and savobj parameters")
  }
  
  ### If-Else choose DB to download ##############################################
  if (all_file_name == "db_araneae.csv") {
    GUID <-  "mgnjr"
  } else if (all_file_name == "db_bivalvia.csv") {
    GUID <-  "n3kgh"
  } else if (all_file_name == "db_branchiobdellida.csv") {
    GUID <-  "94cju"
  } else if (all_file_name == "db_bryozoa.csv") {
    GUID <-  "7t56a"
  } else if (all_file_name == "db_coelenterata.csv") {
    GUID <-  "9jb5f"
  } else if (all_file_name == "db_coleoptera.csv") {
    GUID <-  "jgcwt"
  } else if (all_file_name == "db_crustacea.csv") {
    GUID <-  "rav2e"
  } else if (all_file_name == "db_diptera.csv") {
    GUID <-  "8mzta"
  } else if (all_file_name == "db_entoprocta.csv") {
    GUID <-  "ykhxz"
  } else if (all_file_name == "db_ephemeroptera.csv") {
    GUID <-  "c4y2q"
  } else if (all_file_name == "db_gasteropoda.csv") {
    GUID <-  "rnmq7"
  } else if (all_file_name == "db_heteroptera.csv") {
    GUID <-  "sy59b"
  } else if (all_file_name == "db_hirudinea.csv") {
    GUID <-  "kxfjp"
  } else if (all_file_name == "db_hydrachnidia.csv") {
    GUID <-  "74cx9"
  } else if (all_file_name == "db_hymenoptera.csv") {
    GUID <-  "yejnr"
  } else if (all_file_name == "db_lepidoptera.csv") {
    GUID <-  "n6rz7"
  } else if (all_file_name == "db_megaloptera.csv") {
    GUID <-  "9rfp7"
  } else if (all_file_name == "db_nematoda.csv") {
    GUID <-  "xsc2d"
  } else if (all_file_name == "db_nematomorpha.csv") {
    GUID <-  "dk2t8"
  } else if (all_file_name == "db_nemertini.csv") {
    GUID <-  "6tcwu"
  } else if (all_file_name == "db_neuroptera.csv") {
    GUID <-  "x4f6u"
  } else if (all_file_name == "db_odonata.csv") {
    GUID <-  "z3mhu"
  } else if (all_file_name == "db_oligochaeta.csv") {
    GUID <-  "fgjct"
  } else if (all_file_name == "db_plecoptera.csv") {
    GUID <-  "hyskr"
  } else if (all_file_name == "db_polychaeta.csv") {
    GUID <-  "qnje2"
  } else if (all_file_name == "db_porifera.csv") {
    GUID <-  "u8mz4"
  } else if (all_file_name == "db_trichoptera.csv") {
    GUID <-  "v9zfn"
  } else if (all_file_name == "db_turbellaria.csv") {
    GUID <-  "zn3be"
  }
  
  
  
  
  ### Object ########################################################
  if (object == T & savobj == F) {
    
    ### Connection with OSF ########################################################
    GETurl <- paste0("https://api.osf.io/v2/files/",GUID)
    
    req <- GET(GETurl, write_disk(cod,overwrite=T))
    json_data <- fromJSON(file = cod)
    
    if (length(json_data$data) > 0){
      req1 <- GET(json_data$data$links$download,
                  write_disk(all_file_name, overwrite = TRUE))
      print("The file has been downloaded like an object in your R workspace")
    }
    else if (length(Access_Token) == 1){
      if (grepl("https://osf.io",Access_Token)==TRUE){
        req1 <- GET(paste0("https://api.osf.io/v2/files/",GUID,"/",gsub(".*/","",Access_Token)),
                    write_disk(cod, overwrite = TRUE))
        json_data <- fromJSON(file = cod)
        if (length(json_data$data) > 0){
          req1 <- GET(json_data$data$links$download,
                      write_disk(all_file_name, overwrite = TRUE))
          print("The file has been downloaded like an object in your R workspace")
        }
        else{
          print(json_data$errors[[1]]$detail[1])
        }
      }
      else if (grepl("https://osf.io",Access_Token)==FALSE){
        req1 <- GET(paste0("https://api.osf.io/v2/files/",GUID),
                    write_disk(cod, overwrite = TRUE),
                    add_headers("Authorization" = paste0("Bearer ",Access_Token)))
        json_data <- fromJSON(file = cod)
        if (length(json_data$data) > 0){
          req1 <- GET(json_data$data$links$download,
                      write_disk(all_file_name, overwrite = TRUE),
                      add_headers("Authorization" = paste0("Bearer ",Access_Token)))
          print("The file has been downloaded like an object in your R workspaces")
        }
        else{
          print(json_data$errors[[1]]$detail[1])
        }
      }
      else{
        print(json_data$errors[[1]]$detail[1])
      }
    }
    else{
      print(json_data$errors[[1]]$detail[1])
    }
    
    ### Create object and delete file in your sistem ##########################
    macro <- read.csv(paste0(destination_path, "/", all_file_name), header = T, sep = ";")
    file.remove(paste0(destination_path, "/", all_file_name))
  } 
  
  ### Save & Object ########################################################
  if (object == F & savobj == T) {
    
    ### Connection with OSF ########################################################
    GETurl <- paste0("https://api.osf.io/v2/files/",GUID)
    req <- GET(GETurl, write_disk(cod,overwrite=T))
    json_data <- fromJSON(file = cod)
    
    if (length(json_data$data) > 0){
      req1 <- GET(json_data$data$links$download,
                  write_disk(all_file_name, overwrite = TRUE))
      print(paste0("The file has been downloaded to your working directory as: ",
                   all_file_name))
      print("The file has been downloaded like an object in your R workspace")
    }
    else if (length(Access_Token) == 1){
      if (grepl("https://osf.io",Access_Token)==TRUE){
        req1 <- GET(paste0("https://api.osf.io/v2/files/",GUID,"/",gsub(".*/","",Access_Token)),
                    write_disk(cod, overwrite = TRUE))
        json_data <- fromJSON(file = cod)
        if (length(json_data$data) > 0){
          req1 <- GET(json_data$data$links$download,
                      write_disk(all_file_name, overwrite = TRUE))
          print(paste0("The file has been downloaded to your working directory as: ",
                       all_file_name))
          print("The file has been downloaded like an object in your R workspace")
        }
        else{
          print(json_data$errors[[1]]$detail[1])
        }
      }
      else if (grepl("https://osf.io",Access_Token)==FALSE){
        req1 <- GET(paste0("https://api.osf.io/v2/files/",GUID),
                    write_disk(cod, overwrite = TRUE),
                    add_headers("Authorization" = paste0("Bearer ",Access_Token)))
        json_data <- fromJSON(file = cod)
        if (length(json_data$data) > 0){
          req1 <- GET(json_data$data$links$download,
                      write_disk(all_file_name, overwrite = TRUE),
                      add_headers("Authorization" = paste0("Bearer ",Access_Token)))
          print(paste0("The file has been downloaded to your working directory as: ",
                       all_file_name))
          print("The file has been downloaded like an object in your R workspace")
        }
        else{
          print(json_data$errors[[1]]$detail[1])
        }
      }
      else{
        print(json_data$errors[[1]]$detail[1])
      }
    }
    else{
      print(json_data$errors[[1]]$detail[1])
    }
    
    ### Create object in your R workspace #####################################
    macro <- read.csv(paste0(destination_path, "/", all_file_name), header = T, sep = ";")
  } 
  
  ### Return object macro and delete file cod #####################################################
  file.remove(paste0(destination_path, "/", cod))  
  return(macro)
}
################################################################################

