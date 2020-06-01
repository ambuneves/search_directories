search.directories <- function(dir = NULL, file_type, folders = NULL) {
  # Set up directory
  if (is.null(dir) == TRUE){
    path <- getwd() # current directory
  }
  else {
    path <- dir # we indicate which directory
  }
  # set up which file type you are looking for
  file <- paste0(".",file_type) # same as paste(".",file_type, sep = ""), but simpler
  # Set up subfolder
  if (is.null(folders) == TRUE){   #option a (NULL), default: look at all the subfolder within that directory
    subfolders <- list.dirs(path = path)[-1] # if you set is at 'path' instead of current directory, it is simpler and you do not need to setwd(path)
    # As well as that, you end up getting a vector with full subfolder paths instead of relative paths
  }
  else {  # option b: give a vector of subfolders' relative path
    subfolders <- folders # what you had was good, but this is simpler (and shorter code). You just need to make sure the vector has the full subfolder paths
  }
  #Set up the paths that the loop will use to search for files  
  # subpath <- vector(mode="character", length=length(subfolders)) We don't need this'
  volume_data <- matrix(data=NA, nrow=0, ncol=4) #Initialize the matrix 
  colnames(volume_data) <- c("specimen","filename", "folder", paste(file_type, "path", sep = "_"))
  # It would be better to create a temp matrix with the 4 columns you want and then append it.
  for (i in 1:length(subfolders)) {
    filename <- list.files(subfolders[i], pattern = file)
    if (length(filename) > 0){
      specimen <- gsub(pattern=file, replacement="", x=filename)
      folder <- rep(subfolders[i], length(filename))
      file_path <- paste0(folder, file)
      temp_matrix <- cbind(specimen, filename, folder, file_path)
      volume_data <- rbind(volume_data, temp_matrix)
      rm(filename, specimen, folder, file_path, temp_matrix)
    }
    else {
      rm(filename)
    }
  }
  df_all <- as.data.frame(volume_data)
  return(df_all)
}


#Down here is the original code 

# # subpath <- vector(mode="character", length=(length(subfolders))) #this is supposed to be blank... i think
# # volume_data <- matrix(data=NA, nrow=0, ncol=4)
# # colnames(volume_data) <- c("specimen", "filename", "folder", "full_path")
# 
# for (i in 1: length(subfolders)){
#   subpath[i] <- paste0(path, sub(".", "", subfolders[i])) # we are removing the first character in the subfolder path
#   all.files <- list.files(path = subpath[i])
#   aim.ids <- grep(pattern = paste0("\\", file_type), x = all.files, ignore.case = T)
#   aim.files <- all.files[aim.ids]
#   full_path <- Sys.glob(file.path(subpath[i], aim.files)) # are they all .aim, or could they be in caps? Double-check
#   setwd(subpath[i])
#   #filename <- list.files(pattern="*.aim$") # the dollar sign makes sure that it is the end of the string only
#   folder <- rep(substr(subfolders[i], start = 3, stop=nchar(subfolders[i])), length(aim.files)) # just the folder name
#   # seq <- grep(pattern = paste0("\\", file_type, "\\.1"), x = aim.files, ignore.case = T)
#   # for (j in seq){
#   #   name <- aim.files[[j]]
#   #   name_new <- gsub('.{2}$', '', name)
#   #   aim.files[[j]] <- name_new
#   # }
#   specimen <- gsub('.{4}$', '', aim.files) # potential specimen name? Need to check with Wei and Jay. Here we extract 4 characters at the end
#   setwd(path)
#   temp <- cbind.data.frame(specimen, aim.files, folder, full_path)
#   volume_data <- rbind(volume_data, temp)
# }
# 
# df_all <- as.data.frame(volume_data)