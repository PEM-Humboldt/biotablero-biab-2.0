#Function load_libraries: load or install the necessary libraries
#Arguments:
#libraries: a character vector for the libraries needed

load_libraries = function(libraries){
  
  #Identify if libraries required are already installed
  new.packages = libraries[!(libraries %in% installed.packages()[, "Package"])]
  
  #Install libraries if needed
  if(length(new.packages))install.packages(new.packages)
  
  #Load libraries
  for(i in libraries){library(i, character.only=T)}
}

