# FUNC: Unregister cores ####

# Purpose: removes parallel processing nodes from active use
# to avoid hidden R processes from crashing the session

unregisterCores <- function() {
  
  env <- foreach:::.foreachGlobals
  
  rm(list=ls(name=env), pos=env)
  
}
