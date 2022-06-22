cat(paste((Sys.time())))

for (i in 1:100000) {
  i*sqrt(i)
}

cat(paste(Sys.time()))
for (i in 1:4) {
  
  jobRunScript(
    BLL,
    name = paste0("test_",i),
    importEnv = T,
    exportEnv = "R_GlobalEnv"
  )  
}