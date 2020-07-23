#
# code:  Compile all initiative reporting data after the initiative reporting deadline
#
# author: Kelly Claborn, clabornkelly@gmail.com
# created: July 2020
# modified: 
# 
# 


# !!! NOTE: need to adjust filepaths if doing the compiling of files somewhere other than the remote server!


# ---- define initiative key list ----

init.key.list <- c("i0009", "i0026", "i0027", "i0029", "i0030",
                   "i0031", "i0032", "i0033", "i0034", "i0035",
                   "i0036", "i0037", "i0038", "i0039", "i0040",
                   "i0041", "i0042", "i0043", "i0044", "i0045", 
                   "i0046")

master.init.dim <- import("~/ShinyApps/Initiative_reporting/responses/FY21_initiative_dim.csv", integer64 = "numeric") %>%
  group_by(initiative) %>%
  summarise(initiativekey = unique(initiativekey))


# ---- use loop to source in four main files from each initiative file ----

for(i in init.key.list) {
  filename <- list(paste("initiative.dim.", i, sep = ""),
                   paste("init.indicator.dim.", i, sep = ""),
                   paste("init.indicator.fact.", i, sep = ""),
                   paste("milestones.", i, sep = ""))
  
  assign(filename[[1]], 
         import(paste("~/ShinyApps/Initiative_reporting/responses", i, "FY21_initiative_dim.csv", sep = "/"), integer64 = "numeric") %>%
           filter(initiativekey==i) %>% filter(timestamp==max(timestamp))) # filtered by most recent submission per initiative team
  
  assign(filename[[2]],
         import(paste("~/ShinyApps/Initiative_reporting/responses", i, "FY21_init_indicator_dim.csv", sep = "/"), integer64 = "numeric") %>% 
           left_join(master.init.dim[,c("initiative","initiativekey")], by = "initiative") %>%
           filter(initiativekey==i) %>% filter(timestamp==max(timestamp))) # filtered by most recent submission per initiative team
  
  assign(filename[[3]],
         import(paste("~/ShinyApps/Initiative_reporting/responses", i, "FY21_init_indicator_fact.csv", sep = "/"), integer64 = "numeric") %>% 
           left_join(master.init.dim[,c("initiative","initiativekey")], by = "initiative") %>%
           filter(initiativekey==i) %>% filter(timestamp==max(timestamp))) # filtered by most recent submission per initiative team
  
  assign(filename[[4]],
         import(paste("~/ShinyApps/Initiative_reporting/responses", i, "FY21_milestones.csv", sep = "/"), integer64 = "numeric") %>% 
           left_join(master.init.dim[,c("initiative","initiativekey")], by = "initiative") %>%
           filter(initiativekey==i) %>% filter(timestamp==max(timestamp))) # filtered by most recent submission per initiative team
  
}


# ---- define vectors with names of all data frames ----

filelist.initiative.dim <- grep("initiative.dim.", ls(), value = TRUE)
filelist.init.indicator.dim <- grep("init.indicator.dim.", ls(), value = TRUE)
filelist.init.indicator.fact <- grep("init.indicator.fact.", ls(), value = TRUE)
filelist.milestones <- grep("milestones.", ls(), value = TRUE)


# ---- bind all initiative data together into four main files ----

initiative.dim <- do.call(rbind, mget(filelist.initiative.dim))
init.indicator.dim <- do.call(rbind, mget(filelist.init.indicator.dim))
init.indicator.fact <- do.call(rbind, mget(filelist.init.indicator.fact))
milestones <- do.call(rbind, mget(filelist.milestones))


# ---- output to responses folder ----

export(initiative.dim, '~/ShinyApps/Initiative_reporting/responses/FY21_initaitive_dim_compiled.csv')
export(initiative.dim, '~/ShinyApps/Initiative_reporting/responses/FY21_initaitive_dim_compiled.csv')
export(initiative.dim, '~/ShinyApps/Initiative_reporting/responses/FY21_initaitive_dim_compiled.csv')
export(initiative.dim, '~/ShinyApps/Initiative_reporting/responses/FY21_initaitive_dim_compiled.csv')





rm(list=filelist.initiative.dim)
rm(list=filelist.init.indicator.dim)
rm(list=filelist.init.indicator.fact)
rm(list=filelist.milestones)
rm(initiative.dim,init.indicator.dim,init.indicator.fact,milestones)
