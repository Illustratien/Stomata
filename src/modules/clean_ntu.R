rm(list = ls())
source("src/modules/match_pipeline_fun.R")
pacman::p_load(purrr,dplyr,foreach)
ntu_file <- list.files("result/Ntu",pattern="*.csv")
# sourcetype <- ntu_file %>% strsplit("_") %>% 
#   map_depth(.,1,~{.x[2]}) %>% unlist() %>% gsub(".csv","",.)
# names(ntu_file) <- sourcetype
n.cores <- parallel::detectCores() - 1
#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores,
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)
ntu_merge <- map_dfr(ntu_file,~{
  resls <- data.table::fread(file.path("result/Ntu/",.x)) %>% 
    rename(stomata.cx=boundingbox_x,stomata.cy=boundingbox_y,
           pic_name="File Name",detect.class=class) %>% 
    mutate(pic_name=as.character(pic_name)) %>% 
    group_by(pic_name) %>% group_split() 
  
  
  res <- foreach(
    i  = resls,
    .packages = c("dplyr","purrr","tidyr")
  ) %dopar% {
    source("src/modules/match_pipeline_fun.R")
    rm_rep(i)
    
  } %>%
    Reduce("bind_rows",.) %>% 
    # recover the unit of bounding box length to pixel 
    mutate(
      # source=.y,
      across(ends_with(c("x", "width")),~.x*2592),
      across(ends_with(c("y", "height")),~.x*1944))
  
  names(res)<- gsub("(stomata\\.|boundingbox_)","detect.",names(res))
  
  res %>%
    rename(
      detect.length=detect.width,
      detect.width=detect.height) %>% 
    mutate(
      across(c(detect.width,detect.length),function(x){x*0.4}), #from pixel to microm
      detect.area=detect.width*detect.length) %>%  
    relocate(pic_name,detect.width,detect.length,detect.area)
  
})
doParallel::stopImplicitCluster()

saveRDS(ntu_merge,file ="result/Ntu/detect_merge.RDS" ,compress = T)
