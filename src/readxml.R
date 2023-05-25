pacman::p_load(dplyr,ggplot2,broom,purrr,tidyr,foreach)
# -------------------------------------------------------------------------
source("src/processing_fun.R")
# parallel setting
n.cores <- parallel::detectCores() - 1
#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
# read -------------------------------------------------------------------------
format_res <- read.csv("result/T16L600_xml_data.csv")%>% 
  mutate(stomata.row=factor(stomata.row))
slope_df <-  read.csv("result/T16L600_slope.csv")
# easy summary of raw data-------------------------------------------------------------------------
pic_examp <- 
  # "T16L600_W1_GC1_R2_P100_g103_2"
  "T16L600_W1_GC1_R2_P115_g48_3"
# "T16L600_W3_GC1_R2_P106_g60_6"
ck_pic(pic_examp,format_res)
# area cover 1*complete, .5*incomplete
# total number 1*complete, .5*incomplete
# sub_df <- format_res %>%  dplyr::filter(pic_name==pic_examp) 
# simple_stat(sub_df)

df_ls <- format_res %>% group_by(pic_name) %>% group_split()
doParallel::registerDoParallel(cl = my.cluster)

system.time(
  stat_df <- foreach(
    sub_df = df_ls,
    .packages = c('dplyr','purrr')
  ) %dopar% {
    source("src/processing_fun.R")
    sub_df %>% simple_stat()
  } %>% 
    map_dfr(.,~{.x})
)

doParallel::stopImplicitCluster()

write.csv(stat_df,
          "result/stomata_stat.csv",row.names = F)
# slope check -------------------------------------------------------------------------
# find pic with big slope
# slope_df %>% 
#   group_by(pic_name) %>% 
#   summarise(m=mean(slope)) %>% 
#   filter(m>.01)

# find the row overview
# subslope <- slope_df %>% 
#   select(pic_name,intercept,stomata.row)%>%
#   tidyr::pivot_wider(names_from = "stomata.row",
#                      values_from = "intercept",
#                      names_prefix = "I") 
# row distance
# subslope <- slope_df %>% dplyr::filter(pic_name==pic_examp)
# subslope %>% row_dist()

slope_ls <- slope_df %>% group_by(pic_name) %>% group_split()

doParallel::registerDoParallel(cl = my.cluster)

system.time(
  rowdist_df <- foreach(
    subslope = slope_ls,
    .packages = c('dplyr','purrr')
  ) %dopar% {
    subslope %>% row_dist_fun()
  } %>% 
    map_dfr(.,~{.x})
)

doParallel::stopImplicitCluster()


rowdist_disperse <- rowdist_df %>% 
  group_by(pic_name) %>% 
  Disperse_1D(.,"row_dist") %>% 
  left_join(rowdist_df,.)

write.csv(rowdist_disperse,
          "result/rowdist_dispersion.csv",row.names = F)
