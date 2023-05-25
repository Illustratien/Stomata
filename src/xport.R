pacman::p_load(dplyr,ggplot2,broom,purrr,tidyr,foreach)

# for TU folder
# path0 <- rstudioapi::getSourceEditorContext()$path %>% 
#   dirname() %>% 
#   sub("My.*","Shared with me/Constantin/Marc_ProScope/",.)
# path <- paste0(path0,"T16L600_Renamed_detected")
# file.copy(paste0(path,"/",flist %>% sub(".xml","",.),".jpg"),paste0(path0,"/TU_share"))
# file.copy(paste0(path,"/",flist),paste0(path0,"/TU_share"))
# -------------------------------------------------------------------------
source("src/processing_fun.R")
# parallel setting
n.cores <- parallel::detectCores() - 1
#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
# raw data preparation-------------------------------------------------------------------------
flist <- list.files("data/detected pictures/",".xml")
doParallel::registerDoParallel(cl = my.cluster)

system.time(
  format_res <- foreach(
    y  = 1:length(flist),
    x=flist,
    .packages = c('XML','purrr','dplyr')
  ) %dopar% {
    xmlread(paste0("data/detected pictures/",x),80) %>%
      mutate(i=y)
  } %>%map_dfr(.,~{.x}) %>%
    dplyr::select(-c(type,pose:difficult,i)) %>%
    dplyr::rename(pic_width=width,pic_height=height,
                  stomata.type=name,pic_name=pic,
                  stomata.row=rowclass) %>%
    dplyr::relocate(stomata.row) %>%
    group_by(pic_name,stomata.row) %>%
    mutate(stomata.row.count=n())
)
#
doParallel::stopImplicitCluster()

names(format_res) <-
  names(format_res) %>%
  gsub("robndbox","stomata",.)

write.csv(format_res,
          "result/T16L600_xml_data.csv",
          row.names = F
)

# easy summary
# format_res %>%
#   group_by(stomata_type) %>%
#   summarise(count=n()) %>%
#   mutate(percentage=count*100/nrow(format_res),
#          total=nrow(format_res))

# Slope -------------------------------------------------------------------------

format_res <- read.csv("result/T16L600_xml_data.csv")%>% 
  mutate(stomata.row=factor(stomata.row))
res_ls<- format_res %>% 
  dplyr::filter(stomata.row.count>1) %>% 
  droplevels() %>% 
  group_by(pic_name,stomata.row) %>% 
  group_split() 

doParallel::registerDoParallel(cl = my.cluster)

system.time(
  slope_df <- foreach(
    i  = 1:length(res_ls),
    df = res_ls,
    .packages = c('dplyr','tidyr')
  ) %dopar% {
    sloptable <- df %>% 
      summarise(mod = broom::tidy(lm(
        display.y ~ stomata.cx , data = .)))%>%
      unnest(cols = c(mod)) 
    sub_df <- df %>% 
      select(pic_name,stomata.row,stomata.row.count) %>% 
      .[1,] 
    
    df <- cbind(sub_df[rep(1,nrow(sloptable)),],
                sloptable)
    return(df)
  } %>% 
    map_dfr(.,~{.x})%>% 
    dplyr::select(pic_name:estimate) %>% 
    mutate(term=case_when(term=='(Intercept)'~'intercept',
                          T~'slope')) %>% 
    tidyr::pivot_wider(names_from = 'term',values_from = 'estimate')
)

doParallel::stopImplicitCluster()
write.csv(slope_df,"result/T16L600_slope.csv",row.names = F)
