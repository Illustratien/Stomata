rm(list = ls())
pacman::p_load(toolPhD,dplyr,ggplot2,purrr,gridExtra,foreach)

# read files --------------------------------------------------------------
ntu_file <- list.files("result/Ntu")
sourcetype <- ntu_file %>% strsplit("_") %>% 
  map_depth(.,1,~{.x[2]}) %>% unlist() %>% gsub(".xlsx","",.)

ntu_dlist <- map(ntu_file,~{
  xlsx::read.xlsx(file.path("result/Ntu/",.x),sheetIndex = 1) %>% 
    #remove first unused column (row id)
    .[,-1] %>% rename(pic_name="File.Name") %>% 
    # recover the unit of bounding box length to pixel 
    mutate(boundingbox_x=boundingbox_x*2592,
           boundingbox_y=boundingbox_y*1944,
    )%>% select(boundingbox_x,boundingbox_y,pic_name,class) %>%
    mutate(type="ntu") %>% 
    rename(stomata.cx=boundingbox_x,stomata.cy=boundingbox_y)
})

# experiment folder name 
folder <- ntu_dlist[[1]]$pic_name[1] %>% strsplit("_") %>% .[[1]] %>% .[1] 

ground_df <- read.csv(sprintf("result/intermediate/%s/%s_xml_data.csv",folder,folder)) %>% 
  dplyr::select(stomata.cx,stomata.cy,pic_name,stomata.type) %>%
  mutate(type="truth") %>% 
  rename(class=stomata.type)

# picturname that is in ground truth 
gdf_pic <- ground_df$pic_name %>% unique()

ntu_dlist <- map(ntu_dlist,~{
  # filter the data of estimation that can match the ground truth
  .x %>% filter(pic_name%in%gdf_pic)
})

# parallel processing -------------------------------------------------------------------------

n.cores <- parallel::detectCores() - 1
#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores,
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)

system.time(
  re <- purrr::map (1:2,function(k){ # for with blurry and without blurry
    # subset data and merge
    ntu_df <- ntu_dlist[[k]]
    pic_tar<- ntu_df$pic_name %>% unique()
    # split for each picture 
    mdf<- bind_rows(ground_df ,ntu_df) %>%
      # add display column 
      mutate(display.y=1944-stomata.cy) %>% 
      group_by(pic_name) %>%
      mutate(id=1:n()) %>% 
      group_split()
    
    res <- foreach(
      i  = 1:length(mdf),
      .packages = c("dplyr","purrr","ggplot2","tidyr","gridExtra","toolPhD")
    ) %dopar% {
      source("src/modules/match_pipeline_fun.R")
      plotfun(mdf[[i]])
      
    }
  })
)

# output directory ------------------------------------------------------------------
dir.create(file.path("./result/check_ground_truth"), showWarnings = FALSE)
tarfoldr <- file.path("./result/check_ground_truth",folder)
dir.create(tarfoldr, showWarnings = FALSE)

# dataframe export---------------------------------------------------------------

dff<- map_depth(re,2,~{.x[[1]]}) %>% 
  map(.,~{Reduce("rbind",.x)}) %>% 
  imap(.,~{.x %>% mutate(source=sourcetype[.y])}) %>% 
  Reduce("rbind",.)
data.table::fwrite(dff,paste0(tarfoldr,"/stomata_check.csv"),row.names = F)

# plot --------------------------------------------------------------------
plot_res<- map_depth(re,2,~{.x[[2]]})
names(plot_res) <- sourcetype

for(k in 1:2){
  pdf(paste0(tarfoldr,"/",folder,"_",sourcetype[k],"_check.pdf"),
      width=20,
      onefile = T)
  
  plot_res[[k]] %>% purrr::walk(.,~{.x %>% print()})
  
  
  dev.off()
  
}

doParallel::stopImplicitCluster()
