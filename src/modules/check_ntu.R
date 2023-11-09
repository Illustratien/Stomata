rm(list = ls())
pacman::p_load(toolPhD,dplyr,ggplot2,purrr,gridExtra,foreach)

# read files --------------------------------------------------------------
ntu_file <- list.files("result/Ntu")
sourcetype <- ntu_file %>% strsplit("_") %>% 
  map_depth(.,1,~{.x[2]}) %>% unlist() %>% gsub(".csv","",.)

ntu_dlist <- map(ntu_file,~{
  data.table::fread(file.path("result/Ntu/",.x)) %>% 
   rename(pic_name="File Name") %>% 
    # recover the unit of bounding box length to pixel 
    mutate(
      across(ends_with(c("x", "width")),~.x*2592),
      across(ends_with(c("y", "height")),~.x*1944)
      # boundingbox_x=boundingbox_x*2592,
      #      boundingbox_y=boundingbox_y*1944,
    )%>% 
    mutate(type="ntu") %>% 
    rename(stomata.cx=boundingbox_x,stomata.cy=boundingbox_y)
})

# experiment folder name 
folder <- ntu_dlist[[1]]$pic_name[1] %>% strsplit("_") %>% .[[1]] %>% .[1] 
message(paste0("\n",folder))

ground_df <- read.csv(sprintf("result/intermediate/%s/%s_xml_data.csv",folder,folder)) %>% 
  mutate(type="truth") %>% 
  rename(class=stomata.type)

# picturname that is in ground truth 
gdf_pic <- ground_df$pic_name %>% unique()
ntu_pic <- ntu_dlist[[1]]$pic_name %>% unique()
if(length(ntu_pic)>length(gdf_pic)){
  ntu_dlist <- map(ntu_dlist,~{
    # filter the data of estimation that can match the ground truth
    .x %>% filter(pic_name%in%gdf_pic)
  })
}else if(length(ntu_pic)<length(gdf_pic)){
  ground_df <- ground_df %>% filter(pic_name%in%ntu_pic)
}

pb = txtProgressBar(min = 0, max = 4,
                    style = 3,    # Progress bar style (also available style = 1 and style = 2)
                    width = 30,initial = 0)
# parallel processing -------------------------------------------------------------------------
message("\nstart matching ground truth and detection:")
setTxtProgressBar(pb,1)
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
    ntu_df <- ntu_dlist[[k]] %>% 
      dplyr::select(stomata.cx,stomata.cy,pic_name,class,confidence,type)
    pic_tar<- ntu_df$pic_name %>% unique()
    # split for each picture 
    mdf<- bind_rows(ground_df %>% 
                      dplyr::select(stomata.cx,stomata.cy,pic_name,class,type) ,
                    ntu_df) %>%
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
doParallel::stopImplicitCluster()

# output directory ------------------------------------------------------------------
dir.create(file.path("./result/check_ground_truth"), showWarnings = FALSE)
tarfoldr <- file.path("./result/check_ground_truth",folder)
dir.create(tarfoldr, showWarnings = FALSE)
# dataframe export---------------------------------------------------------------

dff<- map_depth(re,2,~{.x[[1]]}) %>% 
  map(.,~{Reduce("rbind",.x)}) %>% 
  imap(.,~{.x %>% mutate(source=sourcetype[.y])}) %>% 
  Reduce("rbind",.)
data.table::fwrite(dff,paste0(tarfoldr,"/",folder,"_check.csv"),row.names = F)
message("\nremoved replicates:")
setTxtProgressBar(pb,2)
# -------------------------------------------------------------------------
gmeg <- ground_df %>% 
  select(-c(stomata.row,stomata.per.row,pic_width,pic_height,
                                display.y,type)) %>% 
  rename(truth_class=class)
names(gmeg)<- gsub("(stomata\\.|boundingbox_)","truth.",names(gmeg))

dff<- map_depth(re,2,~{.x[[3]]}) %>% 
  map(.,~{Reduce("rbind",.x)}) %>% 
  imap(.,~{.x %>%
      mutate(source=sourcetype[.y]) %>% 
      left_join(.,ntu_dlist[[.y]],
                by=c("stomata.cx", "stomata.cy", "pic_name",
                     "class","confidence"))
      }) %>% 
  Reduce("rbind",.) %>% 
  dplyr::select(-type)%>% 
  rename(detect_class=class)
names(dff)<- gsub("(stomata\\.|boundingbox_)","detect_",names(dff))
data.table::fwrite(dff%>% left_join(.,gmeg,c("pic_name", "truth.cx","truth.cy")),
                   paste0(tarfoldr,"/",folder,"_detect.csv"),row.names = F)

# plot --------------------------------------------------------------------
message("\nexport pdf:")
setTxtProgressBar(pb,3)
plot_res<- map_depth(re,2,~{.x[[2]]})
names(plot_res) <- sourcetype

for(k in 1:2){
  pdf(paste0(tarfoldr,"/",folder,"_",sourcetype[k],"_check.pdf"),
      width=20,
      onefile = T)
  
  plot_res[[k]] %>% purrr::walk(.,~{.x %>% print()})
  
  
  dev.off()
  
}


message("\ndone!")
setTxtProgressBar(pb,4)