# install.packages('XML')
# install.packages('purrr')
pacman::p_load(dplyr,ggplot2)
path0 <- rstudioapi::getSourceEditorContext()$path %>% 
  dirname() %>% 
  sub("My.*","Shared with me/Constantin/Marc_ProScope/",.)
path <- paste0(path0,"T16L600_Renamed_detected")


flist <- list.files(path,".xml")
# for TU folder
# file.copy(paste0(path,"/",flist %>% sub(".xml","",.),".jpg"),paste0(path0,"/TU_share"))
# file.copy(paste0(path,"/",flist),paste0(path0,"/TU_share"))

# -------------------------------------------------------------------------
xmlread3 <- function(filename,thr){
  tmp<- XML::xmlToList(filename)
  df <- tmp%>% 
    .[grepl("object",names(.))] %>% 
    purrr::map_dfr(.,~{data.frame(t(unlist(.x)))}) %>% 
    mutate(across(robndbox.cx:robndbox.angle,as.numeric),
           pic=tmp$filename,
           width=tmp$size %>% .$width,
           height=tmp$size %>% .$height,
           display.y=as.numeric(height)-robndbox.cy) %>% 
    add_row_class(.,thr)
}

add_row_class <- function(df,thr){
  # chat GPT
  dist_matrix <- dist(df$robndbox.cy)
  # Apply hierarchical clustering to the distance matrix
  hc <- hclust(dist_matrix)
  # Cut the dendrogram at a suitable height to obtain clusters
  # The 'h' parameter determines the height at which to cut the dendrogram
  df %>% 
    mutate(rowclass=cutree(hc, h = thr) %>% factor())
  
}

plot_fun <- function(df){
  df %>% 
    ggplot(aes(stomata.cx,display.y,color=stomata_row))+
    geom_point(aes(shape=stomata_type),size=3)+
    theme_bw()+ggtitle(df[["pic_name"]][1])+
    scale_x_continuous("x",limits = c(0,df[["pic_width"]][1]))+
    scale_y_continuous("y",limits = c(0,df[["pic_height"]][1]))+
    ggforce::geom_ellipse(data=df %>% filter(!grepl("incomplete",stomata_type)),
                          mapping=aes(x0 = stomata.cx, y0 = display.y,
                                      a = stomata.w/2, b = stomata.h/2, 
                                      angle = stomata.angle %>% map_dbl(.,~{
                                        rad2trans(.x)})),
                          show.legend =F)+
    coord_fixed()
}
rad2trans <- function(rad){
  if(rad>(pi/2)){
    rad <- pi-rad
  }
  return(rad)
}
# -------------------------------------------------------------------------
# res <- purrr::imap_dfr(flist,~{
#   xmlread3(paste0(path,"/",.x),80) %>% 
#     mutate(i=.y)
# })
# 
# format_res <- res %>%
#   select(-c(type,pose:difficult,i)) %>% 
#   rename(pic_width=width,pic_height=height,
#          stomata_type=name,pic_name=pic,
#          stomata_row=rowclass) %>% 
#   relocate(stomata_row)
# names(format_res) <-
#   names(format_res) %>% 
#     gsub("robndbox","stomata",.) 
# 
# write.csv(format_res,
#           paste0(path,"/T16L600_xml_data.csv"),
#           row.names = F
# )
# 
rl <- format_res %>% 
  mutate(stomata_row=factor(stomata_row)) %>% 
  group_by(pic_name) %>% group_split()
rname <- format_res$pic_name %>% unique()
i <- grep("T16L600_W1_GC1_R2_P100_g103_2",rname)
plot_fun( rl[[i]]) %>% print()

format_res %>%
  group_by(stomata_type) %>%
  summarise(count=n()) %>%
  mutate(percentage=count*100/nrow(format_res),
         total=nrow(format_res))
# next step-------------------------------------------------------------------------
library(dplyr)
library(broom)
library(tidyr)
library(purrr)
format_res <- read.csv(paste0(path,"/T16L600_xml_data.csv"))
res<- format_res %>% 
  group_by(pic_name,stomata_row) %>% 
  group_split() %>% 
  map_dfr(.,~{
    sloptable <- .x %>% 
      reframe(mod = broom::tidy(lm(
        display.y ~ stomata.cx , data = .)))%>%
      unnest(cols = c(mod)) 
    sub_df <- .x %>% 
      select(pic_name,stomata_row) %>% 
      .[1,] 
    
    df <- cbind(sub_df[rep(nrow(sloptable)),],
                sloptable)
    return(df)
  })


dist2d <- function(slope,from,to) {
  v1 <- c(1,slope) # slope of target line 
  v2 <- a2 - b2 # slope of intersect
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
} 


dist2d <- function(a,b,c) {
  v1 <- b2 - c2 # slope of target line 
  v2 <- a2 - b2 # slope of intersect
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
} 

## two-dimensional case:
a2 <- c(0,2)
b2 <- c(2,0)
c2 <- c(1,3)
d2 <- dist2d(a2,b2,c2)
# row distance
# classify the row group 
# calculate slope and intercept for each group
# calculate distance between each line 

# average distance between row 

# consider angle data in the calculation??

# function -------------------------------------------------------------------------
# old logi
# for each row class
# sd, med and range
# 
# set sd thresh 40, check class sd>thresh or only single point.
# 
# for each candidate,
# extract rowclass and y vec
# 
# for single point, 
# calculate distance to other class, find the minimum.
# check the nearest class is not containing other class (s<100)
# then overwite the new class.
# 
# for multiple points with s>40 and s<100,
# reclassify
classfun <-function(vec){
  # first rough classification
  hist.res <- hist(vec,plot=F)
  # cut(vec,c(0,hist.res$mids[which(hist.res$counts>0)],5000)) %>% as.numeric()
  cut(vec,length(which(hist.res$counts>0))) %>% as.numeric()
} 
# rotation should not be based on the labeled data, but the slope
# rad2deg <- function(rad){
#   deg <- rad * 180 / pi
#   if(deg>90){
#     deg <- deg-180
#   }
#   return(deg)
# }

angle_check<- function(df){
  if(any(df$robndbox.angle>0)){
    rad <- df$robndbox.angle
    rad <- rad   %>% .[.>0]%>% median()
    newxy<- df%>% 
      select(robndbox.cx,robndbox.cy) %>%
      xyrotate(.,rad2trans(rad))
    df$robndbox.cx <- newxy$robndbox.cx
    df$robndbox.cy <- newxy$robndbox.cy
  }
  return(df)
}

xyrotate<- function(xy,angle){
  # make sure xyz is a dataframe with column order x,y,z
  # turn based on the z axis
  # https://www.mathworks.com/help/phased/ref/rotz.html?searchHighlight=rotz&s_tid=srchtitle_rotz_1
  rotz <- matrix(c(cos(angle),-sin(angle),
                   sin(angle),cos(angle)),
                 byrow = T,
                 nrow=2)
  
  new.df <- rotz%*%t(xy)%>%t()%>%data.frame()
  names(new.df) <- c("robndbox.cx","robndbox.cy")
  return(new.df)
}
row_check <- function(df){
  # double check class result from hist
  sumdf <- df %>% group_by(rowclass) %>% 
    summarise(s=sd(robndbox.cy),
              m=median(robndbox.cy),
              r=diff(range(robndbox.cy))) 
  sd.thresh <- 40
  # extreme standard deviation 
  ckdf <-sumdf%>% filter(s>sd.thresh|is.na(s))# sd =na when only one value in the class
  if(nrow(ckdf)>0){
    for (i in 1:nrow(ckdf)){
      # find class with large sd
      ori.class <- ckdf[i,]$"rowclass"
      # find y with large sd
      ori.y <- df[df$rowclass==ori.class,]$"robndbox.cy"
      
      if (is.na(ckdf[i,]$s)){ 
        # when only one value per class
        out.y <- ori.y
        # calculate distance to other classes
        distdf <- sumdf %>% 
          mutate(dist=abs(m-mean(out.y))) %>% 
          filter(!is.na(s)) %>% 
          # find the nearest one
          filter(dist==min(dist)) 
        # check the nearest class is within the range of threshold
        if(distdf$s<100){
          new.class <- distdf%>% .$rowclass
          df <- df %>%
            mutate(rowclass=case_when(robndbox.cy%in%out.y~new.class,
                                      T~rowclass) )
        } else{
          warning(paste("canot reclass based on sd.thresh",sd.thresh))
        }
        
      }else if( ckdf[i,]$"s"<100){
        # in case of just need to reclassify into two classes
        # give a new class that will not overlap with other situation
        cnumber <- diff(range(ori.y))%/%min(df$robndbox.h)+1
        if(cnumber>1){
          re.class <- cut(ori.y,cnumber) %>% as.numeric()+30+i*3
          # look uptable to merge
          ltable <- data.frame(robndbox.cy=ori.y,newclass=re.class)
          # replace old class with new class
          df <- df %>%
            left_join(.,ltable,"robndbox.cy") %>% 
            mutate(rowclass=case_when(!is.na(newclass)~newclass,
                                      T~rowclass) ) %>% 
            select(-newclass)
        }
        
      } else{
        warning(paste("check data",df$pic[1]))
      }
    }
    
  }
  # find classes that are too close
  # merge them into one
  sumdf2 <- df %>% group_by(rowclass) %>% 
    summarise(s=sd(robndbox.cy),
              m2=median(robndbox.cy),
              m=median(robndbox.cy)) %>%
    arrange(m) 
  ind <- which(diff(sumdf2$m)<50)
  for ( j in ind){
    class.vec <- sumdf2[c(j,j+1),]$rowclass
    df2 <- df %>%
      mutate(rowclass=case_when(rowclass%in%class.vec~class.vec[1],
                                T~rowclass) )
    if(sd(df2[df2$rowclass==class.vec[1]]$robndbox.cy)<min(df$robndbox.h)*1.5){
      df <- df2
    }
  }
  return(df)
}
xmlread<- function(filename){
  # read xml file and turn to dataframe
  tmp <- XML::xmlToList(filename) 
  res <- tmp%>% 
    .[grepl("object",names(.))] %>% 
    purrr::map_dfr(.,~{data.frame(t(unlist(.x)))}) %>% 
    mutate(across(robndbox.cx:robndbox.angle,as.numeric),
           pic=tmp$filename,
           width=tmp$size %>% .$width,
           height=tmp$size %>% .$height %>% as.numeric())%>%
    angle_check(.)
  clas <- classfun(res$robndbox.cy) 
  # rowclass check function 
  res <- res %>% 
    mutate(rowclass=clas) %>% 
    row_check(.) %>%
    # row_check(.) %>%
    group_by(rowclass) %>%
    mutate(rowclass=mean(robndbox.cy)) %>% 
    ungroup() %>% 
    mutate( rowclass=rowclass%>% as.factor()%>% 
              as.numeric() %>% as.character(),
            display.y=height-robndbox.cy) 
}
xmlread2<- function(filename){
  # read xml file and turn to dataframe
  tmp <- XML::xmlToList(filename) 
  res <- tmp%>% 
    .[grepl("object",names(.))] %>% 
    purrr::map_dfr(.,~{data.frame(t(unlist(.x)))}) %>% 
    mutate(across(robndbox.cx:robndbox.angle,as.numeric),
           pic=tmp$filename,
           width=tmp$size %>% .$width,
           height=tmp$size %>% .$height) 
  # %>% angle_check(.)
  clas <- classfun(res$robndbox.cy) 
  # rowclass check function 
  res <- res %>% 
    mutate(rowclass=clas) %>% 
    row_check(.) %>%
    group_by(rowclass) %>%
    mutate(rowclass=mean(robndbox.cy)) %>% 
    ungroup() %>% 
    mutate( rowclass=rowclass%>% as.factor()%>%
              as.numeric() %>% as.character(),
            display.y=as.numeric(height)-robndbox.cy) 
}



# -------------------------------------------------------------------------
# filename <- paste0('data/detected pictures/',flist[14])
for (i in c(1:29)){#22,21,20,18,15,9,7,3
  print(i)
  f <- xmlread2(paste0(path,"/",flist[i]))
  
  p <- f%>% 
    ggplot(aes(robndbox.cx,display.y,color=rowclass))+
    geom_point()+
    theme_bw()+ggtitle(paste("a",i," ",flist[i]))+
    scale_x_continuous(c(0,f[["width"]][1]))+
    scale_y_continuous(c(0,f[["height"]][1]))
  print(p)
  
}
# 29 26 23 22 21 20 18 15 9 7 3
# 12
# res %>%   ggplot(aes(robndbox.cx,display.y,color=factor(rowclass)))+geom_point()+
#   theme_bw()
xy <- data.frame(robndbox.cx=c(500,600),
                 display.y=c(1880,253))
xy %>% ggplot(aes(robndbox.cx,display.y))+geom_point()+
  scale_x_continuous(500,1)
a %>% select(robndbox.cx,display.y) %>%
  xyrotate(.,max(a$robndbox.angle)) %>% 
  ggplot(aes(robndbox.cx,display.y))+geom_point()
