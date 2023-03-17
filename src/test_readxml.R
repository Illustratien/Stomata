k <- 26
filename <- paste0('data/detected pictures/',flist[k])
# read xml file and turn to dataframe
tmp <- XML::xmlToList(filename) 
res <- tmp%>% 
  .[grepl("object",names(.))] %>% 
  purrr::map_dfr(.,~{data.frame(t(unlist(.x)))}) %>% 
  mutate(across(robndbox.cx:robndbox.angle,as.numeric),
         
         pic=tmp$filename,
         width=tmp$size %>% .$width,
         height=tmp$size %>% .$height,
         display.y=as.numeric(height)-robndbox.cy)
# res <- res %>% angle_check()
a <- df$robndbox.angle %>% mean()
a
-(180-a*180/pi)

# rowclass check function 
# res <- res 
df <- res %>% angle_check() 
clas <- classfun(df$robndbox.cy) 
df <- df %>% mutate(rowclass=clas,
                    display.y=height-robndbox.cy)

# visualization-------------------------------------------------------------------------
df %>% 
  # mutate( rowclass=clas%>% as.factor()) %>%
  ggplot(aes(robndbox.cx,display.y,color=rowclass ))+geom_point()+
  theme_bw()+ggtitle(paste("a",k))

# check intermediate
res %>% 
  mutate( rowclass=clas%>% as.factor()) %>%
  ggplot(aes(robndbox.cx,display.y,color=rowclass ))+geom_point()+
  theme_bw()+ggtitle(paste("a",k))

k <- 9
f <- xmlread(paste0('data/detected pictures/',flist[k]))
f2 <- xmlread2(paste0('data/detected pictures/',flist[k]))
p <-f%>% 
  ggplot(aes(robndbox.cx,display.y,color=rowclass))+
  geom_point()+
  theme_bw()+ggtitle(paste("a",k))+
  scale_x_continuous(c(0,f[["width"]][1]))+
  scale_y_continuous(c(0,f[["height"]][1]))
print(p)
p <-f2%>% 
  ggplot(aes(robndbox.cx,display.y,color=rowclass))+geom_point()+
  theme_bw()+ggtitle(paste("a",k))+
  scale_x_continuous(c(0,f[["width"]][1]))+
  scale_y_continuous(c(0,f[["height"]][1]))
print(p)
p <-f2%>% 
  angle_check() %>% 
  ggplot(aes(robndbox.cx,display.y,color=rowclass))+geom_point()+
  theme_bw()+ggtitle(paste("a",k))+
  scale_x_continuous(c(0,f[["width"]][1]))+
  scale_y_continuous(c(0,f[["height"]][1]))
print(p)

