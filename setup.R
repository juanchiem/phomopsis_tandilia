library(ggplot2)
my_theme <- theme_bw()+
  theme(panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=10, face = "bold"))

# update_drive()
update_drive <- function(local_folder, drive_folder){
  list.files(path = here::here(local_folder), 
             pattern = ".png",
             all.files = TRUE, full.names = TRUE)%>% 
    map(~ drive_upload(.,
                       path = as_dribble(drive_folder),
                       overwrite = TRUE)
    )
}

