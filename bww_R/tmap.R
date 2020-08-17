tmap1907 <- read_delim(file = "tmap1907.csv", col_names = T, delim = ",")
tmap1908 <- read_delim(file = "tmap1908.csv", col_names = T, delim = ",")

tmap_a <- rbind(tmap1907, tmap1908)

################################################################################
Goesan_tmap <- tmap_a %>% 
  filter(add0 == "괴산군", ser3 == "폭포/계곡") %>% 
  group_by(ser0) %>% 
  summarise(n = n(), mean_rank = mean(rank))
View(Goesan_tmap)
################################################################################
Yeongwol_tmap <- tmap_a %>% 
  filter(add0 == "영월군", ser3 == "폭포/계곡") %>% 
  group_by(ser0) %>% 
  summarise(n = n(), mean_rank = mean(rank))
View(Yeongwol_tmap)
################################################################################
Yangu_tmap <- tmap_a %>% 
  filter(add0 == "양구군", ser3 == "폭포/계곡") %>% 
  group_by(ser0) %>% 
  summarise(n = n(), mean_rank = mean(rank))
View(Yangu_tmap)
################################################################################
Inje_tmap <- tmap_a %>% 
  filter(add0 == "인제군", ser3 == "폭포/계곡") %>% 
  group_by(ser0) %>% 
  summarise(n = n(), mean_rank = mean(rank))
View(Inje_tmap)
################################################################################
