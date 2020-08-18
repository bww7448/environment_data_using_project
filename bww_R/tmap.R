tmap1907 <- read_delim(file = "tmap1907.csv", col_names = T, delim = ",")
tmap1908 <- read_delim(file = "tmap1908.csv", col_names = T, delim = ",")

tmap_a <- rbind(tmap1907, tmap1908)
View(tmap_a)
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

View( tmap_a %>% 
  filter(add0 == "수원시 장안구", ser3 == "한식") %>% 
  group_by(ser0) %>% 
  summarise(n = n(), mean_rank = mean(rank)))

View(Yeongwol_tmap)
################################################################################
Yangu_tmap <- tmap_a %>% 
  filter(add0 == "양구군", ser3 == "폭포/계곡") %>% 
  group_by(ser0) %>% 
  summarise(n = n(), mean_rank = mean(rank))
View(Yangu_tmap)
################################################################################
Gapyeong_tmap <- tmap_a %>% 
  filter(add0 == "가평군", ser3 == "폭포/계곡") %>% 
  group_by(ser0) %>% 
  summarise(n = n(), mean_rank = mean(rank))
View(Gapyeong_tmap)
################################################################################
Paju_tmap <- tmap_a %>% 
  filter(add0 == "파주시", ser3 == "폭포/계곡") %>% 
  group_by(ser0) %>% 
  summarise(n = n(), mean_rank = mean(rank))
View(Paju_tmap)
################################################################################
Ulju_tmap <- tmap_a %>% 
  filter(add0 == "울주군", ser3 == "폭포/계곡") %>% 
  group_by(ser0) %>% 
  summarise(n = n(), mean_rank = mean(rank))
View(Ulju_tmap)
################################################################################
Sancheong_tmap <- tmap_a %>% 
  filter(add0 == "산청군", ser3 == "폭포/계곡") %>% 
  group_by(ser0) %>% 
  summarise(n = n(), mean_rank = mean(rank))
View(Sancheong_tmap)
################################################################################
Bonghwa_tmap <- tmap_a %>% 
  filter(add0 == "봉화군", ser3 == "폭포/계곡") %>% 
  group_by(ser0) %>% 
  summarise(n = n(), mean_rank = mean(rank))
View(Bonghwa_tmap)
################################################################################
Inje_tmap <- tmap_a %>% 
  filter(add0 == "인제군", ser3 == "폭포/계곡") %>% 
  group_by(ser0) %>% 
  summarise(n = n(), mean_rank = mean(rank))
View(Inje_tmap)
################################################################################