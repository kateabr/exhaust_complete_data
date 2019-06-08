library(tidyverse)
library(ggplot2)
library(ggmosaic)

lbls <- c("смысловой", "в составе композита")

# - - - - YAMU - - - -

yam.cnt.ns <- read.csv("csv/yam_ns_cnt.csv", encoding = 'UTF-8')
yam.cnt.vs <- read.csv("csv/yam_vs_cnt.csv", encoding = 'UTF-8')

cairo_pdf('pdf/yam_v.pdf', width = 6.5, height = 3.5, family = "FreeSans")
yam.cnt.vs %>%
  select(-X,-subj) %>%
  group_by(translation) %>%
  summarise(cnt = sum(n)) %>%
  filter(cnt > 1) -> yam.cnt.vs
yam.cnt.vs %>%
  ggplot(., aes(x=translation, y=cnt, fill=translation)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom") +
  geom_label(label = yam.cnt.vs$cnt, show.legend = FALSE)
dev.off() 

cairo_pdf('pdf/yam_n.pdf', width = 6.5, height = 3.5, family = "FreeSans")
yam.cnt.ns %>%
  select(-X,-subj) %>%
  group_by(translation) %>%
  summarise(cnt = sum(n)) %>%
  filter(cnt > 2) -> yam.cnt.ns
yam.cnt.ns %>%
  ggplot(., aes(x=translation, y=cnt, fill=translation)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom") +
  geom_label(label = yam.cnt.ns$cnt, show.legend = FALSE)
dev.off() 

cairo_pdf('pdf/yam_tot.pdf', width = 2.6, height = 3.5, family = "FreeSans")
yam.cnt.vs %>%
  summarise(v_tot = sum(cnt)) %>%
  mutate(n_tot = sum(yam.cnt.ns$cnt)) %>%
  gather(value = "n", key = "type") %>%
  ggplot(., aes(x="", y=n, fill=type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.title=element_blank(), legend.title = element_blank(), legend.direction = "vertical", legend.position="bottom") +
  scale_fill_discrete(labels = lbls) +
  geom_text(aes(y = n/3 + c(0, cumsum(n)[-length(n)]), 
                label = n), size = 4)
dev.off() 


# - - - - NAKUNARU_DIE - - - -

nakunaru_die.all.ns <- read.csv("csv/nakunaru_die_ns.csv", encoding = "UTF-8")

cairo_pdf('pdf/nakunaru_die_ns.pdf', width = 6.5, height = 3.5, family = "FreeSans")
nakunaru_die.all.ns %>%
  ggplot(., aes(x=translation, y=n, fill=translation)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom") +
  facet_wrap(~human, labeller = labeller(human = as_labeller(c(no = "не человек", yes = "человек"))))
dev.off()


# - - - - TSUKIRU - - - -

tukiru.vs.cnt <- read.csv("csv/tukiru_cnt_vs.csv", encoding = "UTF-8")
tukiru.ns.cnt <- read.csv("csv/tukiru_cnt_ns.csv", encoding = "UTF-8")

cairo_pdf('pdf/tukiru_v.pdf', width = 6.5, height = 3.5, family = "FreeSans")
tukiru.vs.cnt %>%
  select(-X,-subj) %>%
  group_by(translation) %>%
  summarise(cnt = sum(n)) -> tukiru.vs.cnt
tukiru.vs.cnt %>%
  ggplot(., aes(x=translation, y=cnt, fill=translation)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom") +
  geom_label(label = tukiru.vs.cnt$cnt, show.legend = FALSE)

dev.off() 

cairo_pdf('pdf/tukiru_n.pdf', width = 6.5, height = 3.5, family = "FreeSans")
tukiru.ns.cnt %>%
  select(-X,-subj) %>%
  group_by(translation) %>%
  summarise(cnt = sum(n)) %>%
  filter(cnt > 1) -> tukiru.ns.cnt
tukiru.ns.cnt %>%
  ggplot(., aes(x=translation, y=cnt, fill=translation)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom") +
  geom_label(label = tukiru.ns.cnt$cnt, show.legend = FALSE)
dev.off()

cairo_pdf('pdf/tukiru_tot.pdf', width = 2.6, height = 3.5, family = "FreeSans")
tukiru.vs.cnt %>%
  summarise(v_tot = sum(cnt)) %>%
  mutate(n_tot = sum(tukiru.ns.cnt$cnt)) %>%
  gather(value = "n", key = "type") %>%
  ggplot(., aes(x="", y=n, fill=type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.title=element_blank(), legend.title = element_blank(), legend.direction = "vertical", legend.position="bottom") +
  scale_fill_discrete(labels = lbls) +
  geom_text(aes(y = n/3 + c(0, cumsum(n)[-length(n)]), 
                label = n), size = 4)
dev.off()


# - - - - HATERU - - - -

hateru.vs.cnt <- read.csv("csv/hateru_cnt_vs.csv", encoding = "UTF-8")
hateru.ns.cnt <- read.csv("csv/hateru_cnt_ns.csv", encoding = "UTF-8")

cairo_pdf('pdf/hateru_v.pdf', width = 6.5, height = 3.5, family = "FreeSans")
hateru.vs.cnt %>%
  select(-X,-subj) %>%
  group_by(translation) %>%
  summarise(cnt = sum(n)) -> hateru.vs.cnt
hateru.vs.cnt %>%
  ggplot(., aes(x=translation, y=cnt, fill=translation)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom") +
  geom_label(label = hateru.vs.cnt$cnt, show.legend = FALSE)
dev.off() 

cairo_pdf('pdf/hateru_n.pdf', width = 6.5, height = 3.5, family = "FreeSans")
hateru.ns.cnt %>%
  select(-X,-subj) %>%
  group_by(translation) %>%
  summarise(cnt = sum(n)) %>%
  filter(cnt > 1) -> hateru.ns.cnt
hateru.ns.cnt %>%
  ggplot(., aes(x=translation, y=cnt, fill=translation)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom") +
  geom_label(label = hateru.ns.cnt$cnt, show.legend = FALSE)
dev.off()

cairo_pdf('pdf/hateru_tot.pdf', width = 2.6, height = 3.5, family = "FreeSans")
hateru.vs.cnt %>%
  summarise(v_tot = sum(cnt)) %>%
  mutate(n_tot = sum(hateru.ns.cnt$cnt)) %>%
  gather(value = "n", key = "type") %>%
  ggplot(., aes(x="", y=n, fill=type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.title=element_blank(), legend.title = element_blank(), legend.direction = "vertical", legend.position="bottom") +
  scale_fill_discrete(labels = lbls) +
  geom_text(aes(y = n/3 + c(0, cumsum(n)[-length(n)]), 
                label = n), size = 4)
dev.off()

# - - - - OWARU - - - -

owaru.ns.cnt <- read.csv("csv/owaru_cnt_ns.csv", encoding = "UTF-8")
owaru.vs.cnt <- read.csv("csv/owaru_cnt_vs.csv", encoding = "UTF-8")

cairo_pdf('pdf/owaru_ns.pdf', width = 6.5, height = 3.5, family = "FreeSans")
owaru.ns.cnt %>%
  select(-X,-subj) %>%
  group_by(translation) %>%
  summarise(cnt = sum(n)) %>%
  filter(cnt > 1) -> owaru.ns.cnt
owaru.ns.cnt  %>%
  ggplot(., aes(x=translation, y=cnt, fill=translation)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom") +
  geom_label(label = owaru.ns.cnt$cnt, show.legend = FALSE)
dev.off()

cairo_pdf('pdf/owaru_vs.pdf', width = 6.5, height = 3.5, family = "FreeSans")
owaru.vs.cnt %>%
  select(-X,-subj) %>%
  group_by(translation) %>%
  summarise(cnt = sum(n)) -> owaru.vs.cnt
owaru.vs.cnt  %>%
  ggplot(., aes(x=translation, y=cnt, fill=translation)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom") +
  geom_label(label = owaru.vs.cnt$cnt, show.legend = FALSE)
dev.off()

cairo_pdf('pdf/owaru_tot.pdf', width = 2.6, height = 3.5, family = "FreeSans")
owaru.vs.cnt %>%
  summarise(v_tot = sum(cnt)) %>%
  mutate(n_tot = sum(owaru.ns.cnt$cnt)) %>%
  gather(value = "n", key = "type") %>%
  ggplot(., aes(x="", y=n, fill=type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.title=element_blank(), legend.title = element_blank(), legend.direction = "vertical", legend.position="bottom") +
  scale_fill_discrete(labels = lbls) +
  geom_text(aes(y = n/3 + c(0, cumsum(n)[-length(n)]), 
                label = n), size = 4)
dev.off()

# - - - - TAERU - - - -

taeru.ns.cnt <- read.csv("csv/taeru_cnt_ns.csv", encoding = 'UTF-8')
taeru.vs.cnt <- read.csv("csv/taeru_cnt_vs.csv", encoding = 'UTF-8')

cairo_pdf('pdf/taeru_v.pdf', width = 6.5, height = 3.5, family = "FreeSans")
taeru.vs.cnt %>%
  select(-X,-subj) %>%
  group_by(translation) %>%
  summarise(cnt = sum(n)) -> taeru.vs.cnt
taeru.vs.cnt %>%
  ggplot(., aes(x=translation, y=cnt, fill=translation)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom") +
  geom_label(label = taeru.vs.cnt$cnt, show.legend = FALSE)
dev.off() 

cairo_pdf('pdf/taeru_n.pdf', width = 6.5, height = 3.5, family = "FreeSans")
taeru.ns.cnt %>%
  select(-X,-subj) %>%
  group_by(translation) %>%
  filter(n > 1) %>%
  summarise(cnt = sum(n)) -> taeru.ns.cnt
taeru.ns.cnt %>%
  ggplot(., aes(x=translation, y=cnt, fill=translation)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom") +
  geom_label(label = taeru.ns.cnt$cnt, show.legend = FALSE)
dev.off() 

cairo_pdf('pdf/taeru_tot.pdf', width = 2.6, height = 3.5, family = "FreeSans")
taeru.vs.cnt %>%
  summarise(v_tot = sum(cnt)) %>%
  mutate(n_tot = sum(taeru.ns.cnt$cnt)) %>%
  gather(value = "n", key = "type") %>%
  ggplot(., aes(x="", y=n, fill=type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.title=element_blank(), legend.title = element_blank(), legend.direction = "vertical", legend.position="bottom") +
  scale_fill_discrete(labels = lbls) +
  geom_text(aes(y = n/3 + c(0, cumsum(n)[-length(n)]), 
                label = n), size = 4)
dev.off() 

# - - - - KIRERU - - - -

kireru.ns.cnt <- read.csv("csv/kireru_cnt_ns.csv", encoding = 'UTF-8')
kireru.vs.cnt <- read.csv("csv/kireru_cnt_vs.csv", encoding = 'UTF-8')

cairo_pdf('pdf/kireru_v.pdf', width = 6.5, height = 3.5, family = "FreeSans")
kireru.vs.cnt %>%
  select(-X,-subj) %>%
  group_by(translation) %>%
  summarise(cnt = sum(n)) -> kireru.vs.cnt
kireru.vs.cnt %>%
  ggplot(., aes(x=translation, y=cnt, fill=translation)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom") +
  geom_label(label = kireru.vs.cnt$cnt, show.legend = FALSE)
dev.off() 

cairo_pdf('pdf/kireru_n.pdf', width = 6.5, height = 3.5, family = "FreeSans")
kireru.ns.cnt %>%
  select(-X,-subj) %>%
  group_by(translation) %>%
  summarise(cnt = sum(n)) -> kireru.ns.cnt
kireru.ns.cnt%>%
  ggplot(., aes(x=translation, y=cnt, fill=translation)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom") +
  geom_label(label = kireru.ns.cnt$cnt, show.legend = FALSE)
dev.off() 

cairo_pdf('pdf/kireru_tot.pdf', width = 2.6, height = 3.5, family = "FreeSans")
kireru.vs.cnt %>%
  summarise(v_tot = sum(cnt)) %>%
  mutate(n_tot = sum(kireru.ns.cnt$cnt)) %>%
  gather(value = "n", key = "type") %>%
  ggplot(., aes(x="", y=n, fill=type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.title=element_blank(), legend.title = element_blank(), legend.direction = "vertical", legend.position="bottom") +
  scale_fill_discrete(labels = lbls) +
  geom_text(aes(y = n/3 + c(0, cumsum(n)[-length(n)]), 
                label = n), size = 4)
dev.off() 


# - - - - NAKUNARU_NOT_DIE - - - -

nakunaru_not_die.all.ns <- read.csv("csv/nakunaru_not_die_ns.csv", encoding = "UTF-8")

cairo_pdf('pdf/nakunaru_not_die_ns.pdf', width = 6.5, height = 3.5, family = "FreeSans")
nakunaru_not_die.all.ns %>%
  select(-X,-subj) %>%
  group_by(translation) %>%
  filter(n > 2) %>%
  summarise(cnt = sum(n)) -> nakunaru_not_die.all.ns
nakunaru_not_die.all.ns %>%
  ggplot(., aes(x=translation, y=cnt, fill=translation)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom") +
  geom_label(label = nakunaru_not_die.all.ns$cnt, show.legend = FALSE)
dev.off()
