setwd("D:\\Desktop\\course_work\\final_tables\\exhaust_complete_data")

library(tidyverse)
library(ggplot2)
library(ggmosaic)

# - - - - YAMU - - - -

yam.cnt.ns <- read.csv("yam_ns_cnt.csv", encoding = 'UTF-8')
yam.cnt.vs <- read.csv("yam_vs_cnt.csv", encoding = 'UTF-8')

cairo_pdf('yam_v.pdf' , family="Yu Mincho")
yam.cnt.vs %>%
  ggplot(., aes(x=subj, y=n, fill=subj)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom")
dev.off() 

cairo_pdf('yam_n.pdf' , family="Yu Mincho")
yam.cnt.ns %>%
  filter(cnt > 2) %>%
  ggplot(., aes(x=subj, y=cnt, fill=subj)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom")
dev.off() 

cairo_pdf('yam_tot.pdf' , family="Yu Mincho")
yam.cnt.vs %>%
  summarise(v_tot = sum(n)) %>%
  mutate(n_tot = sum(yam.cnt.ns$cnt)) %>%
  gather(value = "n", key = "type") %>%
  ggplot(., aes(x="", y=n, fill=type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.title=element_blank(), legend.title = element_blank(), legend.direction = "vertical", legend.position="bottom") +
  scale_fill_discrete(labels = c("смысловой", "вспомогательный")) +
  geom_text(aes(y = n/3 + c(0, cumsum(n)[-length(n)]), 
                label = n), size = 4)
dev.off() 


# - - - - NAKUNARU_DIE - - - -

nakunaru_die.all.ns <- read.csv("nakunaru_die_ns.csv", encoding = "UTF-8")

cairo_pdf('nakunaru_die_ns.pdf' , family="Yu Mincho")
nakunaru_die.all.ns %>%
  ggplot(., aes(x=subj, y=n, fill=subj)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom") +
  facet_wrap(~human, labeller = labeller(human = as_labeller(c(no = "не человек", yes = "человек"))))
dev.off()


# - - - - TUKIRU - - - -

tukiru.vs.cnt <- read.csv("tukiru_cnt_vs.csv", encoding = "UTF-8")
tukiru.ns.cnt <- read.csv("tukiru_cnt_ns.csv", encoding = "UTF-8")

cairo_pdf('tukiru_v.pdf' , family="Yu Mincho")
tukiru.vs.cnt %>%
  ggplot(., aes(x=subj, y=n, fill=subj)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom")
dev.off() 

cairo_pdf('tukiru_n.pdf' , family="Yu Mincho")
tukiru.ns.cnt %>%
  filter(cnt > 1) %>%
  ggplot(., aes(x=subj, y=cnt, fill=subj)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom")
dev.off()

cairo_pdf('tukiru_tot.pdf' , family="Yu Mincho")
tukiru.vs.cnt %>%
  summarise(v_tot = sum(n)) %>%
  mutate(n_tot = sum(tukiru.ns.cnt$cnt)) %>%
  gather(value = "n", key = "type") %>%
  ggplot(., aes(x="", y=n, fill=type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.title=element_blank(), legend.title = element_blank(), legend.direction = "vertical", legend.position="bottom") +
  scale_fill_discrete(labels = c("смысловой", "вспомогательный")) +
  geom_text(aes(y = n/3 + c(0, cumsum(n)[-length(n)]), 
                label = n), size = 4)
dev.off()


# - - - - HATERU - - - -

hateru.vs.cnt <- read.csv("hateru_cnt_vs.csv", encoding = "UTF-8")
hateru.ns.cnt <- read.csv("hateru_cnt_ns.csv", encoding = "UTF-8")

cairo_pdf('hateru_v.pdf' , family="Yu Mincho")
hateru.vs.cnt %>%
  ggplot(., aes(x=subj, y=n, fill=subj)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom")
dev.off() 

cairo_pdf('hateru_n.pdf' , family="Yu Mincho")
hateru.ns.cnt %>%
  filter(cnt > 1) %>%
  ggplot(., aes(x=subj, y=cnt, fill=subj)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom")
dev.off()

cairo_pdf('hateru_tot.pdf' , family="Yu Mincho")
hateru.vs.cnt %>%
  summarise(v_tot = sum(n)) %>%
  mutate(n_tot = sum(hateru.ns.cnt$cnt)) %>%
  gather(value = "n", key = "type") %>%
  ggplot(., aes(x="", y=n, fill=type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.title=element_blank(), legend.title = element_blank()) +
  scale_fill_discrete(labels = c("смысловой", "вспомогательный")) +
  geom_text(aes(y = n/3 + c(0, cumsum(n)[-length(n)]), 
                label = n), size = 4)
dev.off()

# - - - - OWARU - - - -

owaru.all.ns <- read.csv("owaru_ns.csv", encoding = "UTF-8")

cairo_pdf('owaru_ns.pdf' , family="Yu Mincho")
owaru.all.ns %>%
  filter(n > 1) %>%
  ggplot(., aes(x=subj, y=n, fill=subj)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom")
dev.off()


# - - - - TAERU - - - -

taeru.ns.cnt <- read.csv("taeru_cnt_ns.csv", encoding = 'UTF-8')
taeru.vs.cnt <- read.csv("taeru_cnt_vs.csv", encoding = 'UTF-8')

cairo_pdf('taeru_v.pdf' , family="Yu Mincho")
taeru.vs.cnt %>%
  ggplot(., aes(x=subj, y=n, fill=subj)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom")
dev.off() 

cairo_pdf('taeru_n.pdf' , family="Yu Mincho")
taeru.ns.cnt %>%
  filter(cnt > 1) %>%
  ggplot(., aes(x=subj, y=cnt, fill=subj)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom")
dev.off() 

cairo_pdf('taeru_tot.pdf' , family="Yu Mincho")
taeru.vs.cnt %>%
  summarise(v_tot = sum(n)) %>%
  mutate(n_tot = sum(taeru.ns.cnt$cnt)) %>%
  gather(value = "n", key = "type") %>%
  ggplot(., aes(x="", y=n, fill=type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.title=element_blank(), legend.title = element_blank(), legend.direction = "vertical", legend.position="bottom") +
  scale_fill_discrete(labels = c("смысловой", "вспомогательный")) +
  geom_text(aes(y = n/3 + c(0, cumsum(n)[-length(n)]), 
                label = n), size = 4)
dev.off() 


# - - - - SHIMAU - - - -

shimau.ns.cnt <- read.csv("shimau_cnt_ns.csv", encoding = 'UTF-8')
shimau.vs.cnt <- read.csv("shimau_cnt_vs.csv", encoding = 'UTF-8')

cairo_pdf('shimau_v.pdf' , family="Yu Mincho")
shimau.vs.cnt %>%
  ggplot(., aes(x=subj, y=n, fill=subj)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom")
dev.off() 

cairo_pdf('shimau_n.pdf' , family="Yu Mincho")
shimau.ns.cnt %>%
  ggplot(., aes(x=subj, y=n, fill=subj)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom")
dev.off() 

cairo_pdf('shimau_tot.pdf' , family="Yu Mincho")
shimau.vs.cnt %>%
  summarise(v_tot = sum(n)) %>%
  mutate(n_tot = sum(shimau.ns.cnt$n)) %>%
  mutate(hide_tot = 113) %>%
  gather(value = "n", key = "type") %>%
  ggplot(., aes(x="", y=n, fill=type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.title=element_blank(), legend.title = element_blank(), legend.direction = "vertical", legend.position="bottom") +
  scale_fill_discrete(labels = c('"прятать"', "смысловой", "вспомогательный")) +
  geom_text(aes(y = n/3 + c(0, cumsum(n)[-length(n)]), 
                label = n), size = 4)
dev.off() 


# - - - - KIRERU - - - -

kireru.ns.cnt <- read.csv("kireru_cnt_ns.csv", encoding = 'UTF-8')
kireru.vs.cnt <- read.csv("kireru_cnt_vs.csv", encoding = 'UTF-8')

cairo_pdf('kireru_v.pdf' , family="Yu Mincho")
kireru.vs.cnt %>%
  ggplot(., aes(x=subj, y=n, fill=subj)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom")
dev.off() 

cairo_pdf('kireru_n.pdf' , family="Yu Mincho")
kireru.ns.cnt %>%
  ggplot(., aes(x=subj, y=cnt, fill=subj)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom")
dev.off() 

cairo_pdf('kireru_tot.pdf' , family="Yu Mincho")
kireru.vs.cnt %>%
  summarise(v_tot = sum(n)) %>%
  mutate(n_tot = sum(kireru.ns.cnt$cnt)) %>%
  gather(value = "n", key = "type") %>%
  ggplot(., aes(x="", y=n, fill=type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.title=element_blank(), legend.title = element_blank(), legend.direction = "vertical", legend.position="bottom") +
  scale_fill_discrete(labels = c("смысловой", "вспомогательный")) +
  geom_text(aes(y = n/3 + c(0, cumsum(n)[-length(n)]), 
                label = n), size = 4)
dev.off() 


# - - - - NAKUNARU_NOT_DIE - - - -

nakunaru_not_die.all.ns <- read.csv("nakunaru_not_die_ns.csv", encoding = "UTF-8")

cairo_pdf('nakunaru_not_die_ns.pdf' , family="Yu Mincho")
nakunaru_not_die.all.ns %>%
  filter(cnt > 2) %>%
  ggplot(., aes(x=subj, y=cnt, fill=subj)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom")
dev.off()
