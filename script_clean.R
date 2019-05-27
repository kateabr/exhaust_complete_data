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
  filter(cnt > 1) %>%
  ggplot(., aes(x=subj, y=cnt, fill=subj)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom")
dev.off() 

cairo_pdf('yam_tot.pdf' , family="Yu Mincho")
yam.cnt.vs %>%
  mutate(v_tot = sum(n)) %>%
  select(v_tot) %>%
  unique() %>%
  mutate(n_tot = sum(yam.cnt.ns$cnt)) %>%
  gather(value = "n", key = "type") %>%
  ggplot(., aes(x="", y=n, fill=type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.title=element_blank(), legend.title = element_blank(), legend.direction = "vertical", legend.position="bottom") +
  scale_fill_discrete(labels = c("с существительным", "с глаголом")) +
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
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
dev.off() 

cairo_pdf('tukiru_n.pdf' , family="Yu Mincho")
tukiru.ns.cnt %>%
  filter(cnt > 1) %>%
  ggplot(., aes(x=subj, y=cnt, fill=subj)) +
  geom_bar(width = 1, stat = "identity") +
  labs(x = "Слово", y = "Количество вхождений") +
  guides(fill = guide_legend(title = element_blank()))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
dev.off()

cairo_pdf('tukiru_tot.pdf' , family="Yu Mincho")
tukiru.vs.cnt %>%
  mutate(v_tot = sum(n)) %>%
  select(v_tot) %>%
  unique() %>%
  mutate(n_tot = sum(tukiru.ns.cnt$cnt)) %>%
  gather(value = "n", key = "type") %>%
  ggplot(., aes(x="", y=n, fill=type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.title=element_blank(), legend.title = element_blank()) +
  scale_fill_discrete(labels = c("смысловой", "вспомогательный")) +
  geom_text(aes(y = n/3 + c(0, cumsum(n)[-length(n)]), 
                label = n), size = 4)
dev.off()