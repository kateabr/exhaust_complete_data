library(tidyverse)
library(ggplot2)
library(ggmosaic)

draw_tot <- function(verb, noun) {
  lbls <- c("сольное", "в составе композита")
  
  verb %>%
    summarise(v_tot = sum(n)) %>%
    mutate(n_tot = sum(noun$n)) %>%
    gather(value = "n", key = "type") %>%
    ggplot(., aes(x="", y=n, fill=type)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) +
    theme(axis.title=element_blank(), legend.title = element_blank(), legend.direction = "vertical", legend.position="bottom") +
    scale_fill_discrete(labels = lbls) +
    geom_text(aes(y = n/3 + c(0, cumsum(n)[-length(n)]), 
                  label = n), size = 4)
}

draw_spec <- function(table, threshold) {
  table %>%
    select(-X,-subj) %>%
    group_by(translation) %>%
    summarise(cnt = sum(n)) %>%
    filter(cnt > threshold) -> table
  table %>%
    ggplot(., aes(x=translation, y=cnt, fill=translation)) +
    geom_bar(width = 1, stat = "identity") +
    labs(x = "Слово", y = "Количество вхождений") +
    guides(fill = guide_legend(title = element_blank()))+
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.direction = "horizontal", legend.position="bottom") +
    geom_label(label = table$cnt, show.legend = FALSE)
}

# - - - - YAMU - - - -

yam.cnt.ns <- read.csv("csv/yam_ns_cnt.csv", encoding = 'UTF-8')
yam.cnt.vs <- read.csv("csv/yam_vs_cnt.csv", encoding = 'UTF-8')

cairo_pdf('pdf/yam_v.pdf', width = 6.5, height = 3.5, family = "FreeSans")
draw_spec(yam.cnt.vs, threshold = 1)
dev.off() 

cairo_pdf('pdf/yam_n.pdf', width = 6.5, height = 3.5, family = "FreeSans")
draw_spec(yam.cnt.ns, threshold = 2)
dev.off() 

cairo_pdf('pdf/yam_tot.pdf', width = 2.6, height = 3.5, family = "FreeSans")
draw_tot(yam.cnt.vs, yam.cnt.ns)
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
draw_spec(tukiru.vs.cnt, threshold = 0)
dev.off() 

cairo_pdf('pdf/tukiru_n.pdf', width = 6.5, height = 3.5, family = "FreeSans")
draw_spec(tukiru.ns.cnt, threshold = 1)
dev.off() 

cairo_pdf('pdf/tukiru_tot.pdf', width = 2.6, height = 3.5, family = "FreeSans")
draw_tot(tukiru.vs.cnt, tukiru.ns.cnt)
dev.off()


# - - - - HATERU - - - -

hateru.vs.cnt <- read.csv("csv/hateru_cnt_vs.csv", encoding = "UTF-8")
hateru.ns.cnt <- read.csv("csv/hateru_cnt_ns.csv", encoding = "UTF-8")

cairo_pdf('pdf/hateru_v.pdf', width = 6.5, height = 3.5, family = "FreeSans")
draw_spec(hateru.vs.cnt, threshold = 0)
dev.off() 

cairo_pdf('pdf/hateru_n.pdf', width = 6.5, height = 3.5, family = "FreeSans")
draw_spec(hateru.ns.cnt, threshold = 1)
dev.off()

cairo_pdf('pdf/hateru_tot.pdf', width = 2.6, height = 3.5, family = "FreeSans")
draw_tot(hateru.vs.cnt, hateru.ns.cnt)
dev.off()

# - - - - OWARU - - - -

owaru.ns.cnt <- read.csv("csv/owaru_cnt_ns.csv", encoding = "UTF-8")
owaru.vs.cnt <- read.csv("csv/owaru_cnt_vs.csv", encoding = "UTF-8")

cairo_pdf('pdf/owaru_ns.pdf', width = 6.5, height = 3.5, family = "FreeSans")
draw_spec(owaru.ns.cnt, threshold = 1)
dev.off()

cairo_pdf('pdf/owaru_vs.pdf', width = 6.5, height = 3.5, family = "FreeSans")
draw_spec(owaru.vs.cnt, threshold = 0)
dev.off()

cairo_pdf('pdf/owaru_tot.pdf', width = 2.6, height = 3.5, family = "FreeSans")
draw_tot(owaru.vs.cnt, owaru.ns.cnt)
dev.off()

# - - - - TAERU - - - -

taeru.ns.cnt <- read.csv("csv/taeru_cnt_ns.csv", encoding = 'UTF-8')
taeru.vs.cnt <- read.csv("csv/taeru_cnt_vs.csv", encoding = 'UTF-8')

cairo_pdf('pdf/taeru_v.pdf', width = 6.5, height = 3.5, family = "FreeSans")
draw_spec(taeru.vs.cnt, threshold = 0)
dev.off() 

cairo_pdf('pdf/taeru_n.pdf', width = 6.5, height = 3.5, family = "FreeSans")
draw_spec(taeru.ns.cnt, threshold = 1)
dev.off() 

cairo_pdf('pdf/taeru_tot.pdf', width = 2.6, height = 3.5, family = "FreeSans")
draw_tot(taeru.vs.cnt, taeru.ns.cnt)
dev.off() 

# - - - - KIRERU - - - -

kireru.ns.cnt <- read.csv("csv/kireru_cnt_ns.csv", encoding = 'UTF-8')
kireru.vs.cnt <- read.csv("csv/kireru_cnt_vs.csv", encoding = 'UTF-8')

cairo_pdf('pdf/kireru_v.pdf', width = 6.5, height = 3.5, family = "FreeSans")
draw_spec(kireru.vs.cnt, threshold = 0)
dev.off() 

cairo_pdf('pdf/kireru_n.pdf', width = 6.5, height = 3.5, family = "FreeSans")
draw_spec(kireru.ns.cnt, threshold = 0)
dev.off() 

cairo_pdf('pdf/kireru_tot.pdf', width = 2.6, height = 3.5, family = "FreeSans")
kireru.vs.cnt %>%
  group_by(non_omonym) %>%
  summarise(v_tot = sum(n)) %>%
  spread(value = "v_tot", key = "non_omonym") %>%
  mutate(yes_noun = sum(kireru.ns.cnt$n)) %>%
  gather(value = "n", key = "type") -> kireru.stats
kireru.stats %>%
  ggplot(., aes(x="", y=n, fill=type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.title=element_blank(), legend.title = element_blank(), legend.direction = "vertical", legend.position="bottom") +
  scale_fill_discrete(labels = c("сольное", "омонимичное вхождение", "в составе композита")) +
  geom_text(aes(y = n/3 + c(0, cumsum(n)[-length(n)]), 
                label = n), size = 4)
dev.off() 


# - - - - NAKUNARU_NOT_DIE - - - -

nakunaru_not_die.all.ns <- read.csv("csv/nakunaru_not_die_ns.csv", encoding = "UTF-8")

cairo_pdf('pdf/nakunaru_not_die_ns.pdf', width = 6.5, height = 3.5, family = "FreeSans")
draw_spec(nakunaru_not_die.all.ns, threshold = 2)
dev.off()
