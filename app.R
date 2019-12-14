library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(RNHANES)
library(weights)
library(ggsci)
library(ggthemes)
library(gridExtra)
library(dslabs)
library(RColorBrewer)
library(scales)
library(RCurl)
library(httr)
library(haven)
library(survey)
library(dplyr)
library(ggrepel)

# Load datasets

d99<-read_xpt("DEMO.XPT") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  transmute(SEQN=SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  left_join(read_xpt("BMX.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI) %>%
  left_join(read_xpt("WHQ.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070) %>%
  left_join(read_xpt("FSQ.XPT"), by="SEQN") %>%
  select(SEQN,  RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC) %>%
  left_join(read_xpt("DIQ.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010) %>%
  left_join(read_xpt("SMQ.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010, SMQ040, SMD070)
d99$wave <- rep("1999-2000",nrow(d99))

d01<-read_xpt("DEMO_B.XPT") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  transmute(SEQN=SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  left_join(read_xpt("BMX_B.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI) %>%
  left_join(read_xpt("WHQ_B.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070) %>%
  left_join(read_xpt("FSQ_B.XPT"), by="SEQN") %>%
  select(SEQN,  RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC) %>%
  left_join(read_xpt("DIQ_B.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010) %>%
  left_join(read_xpt("SMQ_B.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010, SMQ040, SMD070)
d01$wave <- rep("2001-2002",nrow(d01))

d03<-read_xpt("DEMO_C.XPT") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  transmute(SEQN=SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  left_join(read_xpt("BMX_C.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI) %>%
  left_join(read_xpt("WHQ_C.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070) %>%
  left_join(read_xpt("FSQ_C.XPT"), by="SEQN") %>%
  select(SEQN,  RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, FSDHH, FSDAD) %>%
  transmute(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,  RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC=FSDHH, ADFDSEC=FSDAD) %>%
  left_join(read_xpt("DIQ_C.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010) %>%
  left_join(read_xpt("SMQ_C.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010, SMQ040, SMD070)
d03$wave <- rep("2003-2004",nrow(d03))

d05<-read_xpt("DEMO_D.XPT") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  transmute(SEQN=SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  left_join(read_xpt("BMX_D.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI) %>%
  left_join(read_xpt("WHQ_D.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070) %>%
  left_join(read_xpt("FSQ_D.XPT"), by="SEQN") %>%
  select(SEQN,  RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, FSDHH, FSDAD) %>%
  transmute(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,  RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC=FSDHH, ADFDSEC=FSDAD) %>%
  left_join(read_xpt("DIQ_D.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010) %>%
  left_join(read_xpt("SMQ_D.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010, SMQ040, SMD070)
d05$wave <- rep("2005-2006",nrow(d05))

d07<-read_xpt("DEMO_E.XPT") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,RIDEXPRG, INDFMIN2, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  transmute(SEQN=SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC=INDFMIN2, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  left_join(read_xpt("BMX_E.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI) %>%
  left_join(read_xpt("WHQ_E.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070) %>%
  left_join(read_xpt("FSQ_E.XPT"), by="SEQN") %>%
  select(SEQN,  RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, FSDHH, FSDAD) %>%
  transmute(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,  RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC=FSDHH, ADFDSEC=FSDAD) %>%
  left_join(read_xpt("DIQ_E.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010) %>%
  left_join(read_xpt("SMQ_E.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010, SMQ040, SMD650) %>%
  transmute(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,  RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010, SMQ040, SMD070 = SMD650)
d07$wave <- rep("2007-2008",nrow(d07))

d09<-read_xpt("DEMO_F.XPT") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,RIDEXPRG, INDFMIN2, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  transmute(SEQN=SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC=INDFMIN2, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  left_join(read_xpt("BMX_F.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI) %>%
  left_join(read_xpt("WHQ_F.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070) %>%
  left_join(read_xpt("FSQ_F.XPT"), by="SEQN") %>%
  select(SEQN,  RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, FSDHH, FSDAD) %>%
  transmute(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,  RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC=FSDHH, ADFDSEC=FSDAD) %>%
  left_join(read_xpt("DIQ_F.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010) %>%
  left_join(read_xpt("SMQ_F.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010, SMQ040, SMD650) %>%
  transmute(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,  RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010, SMQ040, SMD070 = SMD650)
d09$wave <- rep("2009-2010",nrow(d09))

d11<-read_xpt("DEMO_G.XPT") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,RIDEXPRG, INDFMIN2, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  transmute(SEQN=SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC=INDFMIN2, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  left_join(read_xpt("BMX_G.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI) %>%
  left_join(read_xpt("WHQ_G.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070) %>%
  left_join(read_xpt("FSQ_G.XPT"), by="SEQN") %>%
  select(SEQN,  RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, FSDHH, FSDAD) %>%
  transmute(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,  RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC=FSDHH, ADFDSEC=FSDAD) %>%
  left_join(read_xpt("DIQ_G.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010) %>%
  left_join(read_xpt("SMQ_G.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010, SMQ040, SMD650) %>%
  transmute(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,  RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010, SMQ040, SMD070 = SMD650)
d11$wave <- rep("2011-2012",nrow(d11))

d11<-read_xpt("DEMO_G.XPT") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,RIDEXPRG, INDFMIN2, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  transmute(SEQN=SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC=INDFMIN2, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  left_join(read_xpt("BMX_G.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI) %>%
  left_join(read_xpt("WHQ_G.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070) %>%
  left_join(read_xpt("FSQ_G.XPT"), by="SEQN") %>%
  select(SEQN,  RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, FSDHH, FSDAD) %>%
  transmute(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,  RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC=FSDHH, ADFDSEC=FSDAD) %>%
  left_join(read_xpt("DIQ_G.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010) %>%
  left_join(read_xpt("SMQ_G.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010, SMQ040, SMD650) %>%
  transmute(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,  RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010, SMQ040, SMD070 = SMD650)
d11$wave <- rep("2011-2012",nrow(d11))

d13<-read_xpt("DEMO_H.XPT") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,RIDEXPRG, INDFMIN2, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  transmute(SEQN=SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC=INDFMIN2, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  left_join(read_xpt("BMX_H.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI) %>%
  left_join(read_xpt("WHQ_H.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070) %>%
  left_join(read_xpt("FSQ_H.XPT"), by="SEQN") %>%
  select(SEQN,  RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, FSDHH, FSDAD) %>%
  transmute(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,  RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC=FSDHH, ADFDSEC=FSDAD) %>%
  left_join(read_xpt("DIQ_H.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010) %>%
  left_join(read_xpt("SMQ_H.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010, SMQ040, SMD650) %>%
  transmute(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,  RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010, SMQ040, SMD070 = SMD650)
d13$wave <- rep("2013-2014",nrow(d13))

d15<-read_xpt("DEMO_I.XPT") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,RIDEXPRG, INDFMIN2, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  transmute(SEQN=SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC=INDFMIN2, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  left_join(read_xpt("BMX_I.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI) %>%
  left_join(read_xpt("WHQ_I.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070) %>%
  left_join(read_xpt("DIQ_I.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, DIQ010) %>%
  left_join(read_xpt("SMQ_I.XPT"), by="SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, DIQ010, SMQ040, SMD650) %>%
  transmute(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1,  RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, DIQ010, SMQ040, SMD070 = SMD650)
d15$HHFDSEC <- rep(NA,nrow(d15))
d15$ADFDSEC <- rep(NA,nrow(d15))
d15$wave <- rep("2015-2016",nrow(d15))

dat <- rbind(d99, d01, d03, d05, d07, d09, d11, d13, d15) %>%
  mutate(
    food_security = recode_factor(HHFDSEC,
                                  `1` = 'Full food security',
                                  `2` = 'Marginal food security',
                                  `3` = 'Low food security',
                                  `4` = 'Very low food security'),
    gender = recode_factor(RIAGENDR,
                           `1` = "Male",
                           `2` = "Female"),
    race = recode_factor(RIDRETH1,
                         `1` = "Mexican American",
                         `2` = "Hispanic",
                         `3` = "Non-Hispanic, White",
                         `4` = "Non-Hispanic, Black",
                         `5` = "Others"),
    diabetes = if_else(DIQ010 == 1, "Yes", "No"),
    tryweloss = if_else(WHQ070 == 1, "Yes", "No"),
    cursmoke = if_else(SMQ040 == 1, "Every Day", if_else(SMQ040 == 2, "Some days", "Not at all")),
    bmi_cat = if_else(BMXBMI >= 30, '4', if_else(BMXBMI >= 25 & BMXBMI < 30, '3', if_else(BMXBMI >= 18.5 & BMXBMI < 25, '2', '1'))),
    age_cat = if_else(RIDAGEYR >= 18, 'Adult', 'Youth'),
    poverty_cat = if_else(INDFMPIR >= 5, 
                          '5', if_else(INDFMPIR < 5 & INDFMPIR >= 4, 
                                       '4', if_else(INDFMPIR < 4 & INDFMPIR >= 3, 
                                                    '3', if_else(INDFMPIR < 3 & INDFMPIR >= 2, 
                                                                 '2', ifelse(INDFMPIR < 2 & INDFMPIR >= 1, 
                                                                             '1', '0')))))
  ) 

food_data_9900 <- read_xpt("DRXIFF.XPT") %>%
  select(SEQN, DRDIFDCD, DRXIGRMS) %>% 
  transmute(SEQN=SEQN, START = DRDIFDCD, grams = DRXIGRMS) %>%
  left_join(read_xpt("DRXFMT.XPT"), by = "START") %>%
  select(SEQN, START, grams, LABEL) %>%
  transmute(SEQN, food_code = as.numeric(START), grams, label = LABEL) %>%
  left_join(read.csv("WWEIA1516_foodcat_FNDDS.csv"), by = "food_code") %>%
  select(SEQN, food_code, grams, category_description, label, category_number)
food_data_9900$wave <- rep("1999-2000",nrow(food_data_9900))

food_data_0102 <- read_xpt("DRXIFF_B.XPT") %>%
  select(SEQN, DRDIFDCD, DRXIGRMS) %>% 
  transmute(SEQN=SEQN, START = DRDIFDCD, grams = DRXIGRMS) %>%
  left_join(read_xpt("DRXFMT_B.XPT"), by = "START") %>%
  select(SEQN, START, grams, LABEL) %>%
  transmute(SEQN, food_code = as.numeric(START), grams, label = LABEL) %>%
  left_join(read.csv("WWEIA1516_foodcat_FNDDS.csv"), by = "food_code") %>%
  select(SEQN, food_code, grams, category_description, label, category_number)
food_data_0102$wave <- rep("2001-2002",nrow(food_data_0102))

food_data_0304_day1 <- read_xpt("DR1IFF_C.XPT") %>%
  select(SEQN, DR1IFDCD, DR1IGRMS) %>% 
  transmute(SEQN=SEQN, DRXFDCD = DR1IFDCD, grams = DR1IGRMS) %>%
  left_join(read_xpt("DRXFCD_C.XPT"), by = "DRXFDCD") %>%
  select(SEQN, DRXFDCD, grams, DRXFCSD) %>%
  transmute(SEQN, food_code = as.numeric(DRXFDCD), grams, label = DRXFCSD) %>%
  left_join(read.csv("WWEIA1516_foodcat_FNDDS.csv"), by = "food_code") %>%
  select(SEQN, food_code, grams, category_description, label, category_number)
food_data_0304_day1$wave <- rep("2003-2004",nrow(food_data_0304_day1))

food_data_0304_day2 <- read_xpt("DR2IFF_C.XPT") %>%
  select(SEQN, DR2IFDCD, DR2IGRMS) %>% 
  transmute(SEQN=SEQN, DRXFDCD = DR2IFDCD, grams = DR2IGRMS) %>%
  left_join(read_xpt("DRXFCD_C.XPT"), by = "DRXFDCD") %>%
  select(SEQN, DRXFDCD, grams, DRXFCSD) %>%
  transmute(SEQN, food_code = as.numeric(DRXFDCD), grams, label = DRXFCSD) %>%
  left_join(read.csv("WWEIA1516_foodcat_FNDDS.csv"), by = "food_code") %>%
  select(SEQN, food_code, grams, category_description, label, category_number)
food_data_0304_day2$wave <- rep("2003-2004",nrow(food_data_0304_day2))

food_data_0506_day1 <- read_xpt("DR1IFF_D.XPT") %>%
  select(SEQN, DR1IFDCD, DR1IGRMS) %>% 
  transmute(SEQN=SEQN, DRXFDCD = DR1IFDCD, grams = DR1IGRMS) %>%
  left_join(read_xpt("DRXFCD_D.XPT"), by = "DRXFDCD") %>%
  select(SEQN, DRXFDCD, grams, DRXFCSD) %>%
  transmute(SEQN, food_code = as.numeric(DRXFDCD), grams, label = DRXFCSD) %>%
  left_join(read.csv("WWEIA1516_foodcat_FNDDS.csv"), by = "food_code") %>%
  select(SEQN, food_code, grams, category_description, label, category_number)
food_data_0506_day1$wave <- rep("2005-2006",nrow(food_data_0506_day1))

food_data_0506_day2 <- read_xpt("DR2IFF_D.XPT") %>%
  select(SEQN, DR2IFDCD, DR2IGRMS) %>% 
  transmute(SEQN=SEQN, DRXFDCD = DR2IFDCD, grams = DR2IGRMS) %>%
  left_join(read_xpt("DRXFCD_D.XPT"), by = "DRXFDCD") %>%
  select(SEQN, DRXFDCD, grams, DRXFCSD) %>%
  transmute(SEQN, food_code = as.numeric(DRXFDCD), grams, label = DRXFCSD) %>%
  left_join(read.csv("WWEIA1516_foodcat_FNDDS.csv"), by = "food_code") %>%
  select(SEQN, food_code, grams, category_description, label, category_number)
food_data_0506_day2$wave <- rep("2005-2006",nrow(food_data_0506_day2))

food_data_0708_day1 <- read_xpt("DR1IFF_E.XPT") %>%
  select(SEQN, DR1IFDCD, DR1IGRMS) %>% 
  transmute(SEQN=SEQN, DRXFDCD = DR1IFDCD, grams = DR1IGRMS) %>%
  left_join(read_xpt("DRXFCD_E.XPT"), by = "DRXFDCD") %>%
  select(SEQN, DRXFDCD, grams, DRXFCSD) %>%
  transmute(SEQN, food_code = as.numeric(DRXFDCD), grams, label = DRXFCSD) %>%
  left_join(read.csv("WWEIA1516_foodcat_FNDDS.csv"), by = "food_code") %>%
  select(SEQN, food_code, grams, category_description, label, category_number)
food_data_0708_day1$wave <- rep("2007-2008",nrow(food_data_0708_day1))

food_data_0708_day2 <- read_xpt("DR2IFF_E.XPT") %>%
  select(SEQN, DR2IFDCD, DR2IGRMS) %>% 
  transmute(SEQN=SEQN, DRXFDCD = DR2IFDCD, grams = DR2IGRMS) %>%
  left_join(read_xpt("DRXFCD_E.XPT"), by = "DRXFDCD") %>%
  select(SEQN, DRXFDCD, grams, DRXFCSD) %>%
  transmute(SEQN, food_code = as.numeric(DRXFDCD), grams, label = DRXFCSD) %>%
  left_join(read.csv("WWEIA1516_foodcat_FNDDS.csv"), by = "food_code") %>%
  select(SEQN, food_code, grams, category_description, label, category_number)
food_data_0708_day2$wave <- rep("2007-2008",nrow(food_data_0708_day2))

food_data_0910_day1 <- read_xpt("DR1IFF_F.XPT") %>%
  select(SEQN, DR1IFDCD, DR1IGRMS) %>% 
  transmute(SEQN=SEQN, DRXFDCD = DR1IFDCD, grams = DR1IGRMS) %>%
  left_join(read_xpt("DRXFCD_F.XPT"), by = "DRXFDCD") %>%
  select(SEQN, DRXFDCD, grams, DRXFCSD) %>%
  transmute(SEQN, food_code = as.numeric(DRXFDCD), grams, label = DRXFCSD) %>%
  left_join(read.csv("WWEIA1516_foodcat_FNDDS.csv"), by = "food_code") %>%
  select(SEQN, food_code, grams, category_description, label, category_number)
food_data_0910_day1$wave <- rep("2009-2010",nrow(food_data_0910_day1))

food_data_0910_day2 <- read_xpt("DR2IFF_F.XPT") %>%
  select(SEQN, DR2IFDCD, DR2IGRMS) %>% 
  transmute(SEQN=SEQN, DRXFDCD = DR2IFDCD, grams = DR2IGRMS) %>%
  left_join(read_xpt("DRXFCD_F.XPT"), by = "DRXFDCD") %>%
  select(SEQN, DRXFDCD, grams, DRXFCSD) %>%
  transmute(SEQN, food_code = as.numeric(DRXFDCD), grams, label = DRXFCSD) %>%
  left_join(read.csv("WWEIA1516_foodcat_FNDDS.csv"), by = "food_code") %>%
  select(SEQN, food_code, grams, category_description, label, category_number)
food_data_0910_day2$wave <- rep("2009-2010",nrow(food_data_0910_day2))

food_data_1112_day1 <- read_xpt("DR1IFF_G.XPT") %>%
  select(SEQN, DR1IFDCD, DR1IGRMS) %>% 
  transmute(SEQN=SEQN, DRXFDCD = DR1IFDCD, grams = DR1IGRMS) %>%
  left_join(read_xpt("DRXFCD_G.XPT"), by = "DRXFDCD") %>%
  select(SEQN, DRXFDCD, grams, DRXFCSD) %>%
  transmute(SEQN, food_code = as.numeric(DRXFDCD), grams, label = DRXFCSD) %>%
  left_join(read.csv("WWEIA1516_foodcat_FNDDS.csv"), by = "food_code") %>%
  select(SEQN, food_code, grams, category_description, label, category_number)
food_data_1112_day1$wave <- rep("2011-2012",nrow(food_data_1112_day1))

food_data_1112_day2 <- read_xpt("DR2IFF_G.XPT") %>%
  select(SEQN, DR2IFDCD, DR2IGRMS) %>% 
  transmute(SEQN=SEQN, DRXFDCD = DR2IFDCD, grams = DR2IGRMS) %>%
  left_join(read_xpt("DRXFCD_G.XPT"), by = "DRXFDCD") %>%
  select(SEQN, DRXFDCD, grams, DRXFCSD) %>%
  transmute(SEQN, food_code = as.numeric(DRXFDCD), grams, label = DRXFCSD) %>%
  left_join(read.csv("WWEIA1516_foodcat_FNDDS.csv"), by = "food_code") %>%
  select(SEQN, food_code, grams, category_description, label, category_number)
food_data_1112_day2$wave <- rep("2011-2012",nrow(food_data_1112_day2))

food_data_1314_day1 <- read_xpt("DR1IFF_H.XPT") %>%
  select(SEQN, DR1IFDCD, DR1IGRMS) %>% 
  transmute(SEQN=SEQN, DRXFDCD = DR1IFDCD, grams = DR1IGRMS) %>%
  left_join(read_xpt("DRXFCD_H.XPT"), by = "DRXFDCD") %>%
  select(SEQN, DRXFDCD, grams, DRXFCSD) %>%
  transmute(SEQN, food_code = as.numeric(DRXFDCD), grams, label = DRXFCSD) %>%
  left_join(read.csv("WWEIA1516_foodcat_FNDDS.csv"), by = "food_code") %>%
  select(SEQN, food_code, grams, category_description, label, category_number)
food_data_1314_day1$wave <- rep("2013-2014",nrow(food_data_1314_day1))

food_data_1314_day2 <- read_xpt("DR2IFF_H.XPT") %>%
  select(SEQN, DR2IFDCD, DR2IGRMS) %>% 
  transmute(SEQN=SEQN, DRXFDCD = DR2IFDCD, grams = DR2IGRMS) %>%
  left_join(read_xpt("DRXFCD_H.XPT"), by = "DRXFDCD") %>%
  select(SEQN, DRXFDCD, grams, DRXFCSD) %>%
  transmute(SEQN, food_code = as.numeric(DRXFDCD), grams, label = DRXFCSD) %>%
  left_join(read.csv("WWEIA1516_foodcat_FNDDS.csv"), by = "food_code") %>%
  select(SEQN, food_code, grams, category_description, label, category_number)
food_data_1314_day2$wave <- rep("2013-2014",nrow(food_data_1314_day2))

food_data_1516_day1 <- read_xpt("DR1IFF_I.XPT") %>%
  select(SEQN, DR1IFDCD, DR1IGRMS) %>% 
  transmute(SEQN=SEQN, DRXFDCD = DR1IFDCD, grams = DR1IGRMS) %>%
  left_join(read_xpt("DRXFCD_I.XPT"), by = "DRXFDCD") %>%
  select(SEQN, DRXFDCD, grams, DRXFCSD) %>%
  transmute(SEQN, food_code = as.numeric(DRXFDCD), grams, label = DRXFCSD) %>%
  left_join(read.csv("WWEIA1516_foodcat_FNDDS.csv"), by = "food_code") %>%
  select(SEQN, food_code, grams, category_description, label, category_number)
food_data_1516_day1$wave <- rep("2015-2016",nrow(food_data_1516_day1))

food_data_1516_day2 <- read_xpt("DR2IFF_I.XPT") %>%
  select(SEQN, DR2IFDCD, DR2IGRMS) %>% 
  transmute(SEQN=SEQN, DRXFDCD = DR2IFDCD, grams = DR2IGRMS) %>%
  left_join(read_xpt("DRXFCD_I.XPT"), by = "DRXFDCD") %>%
  select(SEQN, DRXFDCD, grams, DRXFCSD) %>%
  transmute(SEQN, food_code = as.numeric(DRXFDCD), grams, label = DRXFCSD) %>%
  left_join(read.csv("WWEIA1516_foodcat_FNDDS.csv"), by = "food_code") %>%
  select(SEQN, food_code, grams, category_description, label, category_number)
food_data_1516_day2$wave <- rep("2015-2016",nrow(food_data_1516_day2))

total_food_data <- rbind(food_data_9900, 
                         food_data_0102, 
                         food_data_0304_day1, 
                         food_data_0304_day2, 
                         food_data_0506_day1, 
                         food_data_0506_day2, 
                         food_data_0708_day1, 
                         food_data_0708_day2, 
                         food_data_0910_day1, 
                         food_data_0910_day2, 
                         food_data_1112_day1, 
                         food_data_1112_day2, 
                         food_data_1314_day1, 
                         food_data_1314_day2, 
                         food_data_1516_day1, 
                         food_data_1516_day2)

full_food_data <- total_food_data %>%
  left_join(dat, by = "SEQN") %>%
  select(SEQN, wave.x,  food_code, grams, category_description, label, category_number, RIAGENDR, RIDAGEYR, RIDRETH1,  RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010, SMQ040, SMD070, food_security, gender, race, diabetes, tryweloss, cursmoke, bmi_cat, age_cat, poverty_cat) %>%
  transmute(SEQN, wave = wave.x,  food_code, grams, category_description, label, category_number, RIAGENDR, RIDAGEYR, RIDRETH1,  RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010, SMQ040, SMD070, food_security, gender, race, diabetes, tryweloss, cursmoke, bmi_cat, age_cat, poverty_cat)

nutrition_data_0102 <- read_xpt("DRXTOT_B.XPT")%>%
  select(SEQN, DRXTKCAL, DRXTPROT, DRXTCARB, DRXTSUGR, DRXTFIBE, DRXTTFAT, DRXTSFAT, DRXTMFAT, DRXTPFAT, DRXTCHOL, DRXTATOC, DRXTRET, DRXTVARA, DRXTACAR, DRXTBCAR, DRXTCRYP, DRXTLYCO, DRXTLZ, DRXTVB1, DRXTVB2, DRXTNIAC, DRXTVB6, DRXTFOLA, DRXTFA, DRXTFF, DRXTFDFE, DRXTVB12, DRXTVC, DRXTVK, DRXTCALC, DRXTPHOS, DRXTMAGN, DRXTIRON, DRXTZINC, DRXTCOPP, DRDTSODI, DRXTPOTA, DRXTSELE, DRXTCAFF, DRXTTHEO, DRXTS040, DRXTS060, DRXTS080, DRXTS100, DRXTS120, DRXTS140, DRXTS160, DRXTS180, DRXTM161, DRXTM181, DRXTM201, DRXTM221, DRXTP182, DRXTP183, DRXTP184, DRXTP204, DRXTP205, DRXTP225, DRXTP226, DRD320GW, DRD330GW) %>% 
  transmute(SEQN, Calories = DRXTKCAL, Protein = DRXTPROT, Carbohydrates = DRXTCARB, Sugar = DRXTSUGR, Fiber = DRXTFIBE, TotalFat = DRXTTFAT, SFAT = DRXTSFAT, MFAT = DRXTMFAT, PFAT = DRXTPFAT, Cholesterol = DRXTCHOL, VitE = DRXTATOC, Retinol = DRXTRET, VitA = DRXTVARA, AlphaCaro = DRXTACAR, BetaCaro = DRXTBCAR,  BetaCryp = DRXTCRYP, Lycopene = DRXTLYCO, Lutein = DRXTLZ, Thiamin = DRXTVB1, Riboflavin = DRXTVB2, Niacin = DRXTNIAC, VitB6 = DRXTVB6, TotFolate = DRXTFOLA, FolicAcid = DRXTFA, FoodFolate = DRXTFF, Folate = DRXTFDFE, VitB12 = DRXTVB12, VitC = DRXTVC, VitK = DRXTVK, Calcium = DRXTCALC, Phosphorus = DRXTPHOS, Magnesium = DRXTMAGN, Iron = DRXTIRON, Zinc = DRXTZINC, Copper = DRXTCOPP, Sodium = DRDTSODI, Potassium = DRXTPOTA, Selenium = DRXTSELE, Caffeine = DRXTCAFF, Theobromine = DRXTTHEO, Butanoic = DRXTS040, Hexanoic = DRXTS060, Octanoic = DRXTS080, Decanoic = DRXTS100, Dodecanoic = DRXTS120, Tetradecanoic = DRXTS140,  Hexadecanoic = DRXTS160, Octadecanoic = DRXTS180, Hexadecenoic = DRXTM161, Octadecenoic = DRXTM181, Eicosenoic = DRXTM201, Docosenoic = DRXTM221, Octadecadienoic = DRXTP182, Octadecatrienoic = DRXTP183, Octadecatetraenoic = DRXTP184, Eicosatetraenoic = DRXTP204, Eicosapentaenoic = DRXTP205, Docosapentaenoic = DRXTP225, Docosahexaenoic = DRXTP226, PlainWater = DRD320GW, TapWater = DRD330GW)
nutrition_data_0102$wave <- rep("2001-2002",nrow(nutrition_data_0102))

nutrition_data_0304_day1 <- read_xpt("DR1TOT_C.XPT")%>%
  select(SEQN, DR1TKCAL, DR1TPROT, DR1TCARB, DR1TSUGR, DR1TFIBE, DR1TTFAT, DR1TSFAT, DR1TMFAT, DR1TPFAT, DR1TCHOL, DR1TATOC, DR1TRET, DR1TVARA, DR1TACAR, DR1TBCAR, DR1TCRYP, DR1TLYCO, DR1TLZ, DR1TVB1, DR1TVB2, DR1TNIAC, DR1TVB6, DR1TFOLA, DR1TFA, DR1TFF, DR1TFDFE, DR1TVB12, DR1TVC, DR1TVK, DR1TCALC, DR1TPHOS, DR1TMAGN, DR1TIRON, DR1TZINC, DR1TCOPP, DR1TSODI, DR1TPOTA, DR1TSELE, DR1TCAFF, DR1TTHEO, DR1TS040, DR1TS060, DR1TS080, DR1TS100, DR1TS120, DR1TS140, DR1TS160, DR1TS180, DR1TM161, DR1TM181, DR1TM201, DR1TM221, DR1TP182, DR1TP183, DR1TP184, DR1TP204, DR1TP205, DR1TP225, DR1TP226, DR1_320, DR1_330) %>% 
  transmute(SEQN, Calories = DR1TKCAL, Protein = DR1TPROT, Carbohydrates = DR1TCARB, Sugar = DR1TSUGR, Fiber = DR1TFIBE, TotalFat = DR1TTFAT, SFAT = DR1TSFAT, MFAT = DR1TMFAT, PFAT = DR1TPFAT, Cholesterol = DR1TCHOL, VitE = DR1TATOC, Retinol = DR1TRET, VitA = DR1TVARA, AlphaCaro = DR1TACAR, BetaCaro = DR1TBCAR,  BetaCryp = DR1TCRYP, Lycopene = DR1TLYCO, Lutein = DR1TLZ, Thiamin = DR1TVB1, Riboflavin = DR1TVB2, Niacin = DR1TNIAC, VitB6 = DR1TVB6, TotFolate = DR1TFOLA, FolicAcid = DR1TFA, FoodFolate = DR1TFF, Folate = DR1TFDFE, VitB12 = DR1TVB12, VitC = DR1TVC, VitK = DR1TVK, Calcium = DR1TCALC, Phosphorus = DR1TPHOS, Magnesium = DR1TMAGN, Iron = DR1TIRON, Zinc = DR1TZINC, Copper = DR1TCOPP, Sodium = DR1TSODI, Potassium = DR1TPOTA, Selenium = DR1TSELE, Caffeine = DR1TCAFF, Theobromine = DR1TTHEO, Butanoic = DR1TS040, Hexanoic = DR1TS060, Octanoic = DR1TS080, Decanoic = DR1TS100, Dodecanoic = DR1TS120, Tetradecanoic = DR1TS140,  Hexadecanoic = DR1TS160, Octadecanoic = DR1TS180, Hexadecenoic = DR1TM161, Octadecenoic = DR1TM181, Eicosenoic = DR1TM201, Docosenoic = DR1TM221, Octadecadienoic = DR1TP182, Octadecatrienoic = DR1TP183, Octadecatetraenoic = DR1TP184, Eicosatetraenoic = DR1TP204, Eicosapentaenoic = DR1TP205, Docosapentaenoic = DR1TP225, Docosahexaenoic = DR1TP226, PlainWater = DR1_320, TapWater = DR1_330)
nutrition_data_0304_day1$wave <- rep("2003-2004",nrow(nutrition_data_0304_day1))

nutrition_data_0304_day2 <- read_xpt("DR2TOT_C.XPT")%>%
  select(SEQN, DR2TKCAL, DR2TPROT, DR2TCARB, DR2TSUGR, DR2TFIBE, DR2TTFAT, DR2TSFAT, DR2TMFAT, DR2TPFAT, DR2TCHOL, DR2TATOC, DR2TRET, DR2TVARA, DR2TACAR, DR2TBCAR, DR2TCRYP, DR2TLYCO, DR2TLZ, DR2TVB1, DR2TVB2, DR2TNIAC, DR2TVB6, DR2TFOLA, DR2TFA, DR2TFF, DR2TFDFE, DR2TVB12, DR2TVC, DR2TVK, DR2TCALC, DR2TPHOS, DR2TMAGN, DR2TIRON, DR2TZINC, DR2TCOPP, DR2TSODI, DR2TPOTA, DR2TSELE, DR2TCAFF, DR2TTHEO, DR2TS040, DR2TS060, DR2TS080, DR2TS100, DR2TS120, DR2TS140, DR2TS160, DR2TS180, DR2TM161, DR2TM181, DR2TM201, DR2TM221, DR2TP182, DR2TP183, DR2TP184, DR2TP204, DR2TP205, DR2TP225, DR2TP226, DR2_320, DR2_330) %>% 
  transmute(SEQN, Calories = DR2TKCAL, Protein = DR2TPROT, Carbohydrates = DR2TCARB, Sugar = DR2TSUGR, Fiber = DR2TFIBE, TotalFat = DR2TTFAT, SFAT = DR2TSFAT, MFAT = DR2TMFAT, PFAT = DR2TPFAT, Cholesterol = DR2TCHOL, VitE = DR2TATOC, Retinol = DR2TRET, VitA = DR2TVARA, AlphaCaro = DR2TACAR, BetaCaro = DR2TBCAR,  BetaCryp = DR2TCRYP, Lycopene = DR2TLYCO, Lutein = DR2TLZ, Thiamin = DR2TVB1, Riboflavin = DR2TVB2, Niacin = DR2TNIAC, VitB6 = DR2TVB6, TotFolate = DR2TFOLA, FolicAcid = DR2TFA, FoodFolate = DR2TFF, Folate = DR2TFDFE, VitB12 = DR2TVB12, VitC = DR2TVC, VitK = DR2TVK, Calcium = DR2TCALC, Phosphorus = DR2TPHOS, Magnesium = DR2TMAGN, Iron = DR2TIRON, Zinc = DR2TZINC, Copper = DR2TCOPP, Sodium = DR2TSODI, Potassium = DR2TPOTA, Selenium = DR2TSELE, Caffeine = DR2TCAFF, Theobromine = DR2TTHEO, Butanoic = DR2TS040, Hexanoic = DR2TS060, Octanoic = DR2TS080, Decanoic = DR2TS100, Dodecanoic = DR2TS120, Tetradecanoic = DR2TS140,  Hexadecanoic = DR2TS160, Octadecanoic = DR2TS180, Hexadecenoic = DR2TM161, Octadecenoic = DR2TM181, Eicosenoic = DR2TM201, Docosenoic = DR2TM221, Octadecadienoic = DR2TP182, Octadecatrienoic = DR2TP183, Octadecatetraenoic = DR2TP184, Eicosatetraenoic = DR2TP204, Eicosapentaenoic = DR2TP205, Docosapentaenoic = DR2TP225, Docosahexaenoic = DR2TP226, PlainWater = DR2_320, TapWater = DR2_330)
nutrition_data_0304_day2$wave <- rep("2003-2004",nrow(nutrition_data_0304_day2))

nutrition_data_0506_day1 <- read_xpt("DR1TOT_D.XPT")%>%
  select(SEQN, DR1TKCAL, DR1TPROT, DR1TCARB, DR1TSUGR, DR1TFIBE, DR1TTFAT, DR1TSFAT, DR1TMFAT, DR1TPFAT, DR1TCHOL, DR1TATOC, DR1TRET, DR1TVARA, DR1TACAR, DR1TBCAR, DR1TCRYP, DR1TLYCO, DR1TLZ, DR1TVB1, DR1TVB2, DR1TNIAC, DR1TVB6, DR1TFOLA, DR1TFA, DR1TFF, DR1TFDFE, DR1TVB12, DR1TVC, DR1TVK, DR1TCALC, DR1TPHOS, DR1TMAGN, DR1TIRON, DR1TZINC, DR1TCOPP, DR1TSODI, DR1TPOTA, DR1TSELE, DR1TCAFF, DR1TTHEO, DR1TS040, DR1TS060, DR1TS080, DR1TS100, DR1TS120, DR1TS140, DR1TS160, DR1TS180, DR1TM161, DR1TM181, DR1TM201, DR1TM221, DR1TP182, DR1TP183, DR1TP184, DR1TP204, DR1TP205, DR1TP225, DR1TP226, DR1_320Z, DR1_330Z) %>% 
  transmute(SEQN, Calories = DR1TKCAL, Protein = DR1TPROT, Carbohydrates = DR1TCARB, Sugar = DR1TSUGR, Fiber = DR1TFIBE, TotalFat = DR1TTFAT, SFAT = DR1TSFAT, MFAT = DR1TMFAT, PFAT = DR1TPFAT, Cholesterol = DR1TCHOL, VitE = DR1TATOC, Retinol = DR1TRET, VitA = DR1TVARA, AlphaCaro = DR1TACAR, BetaCaro = DR1TBCAR,  BetaCryp = DR1TCRYP, Lycopene = DR1TLYCO, Lutein = DR1TLZ, Thiamin = DR1TVB1, Riboflavin = DR1TVB2, Niacin = DR1TNIAC, VitB6 = DR1TVB6, TotFolate = DR1TFOLA, FolicAcid = DR1TFA, FoodFolate = DR1TFF, Folate = DR1TFDFE, VitB12 = DR1TVB12, VitC = DR1TVC, VitK = DR1TVK, Calcium = DR1TCALC, Phosphorus = DR1TPHOS, Magnesium = DR1TMAGN, Iron = DR1TIRON, Zinc = DR1TZINC, Copper = DR1TCOPP, Sodium = DR1TSODI, Potassium = DR1TPOTA, Selenium = DR1TSELE, Caffeine = DR1TCAFF, Theobromine = DR1TTHEO, Butanoic = DR1TS040, Hexanoic = DR1TS060, Octanoic = DR1TS080, Decanoic = DR1TS100, Dodecanoic = DR1TS120, Tetradecanoic = DR1TS140,  Hexadecanoic = DR1TS160, Octadecanoic = DR1TS180, Hexadecenoic = DR1TM161, Octadecenoic = DR1TM181, Eicosenoic = DR1TM201, Docosenoic = DR1TM221, Octadecadienoic = DR1TP182, Octadecatrienoic = DR1TP183, Octadecatetraenoic = DR1TP184, Eicosatetraenoic = DR1TP204, Eicosapentaenoic = DR1TP205, Docosapentaenoic = DR1TP225, Docosahexaenoic = DR1TP226, PlainWater = DR1_320Z, TapWater = DR1_330Z)
nutrition_data_0506_day1$wave <- rep("2005-2006",nrow(nutrition_data_0506_day1))

nutrition_data_0506_day2 <- read_xpt("DR2TOT_D.XPT")%>%
  select(SEQN, DR2TKCAL, DR2TPROT, DR2TCARB, DR2TSUGR, DR2TFIBE, DR2TTFAT, DR2TSFAT, DR2TMFAT, DR2TPFAT, DR2TCHOL, DR2TATOC, DR2TRET, DR2TVARA, DR2TACAR, DR2TBCAR, DR2TCRYP, DR2TLYCO, DR2TLZ, DR2TVB1, DR2TVB2, DR2TNIAC, DR2TVB6, DR2TFOLA, DR2TFA, DR2TFF, DR2TFDFE, DR2TVB12, DR2TVC, DR2TVK, DR2TCALC, DR2TPHOS, DR2TMAGN, DR2TIRON, DR2TZINC, DR2TCOPP, DR2TSODI, DR2TPOTA, DR2TSELE, DR2TCAFF, DR2TTHEO, DR2TS040, DR2TS060, DR2TS080, DR2TS100, DR2TS120, DR2TS140, DR2TS160, DR2TS180, DR2TM161, DR2TM181, DR2TM201, DR2TM221, DR2TP182, DR2TP183, DR2TP184, DR2TP204, DR2TP205, DR2TP225, DR2TP226, DR2_320Z, DR2_330Z) %>% 
  transmute(SEQN, Calories = DR2TKCAL, Protein = DR2TPROT, Carbohydrates = DR2TCARB, Sugar = DR2TSUGR, Fiber = DR2TFIBE, TotalFat = DR2TTFAT, SFAT = DR2TSFAT, MFAT = DR2TMFAT, PFAT = DR2TPFAT, Cholesterol = DR2TCHOL, VitE = DR2TATOC, Retinol = DR2TRET, VitA = DR2TVARA, AlphaCaro = DR2TACAR, BetaCaro = DR2TBCAR,  BetaCryp = DR2TCRYP, Lycopene = DR2TLYCO, Lutein = DR2TLZ, Thiamin = DR2TVB1, Riboflavin = DR2TVB2, Niacin = DR2TNIAC, VitB6 = DR2TVB6, TotFolate = DR2TFOLA, FolicAcid = DR2TFA, FoodFolate = DR2TFF, Folate = DR2TFDFE, VitB12 = DR2TVB12, VitC = DR2TVC, VitK = DR2TVK, Calcium = DR2TCALC, Phosphorus = DR2TPHOS, Magnesium = DR2TMAGN, Iron = DR2TIRON, Zinc = DR2TZINC, Copper = DR2TCOPP, Sodium = DR2TSODI, Potassium = DR2TPOTA, Selenium = DR2TSELE, Caffeine = DR2TCAFF, Theobromine = DR2TTHEO, Butanoic = DR2TS040, Hexanoic = DR2TS060, Octanoic = DR2TS080, Decanoic = DR2TS100, Dodecanoic = DR2TS120, Tetradecanoic = DR2TS140,  Hexadecanoic = DR2TS160, Octadecanoic = DR2TS180, Hexadecenoic = DR2TM161, Octadecenoic = DR2TM181, Eicosenoic = DR2TM201, Docosenoic = DR2TM221, Octadecadienoic = DR2TP182, Octadecatrienoic = DR2TP183, Octadecatetraenoic = DR2TP184, Eicosatetraenoic = DR2TP204, Eicosapentaenoic = DR2TP205, Docosapentaenoic = DR2TP225, Docosahexaenoic = DR2TP226, PlainWater = DR2_320Z, TapWater = DR2_330Z)
nutrition_data_0506_day2$wave <- rep("2005-2006",nrow(nutrition_data_0506_day2))

nutrition_data_0708_day1 <- read_xpt("DR1TOT_E.XPT")%>%
  select(SEQN, DR1TKCAL, DR1TPROT, DR1TCARB, DR1TSUGR, DR1TFIBE, DR1TTFAT, DR1TSFAT, DR1TMFAT, DR1TPFAT, DR1TCHOL, DR1TATOC, DR1TRET, DR1TVARA, DR1TACAR, DR1TBCAR, DR1TCRYP, DR1TLYCO, DR1TLZ, DR1TVB1, DR1TVB2, DR1TNIAC, DR1TVB6, DR1TFOLA, DR1TFA, DR1TFF, DR1TFDFE, DR1TVB12, DR1TVC, DR1TVK, DR1TCALC, DR1TPHOS, DR1TMAGN, DR1TIRON, DR1TZINC, DR1TCOPP, DR1TSODI, DR1TPOTA, DR1TSELE, DR1TCAFF, DR1TTHEO, DR1TS040, DR1TS060, DR1TS080, DR1TS100, DR1TS120, DR1TS140, DR1TS160, DR1TS180, DR1TM161, DR1TM181, DR1TM201, DR1TM221, DR1TP182, DR1TP183, DR1TP184, DR1TP204, DR1TP205, DR1TP225, DR1TP226, DR1_320Z, DR1_330Z) %>% 
  transmute(SEQN, Calories = DR1TKCAL, Protein = DR1TPROT, Carbohydrates = DR1TCARB, Sugar = DR1TSUGR, Fiber = DR1TFIBE, TotalFat = DR1TTFAT, SFAT = DR1TSFAT, MFAT = DR1TMFAT, PFAT = DR1TPFAT, Cholesterol = DR1TCHOL, VitE = DR1TATOC, Retinol = DR1TRET, VitA = DR1TVARA, AlphaCaro = DR1TACAR, BetaCaro = DR1TBCAR,  BetaCryp = DR1TCRYP, Lycopene = DR1TLYCO, Lutein = DR1TLZ, Thiamin = DR1TVB1, Riboflavin = DR1TVB2, Niacin = DR1TNIAC, VitB6 = DR1TVB6, TotFolate = DR1TFOLA, FolicAcid = DR1TFA, FoodFolate = DR1TFF, Folate = DR1TFDFE, VitB12 = DR1TVB12, VitC = DR1TVC, VitK = DR1TVK, Calcium = DR1TCALC, Phosphorus = DR1TPHOS, Magnesium = DR1TMAGN, Iron = DR1TIRON, Zinc = DR1TZINC, Copper = DR1TCOPP, Sodium = DR1TSODI, Potassium = DR1TPOTA, Selenium = DR1TSELE, Caffeine = DR1TCAFF, Theobromine = DR1TTHEO, Butanoic = DR1TS040, Hexanoic = DR1TS060, Octanoic = DR1TS080, Decanoic = DR1TS100, Dodecanoic = DR1TS120, Tetradecanoic = DR1TS140,  Hexadecanoic = DR1TS160, Octadecanoic = DR1TS180, Hexadecenoic = DR1TM161, Octadecenoic = DR1TM181, Eicosenoic = DR1TM201, Docosenoic = DR1TM221, Octadecadienoic = DR1TP182, Octadecatrienoic = DR1TP183, Octadecatetraenoic = DR1TP184, Eicosatetraenoic = DR1TP204, Eicosapentaenoic = DR1TP205, Docosapentaenoic = DR1TP225, Docosahexaenoic = DR1TP226, PlainWater = DR1_320Z, TapWater = DR1_330Z)
nutrition_data_0708_day1$wave <- rep("2007-2008",nrow(nutrition_data_0708_day1))

nutrition_data_0708_day2 <- read_xpt("DR2TOT_E.XPT")%>%
  select(SEQN, DR2TKCAL, DR2TPROT, DR2TCARB, DR2TSUGR, DR2TFIBE, DR2TTFAT, DR2TSFAT, DR2TMFAT, DR2TPFAT, DR2TCHOL, DR2TATOC, DR2TRET, DR2TVARA, DR2TACAR, DR2TBCAR, DR2TCRYP, DR2TLYCO, DR2TLZ, DR2TVB1, DR2TVB2, DR2TNIAC, DR2TVB6, DR2TFOLA, DR2TFA, DR2TFF, DR2TFDFE, DR2TVB12, DR2TVC, DR2TVK, DR2TCALC, DR2TPHOS, DR2TMAGN, DR2TIRON, DR2TZINC, DR2TCOPP, DR2TSODI, DR2TPOTA, DR2TSELE, DR2TCAFF, DR2TTHEO, DR2TS040, DR2TS060, DR2TS080, DR2TS100, DR2TS120, DR2TS140, DR2TS160, DR2TS180, DR2TM161, DR2TM181, DR2TM201, DR2TM221, DR2TP182, DR2TP183, DR2TP184, DR2TP204, DR2TP205, DR2TP225, DR2TP226, DR2_320Z, DR2_330Z) %>% 
  transmute(SEQN, Calories = DR2TKCAL, Protein = DR2TPROT, Carbohydrates = DR2TCARB, Sugar = DR2TSUGR, Fiber = DR2TFIBE, TotalFat = DR2TTFAT, SFAT = DR2TSFAT, MFAT = DR2TMFAT, PFAT = DR2TPFAT, Cholesterol = DR2TCHOL, VitE = DR2TATOC, Retinol = DR2TRET, VitA = DR2TVARA, AlphaCaro = DR2TACAR, BetaCaro = DR2TBCAR,  BetaCryp = DR2TCRYP, Lycopene = DR2TLYCO, Lutein = DR2TLZ, Thiamin = DR2TVB1, Riboflavin = DR2TVB2, Niacin = DR2TNIAC, VitB6 = DR2TVB6, TotFolate = DR2TFOLA, FolicAcid = DR2TFA, FoodFolate = DR2TFF, Folate = DR2TFDFE, VitB12 = DR2TVB12, VitC = DR2TVC, VitK = DR2TVK, Calcium = DR2TCALC, Phosphorus = DR2TPHOS, Magnesium = DR2TMAGN, Iron = DR2TIRON, Zinc = DR2TZINC, Copper = DR2TCOPP, Sodium = DR2TSODI, Potassium = DR2TPOTA, Selenium = DR2TSELE, Caffeine = DR2TCAFF, Theobromine = DR2TTHEO, Butanoic = DR2TS040, Hexanoic = DR2TS060, Octanoic = DR2TS080, Decanoic = DR2TS100, Dodecanoic = DR2TS120, Tetradecanoic = DR2TS140,  Hexadecanoic = DR2TS160, Octadecanoic = DR2TS180, Hexadecenoic = DR2TM161, Octadecenoic = DR2TM181, Eicosenoic = DR2TM201, Docosenoic = DR2TM221, Octadecadienoic = DR2TP182, Octadecatrienoic = DR2TP183, Octadecatetraenoic = DR2TP184, Eicosatetraenoic = DR2TP204, Eicosapentaenoic = DR2TP205, Docosapentaenoic = DR2TP225, Docosahexaenoic = DR2TP226, PlainWater = DR2_320Z, TapWater = DR2_330Z)
nutrition_data_0708_day2$wave <- rep("2007-2008",nrow(nutrition_data_0708_day2))

nutrition_data_0910_day1 <- read_xpt("DR1TOT_F.XPT")%>%
  select(SEQN, DR1TKCAL, DR1TPROT, DR1TCARB, DR1TSUGR, DR1TFIBE, DR1TTFAT, DR1TSFAT, DR1TMFAT, DR1TPFAT, DR1TCHOL, DR1TATOC, DR1TRET, DR1TVARA, DR1TACAR, DR1TBCAR, DR1TCRYP, DR1TLYCO, DR1TLZ, DR1TVB1, DR1TVB2, DR1TNIAC, DR1TVB6, DR1TFOLA, DR1TFA, DR1TFF, DR1TFDFE, DR1TVB12, DR1TVC, DR1TVK, DR1TCALC, DR1TPHOS, DR1TMAGN, DR1TIRON, DR1TZINC, DR1TCOPP, DR1TSODI, DR1TPOTA, DR1TSELE, DR1TCAFF, DR1TTHEO, DR1TS040, DR1TS060, DR1TS080, DR1TS100, DR1TS120, DR1TS140, DR1TS160, DR1TS180, DR1TM161, DR1TM181, DR1TM201, DR1TM221, DR1TP182, DR1TP183, DR1TP184, DR1TP204, DR1TP205, DR1TP225, DR1TP226, DR1_320Z, DR1_330Z) %>% 
  transmute(SEQN, Calories = DR1TKCAL, Protein = DR1TPROT, Carbohydrates = DR1TCARB, Sugar = DR1TSUGR, Fiber = DR1TFIBE, TotalFat = DR1TTFAT, SFAT = DR1TSFAT, MFAT = DR1TMFAT, PFAT = DR1TPFAT, Cholesterol = DR1TCHOL, VitE = DR1TATOC, Retinol = DR1TRET, VitA = DR1TVARA, AlphaCaro = DR1TACAR, BetaCaro = DR1TBCAR,  BetaCryp = DR1TCRYP, Lycopene = DR1TLYCO, Lutein = DR1TLZ, Thiamin = DR1TVB1, Riboflavin = DR1TVB2, Niacin = DR1TNIAC, VitB6 = DR1TVB6, TotFolate = DR1TFOLA, FolicAcid = DR1TFA, FoodFolate = DR1TFF, Folate = DR1TFDFE, VitB12 = DR1TVB12, VitC = DR1TVC, VitK = DR1TVK, Calcium = DR1TCALC, Phosphorus = DR1TPHOS, Magnesium = DR1TMAGN, Iron = DR1TIRON, Zinc = DR1TZINC, Copper = DR1TCOPP, Sodium = DR1TSODI, Potassium = DR1TPOTA, Selenium = DR1TSELE, Caffeine = DR1TCAFF, Theobromine = DR1TTHEO, Butanoic = DR1TS040, Hexanoic = DR1TS060, Octanoic = DR1TS080, Decanoic = DR1TS100, Dodecanoic = DR1TS120, Tetradecanoic = DR1TS140,  Hexadecanoic = DR1TS160, Octadecanoic = DR1TS180, Hexadecenoic = DR1TM161, Octadecenoic = DR1TM181, Eicosenoic = DR1TM201, Docosenoic = DR1TM221, Octadecadienoic = DR1TP182, Octadecatrienoic = DR1TP183, Octadecatetraenoic = DR1TP184, Eicosatetraenoic = DR1TP204, Eicosapentaenoic = DR1TP205, Docosapentaenoic = DR1TP225, Docosahexaenoic = DR1TP226, PlainWater = DR1_320Z, TapWater = DR1_330Z)
nutrition_data_0910_day1$wave <- rep("2009-2010",nrow(nutrition_data_0910_day1))

nutrition_data_0910_day2 <- read_xpt("DR2TOT_F.XPT")%>%
  select(SEQN, DR2TKCAL, DR2TPROT, DR2TCARB, DR2TSUGR, DR2TFIBE, DR2TTFAT, DR2TSFAT, DR2TMFAT, DR2TPFAT, DR2TCHOL, DR2TATOC, DR2TRET, DR2TVARA, DR2TACAR, DR2TBCAR, DR2TCRYP, DR2TLYCO, DR2TLZ, DR2TVB1, DR2TVB2, DR2TNIAC, DR2TVB6, DR2TFOLA, DR2TFA, DR2TFF, DR2TFDFE, DR2TVB12, DR2TVC, DR2TVK, DR2TCALC, DR2TPHOS, DR2TMAGN, DR2TIRON, DR2TZINC, DR2TCOPP, DR2TSODI, DR2TPOTA, DR2TSELE, DR2TCAFF, DR2TTHEO, DR2TS040, DR2TS060, DR2TS080, DR2TS100, DR2TS120, DR2TS140, DR2TS160, DR2TS180, DR2TM161, DR2TM181, DR2TM201, DR2TM221, DR2TP182, DR2TP183, DR2TP184, DR2TP204, DR2TP205, DR2TP225, DR2TP226, DR2_320Z, DR2_330Z) %>% 
  transmute(SEQN, Calories = DR2TKCAL, Protein = DR2TPROT, Carbohydrates = DR2TCARB, Sugar = DR2TSUGR, Fiber = DR2TFIBE, TotalFat = DR2TTFAT, SFAT = DR2TSFAT, MFAT = DR2TMFAT, PFAT = DR2TPFAT, Cholesterol = DR2TCHOL, VitE = DR2TATOC, Retinol = DR2TRET, VitA = DR2TVARA, AlphaCaro = DR2TACAR, BetaCaro = DR2TBCAR,  BetaCryp = DR2TCRYP, Lycopene = DR2TLYCO, Lutein = DR2TLZ, Thiamin = DR2TVB1, Riboflavin = DR2TVB2, Niacin = DR2TNIAC, VitB6 = DR2TVB6, TotFolate = DR2TFOLA, FolicAcid = DR2TFA, FoodFolate = DR2TFF, Folate = DR2TFDFE, VitB12 = DR2TVB12, VitC = DR2TVC, VitK = DR2TVK, Calcium = DR2TCALC, Phosphorus = DR2TPHOS, Magnesium = DR2TMAGN, Iron = DR2TIRON, Zinc = DR2TZINC, Copper = DR2TCOPP, Sodium = DR2TSODI, Potassium = DR2TPOTA, Selenium = DR2TSELE, Caffeine = DR2TCAFF, Theobromine = DR2TTHEO, Butanoic = DR2TS040, Hexanoic = DR2TS060, Octanoic = DR2TS080, Decanoic = DR2TS100, Dodecanoic = DR2TS120, Tetradecanoic = DR2TS140,  Hexadecanoic = DR2TS160, Octadecanoic = DR2TS180, Hexadecenoic = DR2TM161, Octadecenoic = DR2TM181, Eicosenoic = DR2TM201, Docosenoic = DR2TM221, Octadecadienoic = DR2TP182, Octadecatrienoic = DR2TP183, Octadecatetraenoic = DR2TP184, Eicosatetraenoic = DR2TP204, Eicosapentaenoic = DR2TP205, Docosapentaenoic = DR2TP225, Docosahexaenoic = DR2TP226, PlainWater = DR2_320Z, TapWater = DR2_330Z)
nutrition_data_0910_day2$wave <- rep("2009-2010",nrow(nutrition_data_0910_day2))

nutrition_data_1112_day1 <- read_xpt("DR1TOT_G.XPT")%>%
  select(SEQN, DR1TKCAL, DR1TPROT, DR1TCARB, DR1TSUGR, DR1TFIBE, DR1TTFAT, DR1TSFAT, DR1TMFAT, DR1TPFAT, DR1TCHOL, DR1TATOC, DR1TRET, DR1TVARA, DR1TACAR, DR1TBCAR, DR1TCRYP, DR1TLYCO, DR1TLZ, DR1TVB1, DR1TVB2, DR1TNIAC, DR1TVB6, DR1TFOLA, DR1TFA, DR1TFF, DR1TFDFE, DR1TVB12, DR1TVC, DR1TVK, DR1TCALC, DR1TPHOS, DR1TMAGN, DR1TIRON, DR1TZINC, DR1TCOPP, DR1TSODI, DR1TPOTA, DR1TSELE, DR1TCAFF, DR1TTHEO, DR1TS040, DR1TS060, DR1TS080, DR1TS100, DR1TS120, DR1TS140, DR1TS160, DR1TS180, DR1TM161, DR1TM181, DR1TM201, DR1TM221, DR1TP182, DR1TP183, DR1TP184, DR1TP204, DR1TP205, DR1TP225, DR1TP226, DR1_320Z, DR1_330Z) %>% 
  transmute(SEQN, Calories = DR1TKCAL, Protein = DR1TPROT, Carbohydrates = DR1TCARB, Sugar = DR1TSUGR, Fiber = DR1TFIBE, TotalFat = DR1TTFAT, SFAT = DR1TSFAT, MFAT = DR1TMFAT, PFAT = DR1TPFAT, Cholesterol = DR1TCHOL, VitE = DR1TATOC, Retinol = DR1TRET, VitA = DR1TVARA, AlphaCaro = DR1TACAR, BetaCaro = DR1TBCAR,  BetaCryp = DR1TCRYP, Lycopene = DR1TLYCO, Lutein = DR1TLZ, Thiamin = DR1TVB1, Riboflavin = DR1TVB2, Niacin = DR1TNIAC, VitB6 = DR1TVB6, TotFolate = DR1TFOLA, FolicAcid = DR1TFA, FoodFolate = DR1TFF, Folate = DR1TFDFE, VitB12 = DR1TVB12, VitC = DR1TVC, VitK = DR1TVK, Calcium = DR1TCALC, Phosphorus = DR1TPHOS, Magnesium = DR1TMAGN, Iron = DR1TIRON, Zinc = DR1TZINC, Copper = DR1TCOPP, Sodium = DR1TSODI, Potassium = DR1TPOTA, Selenium = DR1TSELE, Caffeine = DR1TCAFF, Theobromine = DR1TTHEO, Butanoic = DR1TS040, Hexanoic = DR1TS060, Octanoic = DR1TS080, Decanoic = DR1TS100, Dodecanoic = DR1TS120, Tetradecanoic = DR1TS140,  Hexadecanoic = DR1TS160, Octadecanoic = DR1TS180, Hexadecenoic = DR1TM161, Octadecenoic = DR1TM181, Eicosenoic = DR1TM201, Docosenoic = DR1TM221, Octadecadienoic = DR1TP182, Octadecatrienoic = DR1TP183, Octadecatetraenoic = DR1TP184, Eicosatetraenoic = DR1TP204, Eicosapentaenoic = DR1TP205, Docosapentaenoic = DR1TP225, Docosahexaenoic = DR1TP226, PlainWater = DR1_320Z, TapWater = DR1_330Z)
nutrition_data_1112_day1$wave <- rep("2011-2012",nrow(nutrition_data_1112_day1))

nutrition_data_1112_day2 <- read_xpt("DR2TOT_G.XPT")%>%
  select(SEQN, DR2TKCAL, DR2TPROT, DR2TCARB, DR2TSUGR, DR2TFIBE, DR2TTFAT, DR2TSFAT, DR2TMFAT, DR2TPFAT, DR2TCHOL, DR2TATOC, DR2TRET, DR2TVARA, DR2TACAR, DR2TBCAR, DR2TCRYP, DR2TLYCO, DR2TLZ, DR2TVB1, DR2TVB2, DR2TNIAC, DR2TVB6, DR2TFOLA, DR2TFA, DR2TFF, DR2TFDFE, DR2TVB12, DR2TVC, DR2TVK, DR2TCALC, DR2TPHOS, DR2TMAGN, DR2TIRON, DR2TZINC, DR2TCOPP, DR2TSODI, DR2TPOTA, DR2TSELE, DR2TCAFF, DR2TTHEO, DR2TS040, DR2TS060, DR2TS080, DR2TS100, DR2TS120, DR2TS140, DR2TS160, DR2TS180, DR2TM161, DR2TM181, DR2TM201, DR2TM221, DR2TP182, DR2TP183, DR2TP184, DR2TP204, DR2TP205, DR2TP225, DR2TP226, DR2_320Z, DR2_330Z) %>% 
  transmute(SEQN, Calories = DR2TKCAL, Protein = DR2TPROT, Carbohydrates = DR2TCARB, Sugar = DR2TSUGR, Fiber = DR2TFIBE, TotalFat = DR2TTFAT, SFAT = DR2TSFAT, MFAT = DR2TMFAT, PFAT = DR2TPFAT, Cholesterol = DR2TCHOL, VitE = DR2TATOC, Retinol = DR2TRET, VitA = DR2TVARA, AlphaCaro = DR2TACAR, BetaCaro = DR2TBCAR,  BetaCryp = DR2TCRYP, Lycopene = DR2TLYCO, Lutein = DR2TLZ, Thiamin = DR2TVB1, Riboflavin = DR2TVB2, Niacin = DR2TNIAC, VitB6 = DR2TVB6, TotFolate = DR2TFOLA, FolicAcid = DR2TFA, FoodFolate = DR2TFF, Folate = DR2TFDFE, VitB12 = DR2TVB12, VitC = DR2TVC, VitK = DR2TVK, Calcium = DR2TCALC, Phosphorus = DR2TPHOS, Magnesium = DR2TMAGN, Iron = DR2TIRON, Zinc = DR2TZINC, Copper = DR2TCOPP, Sodium = DR2TSODI, Potassium = DR2TPOTA, Selenium = DR2TSELE, Caffeine = DR2TCAFF, Theobromine = DR2TTHEO, Butanoic = DR2TS040, Hexanoic = DR2TS060, Octanoic = DR2TS080, Decanoic = DR2TS100, Dodecanoic = DR2TS120, Tetradecanoic = DR2TS140,  Hexadecanoic = DR2TS160, Octadecanoic = DR2TS180, Hexadecenoic = DR2TM161, Octadecenoic = DR2TM181, Eicosenoic = DR2TM201, Docosenoic = DR2TM221, Octadecadienoic = DR2TP182, Octadecatrienoic = DR2TP183, Octadecatetraenoic = DR2TP184, Eicosatetraenoic = DR2TP204, Eicosapentaenoic = DR2TP205, Docosapentaenoic = DR2TP225, Docosahexaenoic = DR2TP226, PlainWater = DR2_320Z, TapWater = DR2_330Z)
nutrition_data_1112_day2$wave <- rep("2011-2012",nrow(nutrition_data_1112_day2))

nutrition_data_1314_day1 <- read_xpt("DR1TOT_H.XPT")%>%
  select(SEQN, DR1TKCAL, DR1TPROT, DR1TCARB, DR1TSUGR, DR1TFIBE, DR1TTFAT, DR1TSFAT, DR1TMFAT, DR1TPFAT, DR1TCHOL, DR1TATOC, DR1TRET, DR1TVARA, DR1TACAR, DR1TBCAR, DR1TCRYP, DR1TLYCO, DR1TLZ, DR1TVB1, DR1TVB2, DR1TNIAC, DR1TVB6, DR1TFOLA, DR1TFA, DR1TFF, DR1TFDFE, DR1TVB12, DR1TVC, DR1TVK, DR1TCALC, DR1TPHOS, DR1TMAGN, DR1TIRON, DR1TZINC, DR1TCOPP, DR1TSODI, DR1TPOTA, DR1TSELE, DR1TCAFF, DR1TTHEO, DR1TS040, DR1TS060, DR1TS080, DR1TS100, DR1TS120, DR1TS140, DR1TS160, DR1TS180, DR1TM161, DR1TM181, DR1TM201, DR1TM221, DR1TP182, DR1TP183, DR1TP184, DR1TP204, DR1TP205, DR1TP225, DR1TP226, DR1_320Z, DR1_330Z) %>% 
  transmute(SEQN, Calories = DR1TKCAL, Protein = DR1TPROT, Carbohydrates = DR1TCARB, Sugar = DR1TSUGR, Fiber = DR1TFIBE, TotalFat = DR1TTFAT, SFAT = DR1TSFAT, MFAT = DR1TMFAT, PFAT = DR1TPFAT, Cholesterol = DR1TCHOL, VitE = DR1TATOC, Retinol = DR1TRET, VitA = DR1TVARA, AlphaCaro = DR1TACAR, BetaCaro = DR1TBCAR,  BetaCryp = DR1TCRYP, Lycopene = DR1TLYCO, Lutein = DR1TLZ, Thiamin = DR1TVB1, Riboflavin = DR1TVB2, Niacin = DR1TNIAC, VitB6 = DR1TVB6, TotFolate = DR1TFOLA, FolicAcid = DR1TFA, FoodFolate = DR1TFF, Folate = DR1TFDFE, VitB12 = DR1TVB12, VitC = DR1TVC, VitK = DR1TVK, Calcium = DR1TCALC, Phosphorus = DR1TPHOS, Magnesium = DR1TMAGN, Iron = DR1TIRON, Zinc = DR1TZINC, Copper = DR1TCOPP, Sodium = DR1TSODI, Potassium = DR1TPOTA, Selenium = DR1TSELE, Caffeine = DR1TCAFF, Theobromine = DR1TTHEO, Butanoic = DR1TS040, Hexanoic = DR1TS060, Octanoic = DR1TS080, Decanoic = DR1TS100, Dodecanoic = DR1TS120, Tetradecanoic = DR1TS140,  Hexadecanoic = DR1TS160, Octadecanoic = DR1TS180, Hexadecenoic = DR1TM161, Octadecenoic = DR1TM181, Eicosenoic = DR1TM201, Docosenoic = DR1TM221, Octadecadienoic = DR1TP182, Octadecatrienoic = DR1TP183, Octadecatetraenoic = DR1TP184, Eicosatetraenoic = DR1TP204, Eicosapentaenoic = DR1TP205, Docosapentaenoic = DR1TP225, Docosahexaenoic = DR1TP226, PlainWater = DR1_320Z, TapWater = DR1_330Z)
nutrition_data_1314_day1$wave <- rep("2013-2014",nrow(nutrition_data_1314_day1))

nutrition_data_1314_day2 <- read_xpt("DR2TOT_H.XPT")%>%
  select(SEQN, DR2TKCAL, DR2TPROT, DR2TCARB, DR2TSUGR, DR2TFIBE, DR2TTFAT, DR2TSFAT, DR2TMFAT, DR2TPFAT, DR2TCHOL, DR2TATOC, DR2TRET, DR2TVARA, DR2TACAR, DR2TBCAR, DR2TCRYP, DR2TLYCO, DR2TLZ, DR2TVB1, DR2TVB2, DR2TNIAC, DR2TVB6, DR2TFOLA, DR2TFA, DR2TFF, DR2TFDFE, DR2TVB12, DR2TVC, DR2TVK, DR2TCALC, DR2TPHOS, DR2TMAGN, DR2TIRON, DR2TZINC, DR2TCOPP, DR2TSODI, DR2TPOTA, DR2TSELE, DR2TCAFF, DR2TTHEO, DR2TS040, DR2TS060, DR2TS080, DR2TS100, DR2TS120, DR2TS140, DR2TS160, DR2TS180, DR2TM161, DR2TM181, DR2TM201, DR2TM221, DR2TP182, DR2TP183, DR2TP184, DR2TP204, DR2TP205, DR2TP225, DR2TP226, DR2_320Z, DR2_330Z) %>% 
  transmute(SEQN, Calories = DR2TKCAL, Protein = DR2TPROT, Carbohydrates = DR2TCARB, Sugar = DR2TSUGR, Fiber = DR2TFIBE, TotalFat = DR2TTFAT, SFAT = DR2TSFAT, MFAT = DR2TMFAT, PFAT = DR2TPFAT, Cholesterol = DR2TCHOL, VitE = DR2TATOC, Retinol = DR2TRET, VitA = DR2TVARA, AlphaCaro = DR2TACAR, BetaCaro = DR2TBCAR,  BetaCryp = DR2TCRYP, Lycopene = DR2TLYCO, Lutein = DR2TLZ, Thiamin = DR2TVB1, Riboflavin = DR2TVB2, Niacin = DR2TNIAC, VitB6 = DR2TVB6, TotFolate = DR2TFOLA, FolicAcid = DR2TFA, FoodFolate = DR2TFF, Folate = DR2TFDFE, VitB12 = DR2TVB12, VitC = DR2TVC, VitK = DR2TVK, Calcium = DR2TCALC, Phosphorus = DR2TPHOS, Magnesium = DR2TMAGN, Iron = DR2TIRON, Zinc = DR2TZINC, Copper = DR2TCOPP, Sodium = DR2TSODI, Potassium = DR2TPOTA, Selenium = DR2TSELE, Caffeine = DR2TCAFF, Theobromine = DR2TTHEO, Butanoic = DR2TS040, Hexanoic = DR2TS060, Octanoic = DR2TS080, Decanoic = DR2TS100, Dodecanoic = DR2TS120, Tetradecanoic = DR2TS140,  Hexadecanoic = DR2TS160, Octadecanoic = DR2TS180, Hexadecenoic = DR2TM161, Octadecenoic = DR2TM181, Eicosenoic = DR2TM201, Docosenoic = DR2TM221, Octadecadienoic = DR2TP182, Octadecatrienoic = DR2TP183, Octadecatetraenoic = DR2TP184, Eicosatetraenoic = DR2TP204, Eicosapentaenoic = DR2TP205, Docosapentaenoic = DR2TP225, Docosahexaenoic = DR2TP226, PlainWater = DR2_320Z, TapWater = DR2_330Z)
nutrition_data_1314_day2$wave <- rep("2013-2014",nrow(nutrition_data_1314_day2))

nutrition_data_1516_day1 <- read_xpt("DR1TOT_I.XPT")%>%
  select(SEQN, DR1TKCAL, DR1TPROT, DR1TCARB, DR1TSUGR, DR1TFIBE, DR1TTFAT, DR1TSFAT, DR1TMFAT, DR1TPFAT, DR1TCHOL, DR1TATOC, DR1TRET, DR1TVARA, DR1TACAR, DR1TBCAR, DR1TCRYP, DR1TLYCO, DR1TLZ, DR1TVB1, DR1TVB2, DR1TNIAC, DR1TVB6, DR1TFOLA, DR1TFA, DR1TFF, DR1TFDFE, DR1TVB12, DR1TVC, DR1TVK, DR1TCALC, DR1TPHOS, DR1TMAGN, DR1TIRON, DR1TZINC, DR1TCOPP, DR1TSODI, DR1TPOTA, DR1TSELE, DR1TCAFF, DR1TTHEO, DR1TS040, DR1TS060, DR1TS080, DR1TS100, DR1TS120, DR1TS140, DR1TS160, DR1TS180, DR1TM161, DR1TM181, DR1TM201, DR1TM221, DR1TP182, DR1TP183, DR1TP184, DR1TP204, DR1TP205, DR1TP225, DR1TP226, DR1_320Z, DR1_330Z) %>% 
  transmute(SEQN, Calories = DR1TKCAL, Protein = DR1TPROT, Carbohydrates = DR1TCARB, Sugar = DR1TSUGR, Fiber = DR1TFIBE, TotalFat = DR1TTFAT, SFAT = DR1TSFAT, MFAT = DR1TMFAT, PFAT = DR1TPFAT, Cholesterol = DR1TCHOL, VitE = DR1TATOC, Retinol = DR1TRET, VitA = DR1TVARA, AlphaCaro = DR1TACAR, BetaCaro = DR1TBCAR,  BetaCryp = DR1TCRYP, Lycopene = DR1TLYCO, Lutein = DR1TLZ, Thiamin = DR1TVB1, Riboflavin = DR1TVB2, Niacin = DR1TNIAC, VitB6 = DR1TVB6, TotFolate = DR1TFOLA, FolicAcid = DR1TFA, FoodFolate = DR1TFF, Folate = DR1TFDFE, VitB12 = DR1TVB12, VitC = DR1TVC, VitK = DR1TVK, Calcium = DR1TCALC, Phosphorus = DR1TPHOS, Magnesium = DR1TMAGN, Iron = DR1TIRON, Zinc = DR1TZINC, Copper = DR1TCOPP, Sodium = DR1TSODI, Potassium = DR1TPOTA, Selenium = DR1TSELE, Caffeine = DR1TCAFF, Theobromine = DR1TTHEO, Butanoic = DR1TS040, Hexanoic = DR1TS060, Octanoic = DR1TS080, Decanoic = DR1TS100, Dodecanoic = DR1TS120, Tetradecanoic = DR1TS140,  Hexadecanoic = DR1TS160, Octadecanoic = DR1TS180, Hexadecenoic = DR1TM161, Octadecenoic = DR1TM181, Eicosenoic = DR1TM201, Docosenoic = DR1TM221, Octadecadienoic = DR1TP182, Octadecatrienoic = DR1TP183, Octadecatetraenoic = DR1TP184, Eicosatetraenoic = DR1TP204, Eicosapentaenoic = DR1TP205, Docosapentaenoic = DR1TP225, Docosahexaenoic = DR1TP226, PlainWater = DR1_320Z, TapWater = DR1_330Z)
nutrition_data_1516_day1$wave <- rep("2015-2016",nrow(nutrition_data_1516_day1))

nutrition_data_1516_day2 <- read_xpt("DR2TOT_I.XPT")%>%
  select(SEQN, DR2TKCAL, DR2TPROT, DR2TCARB, DR2TSUGR, DR2TFIBE, DR2TTFAT, DR2TSFAT, DR2TMFAT, DR2TPFAT, DR2TCHOL, DR2TATOC, DR2TRET, DR2TVARA, DR2TACAR, DR2TBCAR, DR2TCRYP, DR2TLYCO, DR2TLZ, DR2TVB1, DR2TVB2, DR2TNIAC, DR2TVB6, DR2TFOLA, DR2TFA, DR2TFF, DR2TFDFE, DR2TVB12, DR2TVC, DR2TVK, DR2TCALC, DR2TPHOS, DR2TMAGN, DR2TIRON, DR2TZINC, DR2TCOPP, DR2TSODI, DR2TPOTA, DR2TSELE, DR2TCAFF, DR2TTHEO, DR2TS040, DR2TS060, DR2TS080, DR2TS100, DR2TS120, DR2TS140, DR2TS160, DR2TS180, DR2TM161, DR2TM181, DR2TM201, DR2TM221, DR2TP182, DR2TP183, DR2TP184, DR2TP204, DR2TP205, DR2TP225, DR2TP226, DR2_320Z, DR2_330Z) %>% 
  transmute(SEQN, Calories = DR2TKCAL, Protein = DR2TPROT, Carbohydrates = DR2TCARB, Sugar = DR2TSUGR, Fiber = DR2TFIBE, TotalFat = DR2TTFAT, SFAT = DR2TSFAT, MFAT = DR2TMFAT, PFAT = DR2TPFAT, Cholesterol = DR2TCHOL, VitE = DR2TATOC, Retinol = DR2TRET, VitA = DR2TVARA, AlphaCaro = DR2TACAR, BetaCaro = DR2TBCAR,  BetaCryp = DR2TCRYP, Lycopene = DR2TLYCO, Lutein = DR2TLZ, Thiamin = DR2TVB1, Riboflavin = DR2TVB2, Niacin = DR2TNIAC, VitB6 = DR2TVB6, TotFolate = DR2TFOLA, FolicAcid = DR2TFA, FoodFolate = DR2TFF, Folate = DR2TFDFE, VitB12 = DR2TVB12, VitC = DR2TVC, VitK = DR2TVK, Calcium = DR2TCALC, Phosphorus = DR2TPHOS, Magnesium = DR2TMAGN, Iron = DR2TIRON, Zinc = DR2TZINC, Copper = DR2TCOPP, Sodium = DR2TSODI, Potassium = DR2TPOTA, Selenium = DR2TSELE, Caffeine = DR2TCAFF, Theobromine = DR2TTHEO, Butanoic = DR2TS040, Hexanoic = DR2TS060, Octanoic = DR2TS080, Decanoic = DR2TS100, Dodecanoic = DR2TS120, Tetradecanoic = DR2TS140,  Hexadecanoic = DR2TS160, Octadecanoic = DR2TS180, Hexadecenoic = DR2TM161, Octadecenoic = DR2TM181, Eicosenoic = DR2TM201, Docosenoic = DR2TM221, Octadecadienoic = DR2TP182, Octadecatrienoic = DR2TP183, Octadecatetraenoic = DR2TP184, Eicosatetraenoic = DR2TP204, Eicosapentaenoic = DR2TP205, Docosapentaenoic = DR2TP225, Docosahexaenoic = DR2TP226, PlainWater = DR2_320Z, TapWater = DR2_330Z)
nutrition_data_1516_day2$wave <- rep("2015-2016",nrow(nutrition_data_1516_day2))

total_nutrition_data <- rbind(nutrition_data_0102, 
                              nutrition_data_0304_day1, 
                              nutrition_data_0304_day2, 
                              nutrition_data_0506_day1, 
                              nutrition_data_0506_day2, 
                              nutrition_data_0708_day1, 
                              nutrition_data_0708_day2, 
                              nutrition_data_0910_day1, 
                              nutrition_data_0910_day2, 
                              nutrition_data_1112_day1, 
                              nutrition_data_1112_day2, 
                              nutrition_data_1314_day1, 
                              nutrition_data_1314_day2, 
                              nutrition_data_1516_day1, 
                              nutrition_data_1516_day2)

full_nutrition_data <- total_nutrition_data %>%
  left_join(dat, by = "SEQN") %>%
  select(SEQN, wave.x, Calories, Protein, Carbohydrates, Sugar, Fiber, TotalFat, SFAT, MFAT, PFAT, Cholesterol, VitE, Retinol, VitA, AlphaCaro, BetaCaro,  BetaCryp, Lycopene, Lutein, Thiamin, Riboflavin, Niacin, VitB6, TotFolate, FolicAcid, FoodFolate, Folate, VitB12, VitC, VitK, Calcium, Phosphorus, Magnesium, Iron, Zinc, Copper, Sodium, Potassium, Selenium, Caffeine, Theobromine, Butanoic, Hexanoic, Octanoic, Decanoic, Dodecanoic, Tetradecanoic,  Hexadecanoic, Octadecanoic, Hexadecenoic, Octadecenoic, Eicosenoic, Docosenoic, Octadecadienoic, Octadecatrienoic, Octadecatetraenoic, Eicosatetraenoic, Eicosapentaenoic, Docosapentaenoic, Docosahexaenoic, PlainWater, TapWater, RIAGENDR, RIDAGEYR, RIDRETH1,  RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010, SMQ040, SMD070, food_security, gender, race, diabetes, tryweloss, cursmoke, bmi_cat, age_cat, poverty_cat) %>%
  transmute(SEQN, wave = wave.x, Calories, Protein, Carbohydrates, Sugar, Fiber, TotalFat, SFAT, MFAT, PFAT, Cholesterol, VitE, Retinol, VitA, AlphaCaro, BetaCaro,  BetaCryp, Lycopene, Lutein, Thiamin, Riboflavin, Niacin, VitB6, TotFolate, FolicAcid, FoodFolate, Folate, VitB12, VitC, VitK, Calcium, Phosphorus, Magnesium, Iron, Zinc, Copper, Sodium, Potassium, Selenium, Caffeine, Theobromine, Butanoic, Hexanoic, Octanoic, Decanoic, Dodecanoic, Tetradecanoic,  Hexadecanoic, Octadecanoic, Hexadecenoic, Octadecenoic, Eicosenoic, Docosenoic, Octadecadienoic, Octadecatrienoic, Octadecatetraenoic, Eicosatetraenoic, Eicosapentaenoic, Docosapentaenoic, Docosahexaenoic, PlainWater, TapWater, RIAGENDR, RIDAGEYR, RIDRETH1,  RIDEXPRG, INDFMINC, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI, WHQ070, HHFDSEC, ADFDSEC, DIQ010, SMQ040, SMD070, food_security, gender, race, diabetes, tryweloss, cursmoke, bmi_cat, age_cat, poverty_cat)

full_food_data$general_categories <- as.factor(if_else(full_food_data$category_number %in% c(1002, 1004, 1006, 1008), 'Milk', if_else(full_food_data$category_number %in% c(1202, 1204, 1206, 1208), 'Flavored Milk', if_else(full_food_data$category_number %in% c(1402, 1404), 'Dairy Drinks and Substitutes', if_else(full_food_data$category_number %in% c(1602, 1604), 'Cheese', if_else(full_food_data$category_number %in% c(1820, 1822), 'Yogurt', if_else(full_food_data$category_number %in% c(2002, 2004, 2006, 2008, 2010), 'Meats', if_else(full_food_data$category_number %in% c(2202, 2204, 2206), 'Poultry', if_else(full_food_data$category_number %in% c(2402, 2404), 'Shellfish', if_else(full_food_data$category_number %in% c(2502), 'Eggs', if_else(full_food_data$category_number %in% c(2602, 2604, 2606, 2608), 'Cured Meats/Poultry', if_else(full_food_data$category_number %in% c(2802, 2804, 2806), 'Plant-based Protein Foods', if_else(full_food_data$category_number %in% c(3002, 3004, 3006), 'Mixed Dishes - Meat, Poultry, Seafood', if_else(full_food_data$category_number %in% c(3202, 3204, 3206, 3208), 'Mixed Dishes - Grain-based', if_else(full_food_data$category_number %in% c(3402, 3404, 3406), 'Mixed Dishes - Asian', if_else(full_food_data$category_number %in% c(3502, 3504, 3506), 'Mixed Dishes - Mexican', if_else(full_food_data$category_number %in% c(3602), 'Mixed Dishes - Pizza', if_else(full_food_data$category_number %in% c(3702, 3703, 3704, 3706, 3708, 3720, 3722), 'Mixed Dishes - Sandwiches', if_else(full_food_data$category_number %in% c(3802), 'Mixed Dishes - Soups', if_else(full_food_data$category_number %in% c(4002, 4004), 'Cooked Grains', if_else(full_food_data$category_number %in% c(4202, 4204, 4206, 4208), 'Breads, Rolls, Tortillas', if_else(full_food_data$category_number %in% c(4402, 4404), 'Quick Breads and Bread Products', if_else(full_food_data$category_number %in% c(4602, 4604), 'Ready-to-Eat Cereals', if_else(full_food_data$category_number %in% c(4802, 4804), 'Cooked Cereals', if_else(full_food_data$category_number %in% c(5002, 5004, 5006, 5008), 'Savory Snacks', if_else(full_food_data$category_number %in% c(5202, 5204), 'Crackers', if_else(full_food_data$category_number %in% c(5402, 5404), 'Snack/Meal Bars', if_else(full_food_data$category_number %in% c(5502, 5504, 5506), 'Sweet Bakery Product', if_else(full_food_data$category_number %in% c(5702, 5704), 'Candy', if_else(full_food_data$category_number %in% c(5802, 5804, 5806), 'Other Desserts', if_else(full_food_data$category_number %in% c(6002, 6004, 6006, 6008, 6010, 6012, 6014, 6016, 6018), 'Fruits', if_else(full_food_data$category_number %in% c(6402, 6404, 6406, 6408, 6410, 6412, 6414, 6416, 6418, 6420, 6422), 'Vegetables, excluding Potatoes', if_else(full_food_data$category_number %in% c(6802, 6804, 6806), 'White Potatoes', if_else(full_food_data$category_number %in% c(7002, 7004, 7006, 7008), '100% Juice', if_else(full_food_data$category_number %in% c(7102, 7104, 7106), 'Diet Beverages', if_else(full_food_data$category_number %in% c(7202, 7204, 7206, 7208, 7220), 'Sweetened Beverage', if_else(full_food_data$category_number %in% c(7302, 7304), 'Coffee and Tea', if_else(full_food_data$category_number %in% c(7502, 7504, 7506), 'Alcoholic Beverages', if_else(full_food_data$category_number %in% c(7702, 7704), 'Plain Water', if_else(full_food_data$category_number %in% c(7802, 7804), 'Flavored or Enhanced Water', if_else(full_food_data$category_number %in% c(8002, 8004, 8006, 8008, 8010, 8012), 'Fats and Oils', if_else(full_food_data$category_number %in% c(8402, 8404, 8406, 8408, 8410, 8412), 'Condiments and Sauces', if_else(full_food_data$category_number %in% c(8802, 8804, 8806), 'Sugars', if_else(full_food_data$category_number %in% c(9002, 9004, 9006, 9008, 9010, 9012), 'Baby Foods', if_else(full_food_data$category_number %in% c(9202, 9204), 'Baby Beverages', if_else(full_food_data$category_number %in% c(9402, 9404, 9406), 'Infant Formulas', if_else(full_food_data$category_number %in% c(9602), 'Human Milk', 'Other')))))))))))))))))))))))))))))))))))))))))))))))

nutrition_variables <- c('Energy (kcal)', 'Protein (gm)', 'Carbohydrate (gm)', 'Total sugars (gm)', 'Dietary fiber (gm)', 'Total fat (gm)', 'Total saturated fatty acids (gm)', 'Total monounsaturated fatty acids (gm)', 'Total polyunsaturated fatty acids (gm)', 'Cholesterol (mg)', 'Vitamin E as alpha-tocopherol (mg)', 'Retinol (mcg)', 'Vitamin A, RAE (mcg)', 'Alpha-carotene (mcg)', 'Beta-carotene (mcg)', 'Beta-cryptoxanthin (mcg)', 'Lycopene (mcg)', 'Lutein + zeaxanthin (mcg)', 'Thiamin (Vitamin B1) (mg)', 'Riboflavin (Vitamin B2) (mg)', 'Niacin (mg)', 'Vitamin B6 (mg)', 'Total folate (mcg)', 'Folic acid (mcg)', 'Food folate (mcg)', 'Folate, DFE (mcg)','Vitamin B12 (mcg)', 'Vitamin C (mg)', 'Vitamin K (mcg)', 'Calcium (mg)', 'Phosphorus (mg)', 'Magnesium (mg)', 'Iron (mg)', 'Zinc (mg)', 'Copper (mg)', 'Sodium (mg)', 'Potassium (mg)', 'Selenium (mcg)', 'Caffeine (mg)', 'Theobromine (mg)', 'SFA 4:0 (Butanoic) (gm)', 'SFA 6:0 (Hexanoic) (gm)', 'SFA 8:0 (Octanoic) (gm)', 'SFA 10:0 (Decanoic) (gm)', 'SFA 12:0 (Dodecanoic) (gm)', 'SFA 14:0 (Tetradecanoic) (gm)', 'SFA 16:0 (Hexadecanoic) (gm)', 'SFA 18:0 (Octadecanoic) (gm)', 'MFA 16:1 (Hexadecenoic) (gm)', 'MFA 18:1 (Octadecenoic) (gm)', 'MFA 20:1 (Eicosenoic) (gm)', 'MFA 22:1 (Docosenoic) (gm)', 'PFA 18:2 (Octadecadienoic) (gm)', 'PFA 18:3 (Octadecatrienoic) (gm)', 'PFA 18:4 (Octadecatetraenoic) (gm)', 'PFA 20:4 (Eicosatetraenoic) (gm)', 'PFA 20:5 (Eicosapentaenoic) (gm)', 'PFA 22:5 (Docosapentaenoic) (gm)', 'PFA 22:6 (Docosahexaenoic) (gm)', 'Plain water (gm)', 'Tap water (gm)')

# Define UI
ui <- fluidPage(
  theme = shinythemes::shinytheme("darkly"),
  titlePanel("What Do We Eat? Analysis of NHANES and WWEIA Data"),
  
  tabsetPanel(
    tabPanel("Demographic Trends",
             sidebarLayout(sidebarPanel(
               p(
                 "The data used was created using a number of datasets
                   available from the",
                 a("NHANES Database", href = "https://www.cdc.gov/nchs/nhanes/index.htm")
               ),
               br(),
               selectInput(
                 inputId = "Demographic",
                 label = "Select a demographic variable",
                 choices = c(
                   'Age',
                   'BMI',
                   'Diabetes',
                   'Food Security',
                   'Gender',
                   'Poverty Ratio',
                   'Race',
                   'Smoking Status',
                   'Weight Loss'
                 )
               )
             ),
             mainPanel(plotOutput(outputId = "line1")))),
    
    tabPanel("Food Trends",
             sidebarLayout(
               sidebarPanel(
                 p(
                   "The data used was created using a number of datasets
                   available from the",
                   a("NHANES Database", href = "https://www.cdc.gov/nchs/nhanes/index.htm")
                 ),
                 br(),
                 selectInput(
                   inputId = "Food",
                   label = "Select a food variable",
                   choices = as.list(levels(full_food_data$general_categories))
                 ),
                 selectInput(
                   inputId = "Demographic2",
                   label = "Select a demographic variable",
                   choices = c(
                     'None',
                     'Age',
                     'BMI',
                     'Diabetes',
                     'Food Security',
                     'Gender',
                     'Poverty Ratio',
                     'Race',
                     'Smoking Status',
                     'Weight Loss'
                   )
                 )
               ),
               mainPanel(plotOutput(outputId = "line2"))
             )), 
    
    
    tabPanel("Food - Demographic Relationships",
             sidebarLayout(sidebarPanel(
               p(
                 "The data used was created using a number of datasets
                   available from the",
                 a("NHANES Database", href = "https://www.cdc.gov/nchs/nhanes/index.htm")
               ),
               br(),
               selectInput(
                 inputId = "Food2",
                 label = "Select a food variable",
                 choices = as.list(levels(full_food_data$general_categories))
               ),
               selectInput(
                 inputId = "Var",
                 label = "Select a demographic variable",
                 choices = c(
                   'Age (Years)',
                   'BMI (kg/m^2)',
                   'Poverty Ratio'
                 )
               )
             ),
             mainPanel(plotOutput(outputId = "smooth1")))),
    
    tabPanel("Nutrition Trends",
             sidebarLayout(
               sidebarPanel(
                 p(
                   "The data used was created using a number of datasets
                   available from the",
                   a("NHANES Database", href = "https://www.cdc.gov/nchs/nhanes/index.htm")
                 ),
                 br(),
                 selectInput(
                   inputId = "Nutrition",
                   label = "Select a nutrition variable",
                   choices = nutrition_variables
                 ),
                 selectInput(
                   inputId = "Demographic3",
                   label = "Select a demographic variable",
                   choices = c(
                     'None',
                     'Age',
                     'BMI',
                     'Diabetes',
                     'Food Security',
                     'Gender',
                     'Poverty Ratio',
                     'Race',
                     'Smoking Status',
                     'Weight Loss'
                   )
                 )
               ),
               mainPanel(plotOutput(outputId = "line3"))
             )), 
    
    
    tabPanel("Nutrition - Demographic Relationships",
             sidebarLayout(sidebarPanel(
               p(
                 "The data used was created using a number of datasets
                   available from the",
                 a("NHANES Database", href = "https://www.cdc.gov/nchs/nhanes/index.htm")
               ),
               br(),
               selectInput(
                 inputId = "Nutrition2",
                 label = "Select a nutrition variable",
                 choices = nutrition_variables
               ),
               
               selectInput(
                 inputId = "Var2",
                 label = "Select a demographic variable",
                 choices = c(
                   'Age (Years)',
                   'BMI (kg/m^2)',
                   'Poverty Ratio'
                 )
               )
             ),
             mainPanel(plotOutput(outputId = "smooth2")))),
    
    tabPanel("Nutrition Relationships",
             sidebarLayout(sidebarPanel(
               p(
                 "The data used was created using a number of datasets
                   available from the",
                 a("NHANES Database", href = "https://www.cdc.gov/nchs/nhanes/index.htm")
               ),
               br(),
               selectInput(
                 inputId = "Nutrition3",
                 label = "Select a nutrition variable",
                 choices = nutrition_variables
               ),
               
               selectInput(
                 inputId = "Nutrition4",
                 label = "Select a nutrition variable",
                 choices = nutrition_variables
               ),
               
               selectInput(
                 inputId = "Demographic4",
                 label = "Select a demographic variable",
                 choices = c(
                   'Age',
                   'BMI',
                   'Diabetes',
                   'Food Security',
                   'Gender',
                   'Poverty Ratio',
                   'Race',
                   'Smoking Status',
                   'Weight Loss',
                   'Year'
                 )
               )
             ),
             mainPanel(plotOutput(outputId = "smooth3"))))
  )
)

# Define server
server <- function(input, output){
  
  output$line1 <- renderPlot({
    if (input$Demographic == 'Gender'){column_name <- dat$gender}
    if (input$Demographic == 'Race'){column_name <- dat$race}
    if (input$Demographic == 'Age'){column_name <- dat$age_cat}
    if (input$Demographic == 'Food Security'){column_name <- dat$food_security}
    if (input$Demographic == 'Poverty Ratio'){column_name <- dat$poverty_cat}
    if (input$Demographic == 'BMI'){column_name <- dat$bmi_cat}
    if (input$Demographic == 'Diabetes'){column_name <- dat$diabetes}
    if (input$Demographic == 'Smoking Status'){column_name <- dat$cursmoke}
    if (input$Demographic == 'Weight Loss'){column_name <- dat$tryweloss}
    dat <- cbind(dat, column_name)
    if (input$Demographic == 'Food Security'){
      dat <- dat %>%
        filter(wave != "2015-2016")
    }
    trend <- data.frame(matrix(ncol = 3, nrow=1))
    cnames <- c("year", "category", "freq")
    colnames(trend) <- cnames
    years <- c(unique(dat$wave))
    category <- c(unique(as.character(dat$column_name)))
    category <- category[complete.cases(category)]
    
    for (year in years){
      for (i in (1:length(category))){
        yr = year
        cat = category[i]
        pcts = with(dat %>% filter(wave == year),
                    wpct(column_name, weight =  WTINT2YR))*100
        pct = pcts[[i]]
        newrow = c(yr, cat, pct)
        trend <- rbind(trend, newrow)
      }
    }
    trend <- trend %>% drop_na
    trend$freq<-as.numeric(trend$freq)
    
    trend %>% 
      ggplot(aes(year, freq, fill = category, group = category, color = category)) +
      geom_line(color = "black", size = 0.3) +
      geom_point(colour="black", pch=21, size = 3) +
      theme_light()+
      scale_colour_colorblind()+
      labs(
        title = "Demographic Trends in the United States",
        caption = "NHANES 1999-2016 survey",
        x = "Survey Cycle Years",
        y = "Percentage",
        fill = input$Demographic
      )
  })
  
  output$line2 <- renderPlot({
    if (input$Demographic2 != 'None'){
      if (input$Demographic2 == 'Gender'){column_name2 <- full_food_data$gender}
      if (input$Demographic2 == 'Race'){column_name2 <- full_food_data$race}
      if (input$Demographic2 == 'Age'){column_name2 <- full_food_data$age_cat}
      if (input$Demographic2 == 'Food Security'){column_name2 <- full_food_data$food_security}
      if (input$Demographic2 == 'Poverty Ratio'){column_name2 <- full_food_data$poverty_cat}
      if (input$Demographic2 == 'BMI'){column_name2 <- full_food_data$bmi_cat}
      if (input$Demographic2 == 'Diabetes'){column_name2 <- full_food_data$diabetes}
      if (input$Demographic2 == 'Smoking Status'){column_name2 <- full_food_data$cursmoke}
      if (input$Demographic2 == 'Weight Loss'){column_name2 <- full_food_data$tryweloss}
      full_food_data <- cbind(full_food_data, column_name2)
      full_food_data %>%
        filter(RIDAGEYR >= 2) %>%
        select(wave, grams, general_categories, column_name2) %>%
        drop_na%>%
        group_by(wave, general_categories, column_name2) %>%
        summarise(grams.mean = mean(grams), grams.sd = sd(grams), n=n())%>%
        filter(general_categories %in% c(input$Food)) %>%
        ggplot()+
        geom_point(aes(x=wave, y = grams.mean, color = column_name2)) +
        geom_errorbar(aes(x=wave, 
                          ymin=grams.mean-grams.sd*1.96/sqrt(n), 
                          ymax=grams.mean+grams.sd*1.96/sqrt(n), 
                          colour = column_name2), 
                      width = .2)+
        geom_line(aes(x=wave, y = grams.mean, group = column_name2, colour=column_name2), pch=21, size = 0.3)+
        theme_light()+
        scale_colour_colorblind()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        theme(legend.text = element_text(size = 10))+
        labs(
          title = "Daily Food Trends in the United States",
          caption = "NHANES 1999-2016 survey",
          x = "Survey Cycle Years",
          y = "Average Grams per Day",
          color = input$Demographic2
        )+
        facet_grid(~column_name2)
    }
    else {
      full_food_data %>%
        filter(RIDAGEYR >= 2) %>%
        select(wave, grams, general_categories) %>%
        drop_na%>%
        group_by(wave, general_categories) %>%
        summarise(grams.mean = mean(grams), grams.sd = sd(grams), n=n())%>%
        filter(general_categories %in% c(input$Food)) %>%
        ggplot()+
        geom_point(aes(x=wave, y = grams.mean)) +
        geom_errorbar(aes(x=wave, 
                          ymin=grams.mean-grams.sd*1.96/sqrt(n), 
                          ymax=grams.mean+grams.sd*1.96/sqrt(n)), 
                      width = .2)+
        geom_line(aes(x=wave, y = grams.mean, group = general_categories), pch=21, size = 0.3)+
        theme_light()+
        theme_light()+
        labs(
          title = "Daily Food Trends in the United States",
          caption = "NHANES 1999-2016 survey",
          x = "Survey Cycle Years",
          y = "Average Grams per Day"
        )
    }
  })
  
  output$smooth1 <- renderPlot({
    if (input$Var == 'Age (Years)'){column_name3 <- full_food_data$RIDAGEYR}
    if (input$Var == 'Poverty Ratio'){column_name3 <- full_food_data$INDFMPIR}
    if (input$Var == 'BMI (kg/m^2)'){column_name3 <- full_food_data$BMXBMI}
    full_food_data <- cbind(full_food_data, column_name3)
    full_food_data %>%
      filter(RIDAGEYR >= 2) %>%
      select(wave, grams, general_categories, column_name3) %>%
      filter(general_categories %in% c(input$Food2)) %>%
      drop_na%>%
      ggplot(aes(column_name3, grams))+
      geom_point(aes(x=column_name3, y = grams, color = wave), alpha = 1/2)+
      geom_smooth(aes(column_name3, grams))+
      theme_light()+
      scale_colour_simpsons()+
      theme_light()+
      labs(
        title = "United States Daily Food Scatterplot",
        caption = "NHANES 1999-2016 survey",
        x = input$Var,
        y = "Average Grams per Day",
        color = 'Wave'
      )
  })
  
  output$line3 <- renderPlot({
    if (input$Demographic3 != 'None'){
      if (input$Demographic3 == 'Gender'){column_name4 <- full_nutrition_data$gender}
      if (input$Demographic3 == 'Race'){column_name4 <- full_nutrition_data$race}
      if (input$Demographic3 == 'Age'){column_name4 <- full_nutrition_data$age_cat}
      if (input$Demographic3 == 'Food Security'){column_name4 <- full_nutrition_data$food_security}
      if (input$Demographic3 == 'Poverty Ratio'){column_name4 <- full_nutrition_data$poverty_cat}
      if (input$Demographic3 == 'BMI'){column_name4 <- full_nutrition_data$bmi_cat}
      if (input$Demographic3 == 'Diabetes'){column_name4 <- full_nutrition_data$diabetes}
      if (input$Demographic3 == 'Smoking Status'){column_name4 <- full_nutrition_data$cursmoke}
      if (input$Demographic3 == 'Weight Loss'){column_name4 <- full_nutrition_data$tryweloss}
      
      full_nutrition_data <- cbind(full_nutrition_data, column_name4)
      
      if (input$Nutrition == 'Energy (kcal)'){column_name5 <- full_nutrition_data$Calories}
      if (input$Nutrition == 'Protein (gm)'){column_name5 <- full_nutrition_data$Protein}
      if (input$Nutrition == 'Carbohydrate (gm)'){column_name5 <- full_nutrition_data$Carbohydrates}
      if (input$Nutrition == 'Total sugars (gm)'){column_name5 <- full_nutrition_data$Sugar}
      if (input$Nutrition == 'Dietary fiber (gm)'){column_name5 <- full_nutrition_data$Fiber}
      if (input$Nutrition == 'Total fat (gm)'){column_name5 <- full_nutrition_data$TotalFat}
      if (input$Nutrition == 'Total saturated fatty acids (gm)'){column_name5 <- full_nutrition_data$SFAT}
      if (input$Nutrition == 'Total monounsaturated fatty acids (gm)'){column_name5 <- full_nutrition_data$MFAT}
      if (input$Nutrition == 'Total polyunsaturated fatty acids (gm)'){column_name5 <- full_nutrition_data$PFAT}
      if (input$Nutrition == 'Cholesterol (mg)'){column_name5 <- full_nutrition_data$Cholesterol}
      if (input$Nutrition == 'Vitamin E as alpha-tocopherol (mg)'){column_name5 <- full_nutrition_data$VitE}
      if (input$Nutrition == 'Retinol (mcg)'){column_name5 <- full_nutrition_data$Retinol}
      if (input$Nutrition == 'Vitamin A, RAE (mcg)'){column_name5 <- full_nutrition_data$VitA}
      if (input$Nutrition == 'Alpha-carotene (mcg)'){column_name5 <- full_nutrition_data$AlphaCaro}
      if (input$Nutrition == 'Beta-carotene (mcg)'){column_name5 <- full_nutrition_data$BetaCaro}
      if (input$Nutrition == 'Beta-cryptoxanthin (mcg)'){column_name5 <- full_nutrition_data$BetaCryp}
      if (input$Nutrition == 'Lycopene (mcg)'){column_name5 <- full_nutrition_data$Lycopene}
      if (input$Nutrition == 'Lutein + zeaxanthin (mcg)'){column_name5 <- full_nutrition_data$Lutein}
      if (input$Nutrition == 'Thiamin (Vitamin B1) (mg)'){column_name5 <- full_nutrition_data$Thiamin}
      if (input$Nutrition == 'Riboflavin (Vitamin B2) (mg)'){column_name5 <- full_nutrition_data$Riboflavin}
      if (input$Nutrition == 'Niacin (mg)'){column_name5 <- full_nutrition_data$Niacin}
      if (input$Nutrition == 'Vitamin B6 (mg)'){column_name5 <- full_nutrition_data$VitB6}
      if (input$Nutrition == 'Total folate (mcg)'){column_name5 <- full_nutrition_data$TotFolate}
      if (input$Nutrition == 'Folic acid (mcg)'){column_name5 <- full_nutrition_data$FolicAcid}
      if (input$Nutrition == 'Food folate (mcg)'){column_name5 <- full_nutrition_data$FoodFolate}
      if (input$Nutrition == 'Folate, DFE (mcg)'){column_name5 <- full_nutrition_data$Folate}
      if (input$Nutrition == 'Vitamin B12 (mcg)'){column_name5 <- full_nutrition_data$VitB12}
      if (input$Nutrition == 'Vitamin C (mg)'){column_name5 <- full_nutrition_data$VitC}
      if (input$Nutrition == 'Vitamin K (mcg)'){column_name5 <- full_nutrition_data$VitK}
      if (input$Nutrition == 'Calcium (mg)'){column_name5 <- full_nutrition_data$Calcium}
      if (input$Nutrition == 'Phosphorus (mg)'){column_name5 <- full_nutrition_data$Phosphorus}
      if (input$Nutrition == 'Magnesium (mg)'){column_name5 <- full_nutrition_data$Magnesium}
      if (input$Nutrition == 'Iron (mg)'){column_name5 <- full_nutrition_data$Iron}
      if (input$Nutrition == 'Zinc (mg)'){column_name5 <- full_nutrition_data$Zinc}
      if (input$Nutrition == 'Copper (mg)'){column_name5 <- full_nutrition_data$Copper}
      if (input$Nutrition == 'Sodium (mg)'){column_name5 <- full_nutrition_data$Sodium}
      if (input$Nutrition == 'Potassium (mg)'){column_name5 <- full_nutrition_data$Potassium}
      if (input$Nutrition == 'Selenium (mg)'){column_name5 <- full_nutrition_data$Selenium}
      if (input$Nutrition == 'Caffeine (mg)'){column_name5 <- full_nutrition_data$Caffeine}
      if (input$Nutrition == 'Theobromine (mg)'){column_name5 <- full_nutrition_data$Theobromine}
      if (input$Nutrition == 'Alcohol (mg)'){column_name5 <- full_nutrition_data$Alcohol}
      if (input$Nutrition == 'Moisture (mg)'){column_name5 <- full_nutrition_data$Moisture}
      if (input$Nutrition == 'SFA 4:0 (Butanoic) (gm)'){column_name5 <- full_nutrition_data$Butanoic}
      if (input$Nutrition == 'SFA 6:0 (Hexanoic) (gm)'){column_name5 <- full_nutrition_data$Hexanoic}
      if (input$Nutrition == 'SFA 8:0 (Octanoic) (gm)'){column_name5 <- full_nutrition_data$Octanoic}
      if (input$Nutrition == 'SFA 10:0 (Decanoic) (gm)'){column_name5 <- full_nutrition_data$Decanoic}
      if (input$Nutrition == 'SFA 12:0 (Dodecanoic) (gm)'){column_name5 <- full_nutrition_data$Dodecanoic}
      if (input$Nutrition == 'SFA 14:0 (Tetradecanoic) (gm)'){column_name5 <- full_nutrition_data$Tetradecanoic}
      if (input$Nutrition == 'SFA 16:0 (Hexadecanoic) (gm)'){column_name5 <- full_nutrition_data$Hexadecanoic}
      if (input$Nutrition == 'SFA 18:0 (Octadecanoic) (gm)'){column_name5 <- full_nutrition_data$Octadecanoic}
      if (input$Nutrition == 'MFA 16:1 (Hexadecenoic) (gm)'){column_name5 <- full_nutrition_data$Hexadecenoic}
      if (input$Nutrition == 'MFA 18:1 (Octadecenoic) (gm)'){column_name5 <- full_nutrition_data$Octadecenoic}
      if (input$Nutrition == 'MFA 20:1 (Eicosenoic) (gm)'){column_name5 <- full_nutrition_data$Eicosenoic}
      if (input$Nutrition == 'MFA 22:1 (Docosenoic) (gm)'){column_name5 <- full_nutrition_data$Docosenoic}
      if (input$Nutrition == 'PFA 18:2 (Octadecadienoic) (gm)'){column_name5 <- full_nutrition_data$Octadecadienoic}
      if (input$Nutrition == 'PFA 18:3 (Octadecatrienoic) (gm)'){column_name5 <- full_nutrition_data$Octadecatrienoic}
      if (input$Nutrition == 'PFA 18:4 (Octadecatetraenoic) (gm)'){column_name5 <- full_nutrition_data$Octadecatetraenoic}
      if (input$Nutrition == 'PFA 20:4 (Eicosatetraenoic) (gm)'){column_name5 <- full_nutrition_data$Eicosatetraenoic}
      if (input$Nutrition == 'PFA 20:5 (Eicosapentaenoic) (gm)'){column_name5 <- full_nutrition_data$Eicosapentaenoic}
      if (input$Nutrition == 'PFA 22:5 (Docosapentaenoic) (gm)'){column_name5 <- full_nutrition_data$Docosapentaenoic}
      if (input$Nutrition == 'PFA 22:6 (Docosahexaenoic) (gm)'){column_name5 <- full_nutrition_data$Docosahexaenoic}
      if (input$Nutrition == 'Plain water (gm)'){column_name5 <- full_nutrition_data$PlainWater}
      if (input$Nutrition == 'Tap water (gm)'){column_name5 <- full_nutrition_data$TapWater}
      
      full_nutrition_data <- cbind(full_nutrition_data, column_name5)
      
      full_nutrition_data %>%
        filter(RIDAGEYR >= 2) %>%
        select(wave, column_name4, column_name5) %>%
        drop_na%>%
        group_by(wave,column_name4) %>%
        summarise(colmean = mean(column_name5), colsd = sd(column_name5), n=n())%>%
        ggplot()+
        geom_point(aes(x=wave, y = colmean, color = column_name4)) +
        geom_line(aes(x=wave, y = colmean, group = column_name4, colour=column_name4), pch=21, size = 0.3)+
        geom_errorbar(aes(x=wave, 
                          ymin=colmean-colsd*1.96/sqrt(n), 
                          ymax=colmean+colsd*1.96/sqrt(n), 
                          colour = column_name4), width = .2)+
        theme_light()+
        scale_colour_colorblind()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        theme(legend.text = element_text(size = 10))+
        labs(
          title = "Daily Nutrition Trends in the United States",
          caption = "NHANES 2001-2016 survey",
          x = "Survey Cycle Years",
          y = input$Nutrition,
          color = input$Demographic3
        )+
        facet_grid(~column_name4)
    }
    else {
      
      if (input$Nutrition == 'Energy (kcal)'){column_name6 <- full_nutrition_data$Calories}
      if (input$Nutrition == 'Protein (gm)'){column_name6 <- full_nutrition_data$Protein}
      if (input$Nutrition == 'Carbohydrate (gm)'){column_name6 <- full_nutrition_data$Carbohydrates}
      if (input$Nutrition == 'Total sugars (gm)'){column_name6 <- full_nutrition_data$Sugar}
      if (input$Nutrition == 'Dietary fiber (gm)'){column_name6 <- full_nutrition_data$Fiber}
      if (input$Nutrition == 'Total fat (gm)'){column_name6 <- full_nutrition_data$TotalFat}
      if (input$Nutrition == 'Total saturated fatty acids (gm)'){column_name6 <- full_nutrition_data$SFAT}
      if (input$Nutrition == 'Total monounsaturated fatty acids (gm)'){column_name6 <- full_nutrition_data$MFAT}
      if (input$Nutrition == 'Total polyunsaturated fatty acids (gm)'){column_name6 <- full_nutrition_data$PFAT}
      if (input$Nutrition == 'Cholesterol (mg)'){column_name6 <- full_nutrition_data$Cholesterol}
      if (input$Nutrition == 'Vitamin E as alpha-tocopherol (mg)'){column_name6 <- full_nutrition_data$VitE}
      if (input$Nutrition == 'Retinol (mcg)'){column_name6 <- full_nutrition_data$Retinol}
      if (input$Nutrition == 'Vitamin A, RAE (mcg)'){column_name6 <- full_nutrition_data$VitA}
      if (input$Nutrition == 'Alpha-carotene (mcg)'){column_name6 <- full_nutrition_data$AlphaCaro}
      if (input$Nutrition == 'Beta-carotene (mcg)'){column_name6 <- full_nutrition_data$BetaCaro}
      if (input$Nutrition == 'Beta-cryptoxanthin (mcg)'){column_name6 <- full_nutrition_data$BetaCryp}
      if (input$Nutrition == 'Lycopene (mcg)'){column_name6 <- full_nutrition_data$Lycopene}
      if (input$Nutrition == 'Lutein + zeaxanthin (mcg)'){column_name6 <- full_nutrition_data$Lutein}
      if (input$Nutrition == 'Thiamin (Vitamin B1) (mg)'){column_name6 <- full_nutrition_data$Thiamin}
      if (input$Nutrition == 'Riboflavin (Vitamin B2) (mg)'){column_name6 <- full_nutrition_data$Riboflavin}
      if (input$Nutrition == 'Niacin (mg)'){column_name6 <- full_nutrition_data$Niacin}
      if (input$Nutrition == 'Vitamin B6 (mg)'){column_name6 <- full_nutrition_data$VitB6}
      if (input$Nutrition == 'Total folate (mcg)'){column_name6 <- full_nutrition_data$TotFolate}
      if (input$Nutrition == 'Folic acid (mcg)'){column_name6 <- full_nutrition_data$FolicAcid}
      if (input$Nutrition == 'Food folate (mcg)'){column_name6 <- full_nutrition_data$FoodFolate}
      if (input$Nutrition == 'Folate, DFE (mcg)'){column_name6 <- full_nutrition_data$Folate}
      if (input$Nutrition == 'Vitamin B12 (mcg)'){column_name6 <- full_nutrition_data$VitB12}
      if (input$Nutrition == 'Vitamin C (mg)'){column_name6 <- full_nutrition_data$VitC}
      if (input$Nutrition == 'Vitamin K (mcg)'){column_name6 <- full_nutrition_data$VitK}
      if (input$Nutrition == 'Calcium (mg)'){column_name6 <- full_nutrition_data$Calcium}
      if (input$Nutrition == 'Phosphorus (mg)'){column_name6 <- full_nutrition_data$Phosphorus}
      if (input$Nutrition == 'Magnesium (mg)'){column_name6 <- full_nutrition_data$Magnesium}
      if (input$Nutrition == 'Iron (mg)'){column_name6 <- full_nutrition_data$Iron}
      if (input$Nutrition == 'Zinc (mg)'){column_name6 <- full_nutrition_data$Zinc}
      if (input$Nutrition == 'Copper (mg)'){column_name6 <- full_nutrition_data$Copper}
      if (input$Nutrition == 'Sodium (mg)'){column_name6 <- full_nutrition_data$Sodium}
      if (input$Nutrition == 'Potassium (mg)'){column_name6 <- full_nutrition_data$Potassium}
      if (input$Nutrition == 'Selenium (mg)'){column_name6 <- full_nutrition_data$Selenium}
      if (input$Nutrition == 'Caffeine (mg)'){column_name6 <- full_nutrition_data$Caffeine}
      if (input$Nutrition == 'Theobromine (mg)'){column_name6 <- full_nutrition_data$Theobromine}
      if (input$Nutrition == 'Alcohol (mg)'){column_name6 <- full_nutrition_data$Alcohol}
      if (input$Nutrition == 'Moisture (mg)'){column_name6 <- full_nutrition_data$Moisture}
      if (input$Nutrition == 'SFA 4:0 (Butanoic) (gm)'){column_name6 <- full_nutrition_data$Butanoic}
      if (input$Nutrition == 'SFA 6:0 (Hexanoic) (gm)'){column_name6 <- full_nutrition_data$Hexanoic}
      if (input$Nutrition == 'SFA 8:0 (Octanoic) (gm)'){column_name6 <- full_nutrition_data$Octanoic}
      if (input$Nutrition == 'SFA 10:0 (Decanoic) (gm)'){column_name6 <- full_nutrition_data$Decanoic}
      if (input$Nutrition == 'SFA 12:0 (Dodecanoic) (gm)'){column_name6 <- full_nutrition_data$Dodecanoic}
      if (input$Nutrition == 'SFA 14:0 (Tetradecanoic) (gm)'){column_name6 <- full_nutrition_data$Tetradecanoic}
      if (input$Nutrition == 'SFA 16:0 (Hexadecanoic) (gm)'){column_name6 <- full_nutrition_data$Hexadecanoic}
      if (input$Nutrition == 'SFA 18:0 (Octadecanoic) (gm)'){column_name6 <- full_nutrition_data$Octadecanoic}
      if (input$Nutrition == 'MFA 16:1 (Hexadecenoic) (gm)'){column_name6 <- full_nutrition_data$Hexadecenoic}
      if (input$Nutrition == 'MFA 18:1 (Octadecenoic) (gm)'){column_name6 <- full_nutrition_data$Octadecenoic}
      if (input$Nutrition == 'MFA 20:1 (Eicosenoic) (gm)'){column_name6 <- full_nutrition_data$Eicosenoic}
      if (input$Nutrition == 'MFA 22:1 (Docosenoic) (gm)'){column_name6 <- full_nutrition_data$Docosenoic}
      if (input$Nutrition == 'PFA 18:2 (Octadecadienoic) (gm)'){column_name6 <- full_nutrition_data$Octadecadienoic}
      if (input$Nutrition == 'PFA 18:3 (Octadecatrienoic) (gm)'){column_name6 <- full_nutrition_data$Octadecatrienoic}
      if (input$Nutrition == 'PFA 18:4 (Octadecatetraenoic) (gm)'){column_name6 <- full_nutrition_data$Octadecatetraenoic}
      if (input$Nutrition == 'PFA 20:4 (Eicosatetraenoic) (gm)'){column_name6 <- full_nutrition_data$Eicosatetraenoic}
      if (input$Nutrition == 'PFA 20:5 (Eicosapentaenoic) (gm)'){column_name6 <- full_nutrition_data$Eicosapentaenoic}
      if (input$Nutrition == 'PFA 22:5 (Docosapentaenoic) (gm)'){column_name6 <- full_nutrition_data$Docosapentaenoic}
      if (input$Nutrition == 'PFA 22:6 (Docosahexaenoic) (gm)'){column_name6 <- full_nutrition_data$Docosahexaenoic}
      if (input$Nutrition == 'Plain water (gm)'){column_name6 <- full_nutrition_data$PlainWater}
      if (input$Nutrition == 'Tap water (gm)'){column_name6 <- full_nutrition_data$TapWater}
      
      full_nutrition_data <- cbind(full_nutrition_data, column_name6)
      
      temp_nutrition <- full_nutrition_data %>%
        filter(RIDAGEYR >= 2) %>%
        select(wave, column_name6,) %>%
        drop_na%>%
        group_by(wave) %>%
        summarise(colmean = mean(column_name6), colsd = sd(column_name6), n=n())
      temp_nutrition$group <- rep("group",nrow(temp_nutrition))
      temp_nutrition %>%
        ggplot()+
        geom_point(aes(x=wave, y = colmean)) +
        geom_errorbar(aes(x=wave, 
                          ymin=colmean-colsd*1.96/sqrt(n), 
                          ymax=colmean+colsd*1.96/sqrt(n)), 
                      width = .2)+
        geom_line(aes(x=wave, y = colmean, group = group), pch=21, size = 0.3)+
        theme_light()+
        scale_colour_colorblind()+
        theme_light()+
        labs(
          title = "Daily Nutrition Trends in the United States",
          caption = "NHANES 2001-2016 survey",
          x = "Survey Cycle Years",
          y = input$Nutrition
        )
    }
  })
  
  output$smooth2 <- renderPlot({
    if (input$Var2 == 'Age (Years)'){column_name7 <- full_nutrition_data$RIDAGEYR}
    if (input$Var2 == 'Poverty Ratio'){column_name7 <- full_nutrition_data$INDFMPIR}
    if (input$Var2 == 'BMI (kg/m^2)'){column_name7 <- full_nutrition_data$BMXBMI}
    full_nutrition_data <- cbind(full_nutrition_data, column_name7)
    
    if (input$Nutrition2 == 'Energy (kcal)'){column_name8 <- full_nutrition_data$Calories}
    if (input$Nutrition2 == 'Protein (gm)'){column_name8 <- full_nutrition_data$Protein}
    if (input$Nutrition2 == 'Carbohydrate (gm)'){column_name8 <- full_nutrition_data$Carbohydrates}
    if (input$Nutrition2 == 'Total sugars (gm)'){column_name8 <- full_nutrition_data$Sugar}
    if (input$Nutrition2 == 'Dietary fiber (gm)'){column_name8 <- full_nutrition_data$Fiber}
    if (input$Nutrition2 == 'Total fat (gm)'){column_name8 <- full_nutrition_data$TotalFat}
    if (input$Nutrition2 == 'Total saturated fatty acids (gm)'){column_name8 <- full_nutrition_data$SFAT}
    if (input$Nutrition2 == 'Total monounsaturated fatty acids (gm)'){column_name8 <- full_nutrition_data$MFAT}
    if (input$Nutrition2 == 'Total polyunsaturated fatty acids (gm)'){column_name8 <- full_nutrition_data$PFAT}
    if (input$Nutrition2 == 'Cholesterol (mg)'){column_name8 <- full_nutrition_data$Cholesterol}
    if (input$Nutrition2 == 'Vitamin E as alpha-tocopherol (mg)'){column_name8 <- full_nutrition_data$VitE}
    if (input$Nutrition2 == 'Retinol (mcg)'){column_name8 <- full_nutrition_data$Retinol}
    if (input$Nutrition2 == 'Vitamin A, RAE (mcg)'){column_name8 <- full_nutrition_data$VitA}
    if (input$Nutrition2 == 'Alpha-carotene (mcg)'){column_name8 <- full_nutrition_data$AlphaCaro}
    if (input$Nutrition2 == 'Beta-carotene (mcg)'){column_name8 <- full_nutrition_data$BetaCaro}
    if (input$Nutrition2 == 'Beta-cryptoxanthin (mcg)'){column_name8 <- full_nutrition_data$BetaCryp}
    if (input$Nutrition2 == 'Lycopene (mcg)'){column_name8 <- full_nutrition_data$Lycopene}
    if (input$Nutrition2 == 'Lutein + zeaxanthin (mcg)'){column_name8 <- full_nutrition_data$Lutein}
    if (input$Nutrition2 == 'Thiamin (Vitamin B1) (mg)'){column_name8 <- full_nutrition_data$Thiamin}
    if (input$Nutrition2 == 'Riboflavin (Vitamin B2) (mg)'){column_name8 <- full_nutrition_data$Riboflavin}
    if (input$Nutrition2 == 'Niacin (mg)'){column_name8 <- full_nutrition_data$Niacin}
    if (input$Nutrition2 == 'Vitamin B6 (mg)'){column_name8 <- full_nutrition_data$VitB6}
    if (input$Nutrition2 == 'Total folate (mcg)'){column_name8 <- full_nutrition_data$TotFolate}
    if (input$Nutrition2 == 'Folic acid (mcg)'){column_name8 <- full_nutrition_data$FolicAcid}
    if (input$Nutrition2 == 'Food folate (mcg)'){column_name8 <- full_nutrition_data$FoodFolate}
    if (input$Nutrition2 == 'Folate, DFE (mcg)'){column_name8 <- full_nutrition_data$Folate}
    if (input$Nutrition2 == 'Vitamin B12 (mcg)'){column_name8 <- full_nutrition_data$VitB12}
    if (input$Nutrition2 == 'Vitamin C (mg)'){column_name8 <- full_nutrition_data$VitC}
    if (input$Nutrition2 == 'Vitamin K (mcg)'){column_name8 <- full_nutrition_data$VitK}
    if (input$Nutrition2 == 'Calcium (mg)'){column_name8 <- full_nutrition_data$Calcium}
    if (input$Nutrition2 == 'Phosphorus (mg)'){column_name8 <- full_nutrition_data$Phosphorus}
    if (input$Nutrition2 == 'Magnesium (mg)'){column_name8 <- full_nutrition_data$Magnesium}
    if (input$Nutrition2 == 'Iron (mg)'){column_name8 <- full_nutrition_data$Iron}
    if (input$Nutrition2 == 'Zinc (mg)'){column_name8 <- full_nutrition_data$Zinc}
    if (input$Nutrition2 == 'Copper (mg)'){column_name8 <- full_nutrition_data$Copper}
    if (input$Nutrition2 == 'Sodium (mg)'){column_name8 <- full_nutrition_data$Sodium}
    if (input$Nutrition2 == 'Potassium (mg)'){column_name8 <- full_nutrition_data$Potassium}
    if (input$Nutrition2 == 'Selenium (mg)'){column_name8 <- full_nutrition_data$Selenium}
    if (input$Nutrition2 == 'Caffeine (mg)'){column_name8 <- full_nutrition_data$Caffeine}
    if (input$Nutrition2 == 'Theobromine (mg)'){column_name8 <- full_nutrition_data$Theobromine}
    if (input$Nutrition2 == 'Alcohol (mg)'){column_name8 <- full_nutrition_data$Alcohol}
    if (input$Nutrition2 == 'Moisture (mg)'){column_name8 <- full_nutrition_data$Moisture}
    if (input$Nutrition2 == 'SFA 4:0 (Butanoic) (gm)'){column_name8 <- full_nutrition_data$Butanoic}
    if (input$Nutrition2 == 'SFA 6:0 (Hexanoic) (gm)'){column_name8 <- full_nutrition_data$Hexanoic}
    if (input$Nutrition2 == 'SFA 8:0 (Octanoic) (gm)'){column_name8 <- full_nutrition_data$Octanoic}
    if (input$Nutrition2 == 'SFA 10:0 (Decanoic) (gm)'){column_name8 <- full_nutrition_data$Decanoic}
    if (input$Nutrition2 == 'SFA 12:0 (Dodecanoic) (gm)'){column_name8 <- full_nutrition_data$Dodecanoic}
    if (input$Nutrition2 == 'SFA 14:0 (Tetradecanoic) (gm)'){column_name8 <- full_nutrition_data$Tetradecanoic}
    if (input$Nutrition2 == 'SFA 16:0 (Hexadecanoic) (gm)'){column_name8 <- full_nutrition_data$Hexadecanoic}
    if (input$Nutrition2 == 'SFA 18:0 (Octadecanoic) (gm)'){column_name8 <- full_nutrition_data$Octadecanoic}
    if (input$Nutrition2 == 'MFA 16:1 (Hexadecenoic) (gm)'){column_name8 <- full_nutrition_data$Hexadecenoic}
    if (input$Nutrition2 == 'MFA 18:1 (Octadecenoic) (gm)'){column_name8 <- full_nutrition_data$Octadecenoic}
    if (input$Nutrition2 == 'MFA 20:1 (Eicosenoic) (gm)'){column_name8 <- full_nutrition_data$Eicosenoic}
    if (input$Nutrition2 == 'MFA 22:1 (Docosenoic) (gm)'){column_name8 <- full_nutrition_data$Docosenoic}
    if (input$Nutrition2 == 'PFA 18:2 (Octadecadienoic) (gm)'){column_name8 <- full_nutrition_data$Octadecadienoic}
    if (input$Nutrition2 == 'PFA 18:3 (Octadecatrienoic) (gm)'){column_name8 <- full_nutrition_data$Octadecatrienoic}
    if (input$Nutrition2 == 'PFA 18:4 (Octadecatetraenoic) (gm)'){column_name8 <- full_nutrition_data$Octadecatetraenoic}
    if (input$Nutrition2 == 'PFA 20:4 (Eicosatetraenoic) (gm)'){column_name8 <- full_nutrition_data$Eicosatetraenoic}
    if (input$Nutrition2 == 'PFA 20:5 (Eicosapentaenoic) (gm)'){column_name8 <- full_nutrition_data$Eicosapentaenoic}
    if (input$Nutrition2 == 'PFA 22:5 (Docosapentaenoic) (gm)'){column_name8 <- full_nutrition_data$Docosapentaenoic}
    if (input$Nutrition2 == 'PFA 22:6 (Docosahexaenoic) (gm)'){column_name8 <- full_nutrition_data$Docosahexaenoic}
    if (input$Nutrition2 == 'Plain water (gm)'){column_name8 <- full_nutrition_data$PlainWater}
    if (input$Nutrition2 == 'Tap water (gm)'){column_name8 <- full_nutrition_data$TapWater}
    
    full_nutrition_data <- cbind(full_nutrition_data, column_name8)
    
    full_nutrition_data %>%
      filter(RIDAGEYR >= 2) %>%
      select(wave, column_name7, column_name8) %>%
      drop_na%>%
      ggplot(aes(x=column_name7, y = column_name8))+
      geom_point(aes(x=column_name7, y = column_name8, color = wave), alpha = 1/2)+
      geom_smooth(aes(column_name7, column_name8))+
      theme_light()+
      scale_colour_simpsons()+
      labs(
        title = "United States Daily Nutrition Scatterplot",
        caption = "NHANES 2001-2016 survey",
        x = input$Var2,
        y = input$Nutrition2,
        color = 'Wave'
      )
  })
  
  output$smooth3 <- renderPlot({
    if (input$Demographic4 == 'Gender'){column_name9 <- full_nutrition_data$gender}
    if (input$Demographic4 == 'Race'){column_name9 <- full_nutrition_data$race}
    if (input$Demographic4 == 'Age'){column_name9 <- full_nutrition_data$age_cat}
    if (input$Demographic4 == 'Food Security'){column_name9 <- full_nutrition_data$food_security}
    if (input$Demographic4 == 'Poverty Ratio'){column_name9 <- full_nutrition_data$poverty_cat}
    if (input$Demographic4 == 'BMI'){column_name9 <- full_nutrition_data$bmi_cat}
    if (input$Demographic4 == 'Diabetes'){column_name9 <- full_nutrition_data$diabetes}
    if (input$Demographic4 == 'Smoking Status'){column_name9 <- full_nutrition_data$cursmoke}
    if (input$Demographic4 == 'Weight Loss'){column_name9 <- full_nutrition_data$tryweloss}
    if (input$Demographic4 == 'Year'){column_name9 <- full_nutrition_data$wave}
    full_nutrition_data <- cbind(full_nutrition_data, column_name9)
    
    if (input$Nutrition3 == 'Energy (kcal)'){column_name10 <- full_nutrition_data$Calories}
    if (input$Nutrition3 == 'Protein (gm)'){column_name10 <- full_nutrition_data$Protein}
    if (input$Nutrition3 == 'Carbohydrate (gm)'){column_name10 <- full_nutrition_data$Carbohydrates}
    if (input$Nutrition3 == 'Total sugars (gm)'){column_name10 <- full_nutrition_data$Sugar}
    if (input$Nutrition3 == 'Dietary fiber (gm)'){column_name10 <- full_nutrition_data$Fiber}
    if (input$Nutrition3 == 'Total fat (gm)'){column_name10 <- full_nutrition_data$TotalFat}
    if (input$Nutrition3 == 'Total saturated fatty acids (gm)'){column_name10 <- full_nutrition_data$SFAT}
    if (input$Nutrition3 == 'Total monounsaturated fatty acids (gm)'){column_name10 <- full_nutrition_data$MFAT}
    if (input$Nutrition3 == 'Total polyunsaturated fatty acids (gm)'){column_name10 <- full_nutrition_data$PFAT}
    if (input$Nutrition3 == 'Cholesterol (mg)'){column_name10 <- full_nutrition_data$Cholesterol}
    if (input$Nutrition3 == 'Vitamin E as alpha-tocopherol (mg)'){column_name10 <- full_nutrition_data$VitE}
    if (input$Nutrition3 == 'Retinol (mcg)'){column_name10 <- full_nutrition_data$Retinol}
    if (input$Nutrition3 == 'Vitamin A, RAE (mcg)'){column_name10 <- full_nutrition_data$VitA}
    if (input$Nutrition3 == 'Alpha-carotene (mcg)'){column_name10 <- full_nutrition_data$AlphaCaro}
    if (input$Nutrition3 == 'Beta-carotene (mcg)'){column_name10 <- full_nutrition_data$BetaCaro}
    if (input$Nutrition3 == 'Beta-cryptoxanthin (mcg)'){column_name10 <- full_nutrition_data$BetaCryp}
    if (input$Nutrition3 == 'Lycopene (mcg)'){column_name10 <- full_nutrition_data$Lycopene}
    if (input$Nutrition3 == 'Lutein + zeaxanthin (mcg)'){column_name10 <- full_nutrition_data$Lutein}
    if (input$Nutrition3 == 'Thiamin (Vitamin B1) (mg)'){column_name10 <- full_nutrition_data$Thiamin}
    if (input$Nutrition3 == 'Riboflavin (Vitamin B2) (mg)'){column_name10 <- full_nutrition_data$Riboflavin}
    if (input$Nutrition3 == 'Niacin (mg)'){column_name10 <- full_nutrition_data$Niacin}
    if (input$Nutrition3 == 'Vitamin B6 (mg)'){column_name10 <- full_nutrition_data$VitB6}
    if (input$Nutrition3 == 'Total folate (mcg)'){column_name10 <- full_nutrition_data$TotFolate}
    if (input$Nutrition3 == 'Folic acid (mcg)'){column_name10 <- full_nutrition_data$FolicAcid}
    if (input$Nutrition3 == 'Food folate (mcg)'){column_name10 <- full_nutrition_data$FoodFolate}
    if (input$Nutrition3 == 'Folate, DFE (mcg)'){column_name10 <- full_nutrition_data$Folate}
    if (input$Nutrition3 == 'Vitamin B12 (mcg)'){column_name10 <- full_nutrition_data$VitB12}
    if (input$Nutrition3 == 'Vitamin C (mg)'){column_name10 <- full_nutrition_data$VitC}
    if (input$Nutrition3 == 'Vitamin K (mcg)'){column_name10 <- full_nutrition_data$VitK}
    if (input$Nutrition3 == 'Calcium (mg)'){column_name10 <- full_nutrition_data$Calcium}
    if (input$Nutrition3 == 'Phosphorus (mg)'){column_name10 <- full_nutrition_data$Phosphorus}
    if (input$Nutrition3 == 'Magnesium (mg)'){column_name10 <- full_nutrition_data$Magnesium}
    if (input$Nutrition3 == 'Iron (mg)'){column_name10 <- full_nutrition_data$Iron}
    if (input$Nutrition3 == 'Zinc (mg)'){column_name10 <- full_nutrition_data$Zinc}
    if (input$Nutrition3 == 'Copper (mg)'){column_name10 <- full_nutrition_data$Copper}
    if (input$Nutrition3 == 'Sodium (mg)'){column_name10 <- full_nutrition_data$Sodium}
    if (input$Nutrition3 == 'Potassium (mg)'){column_name10 <- full_nutrition_data$Potassium}
    if (input$Nutrition3 == 'Selenium (mg)'){column_name10 <- full_nutrition_data$Selenium}
    if (input$Nutrition3 == 'Caffeine (mg)'){column_name10 <- full_nutrition_data$Caffeine}
    if (input$Nutrition3 == 'Theobromine (mg)'){column_name10 <- full_nutrition_data$Theobromine}
    if (input$Nutrition3 == 'Alcohol (mg)'){column_name10 <- full_nutrition_data$Alcohol}
    if (input$Nutrition3 == 'Moisture (mg)'){column_name10 <- full_nutrition_data$Moisture}
    if (input$Nutrition3 == 'SFA 4:0 (Butanoic) (gm)'){column_name10 <- full_nutrition_data$Butanoic}
    if (input$Nutrition3 == 'SFA 6:0 (Hexanoic) (gm)'){column_name10 <- full_nutrition_data$Hexanoic}
    if (input$Nutrition3 == 'SFA 8:0 (Octanoic) (gm)'){column_name10 <- full_nutrition_data$Octanoic}
    if (input$Nutrition3 == 'SFA 10:0 (Decanoic) (gm)'){column_name10 <- full_nutrition_data$Decanoic}
    if (input$Nutrition3 == 'SFA 12:0 (Dodecanoic) (gm)'){column_name10 <- full_nutrition_data$Dodecanoic}
    if (input$Nutrition3 == 'SFA 14:0 (Tetradecanoic) (gm)'){column_name10 <- full_nutrition_data$Tetradecanoic}
    if (input$Nutrition3 == 'SFA 16:0 (Hexadecanoic) (gm)'){column_name10 <- full_nutrition_data$Hexadecanoic}
    if (input$Nutrition3 == 'SFA 18:0 (Octadecanoic) (gm)'){column_name10 <- full_nutrition_data$Octadecanoic}
    if (input$Nutrition3 == 'MFA 16:1 (Hexadecenoic) (gm)'){column_name10 <- full_nutrition_data$Hexadecenoic}
    if (input$Nutrition3 == 'MFA 18:1 (Octadecenoic) (gm)'){column_name10 <- full_nutrition_data$Octadecenoic}
    if (input$Nutrition3 == 'MFA 20:1 (Eicosenoic) (gm)'){column_name10 <- full_nutrition_data$Eicosenoic}
    if (input$Nutrition3 == 'MFA 22:1 (Docosenoic) (gm)'){column_name10 <- full_nutrition_data$Docosenoic}
    if (input$Nutrition3 == 'PFA 18:2 (Octadecadienoic) (gm)'){column_name10 <- full_nutrition_data$Octadecadienoic}
    if (input$Nutrition3 == 'PFA 18:3 (Octadecatrienoic) (gm)'){column_name10 <- full_nutrition_data$Octadecatrienoic}
    if (input$Nutrition3 == 'PFA 18:4 (Octadecatetraenoic) (gm)'){column_name10 <- full_nutrition_data$Octadecatetraenoic}
    if (input$Nutrition3 == 'PFA 20:4 (Eicosatetraenoic) (gm)'){column_name10 <- full_nutrition_data$Eicosatetraenoic}
    if (input$Nutrition3 == 'PFA 20:5 (Eicosapentaenoic) (gm)'){column_name10 <- full_nutrition_data$Eicosapentaenoic}
    if (input$Nutrition3 == 'PFA 22:5 (Docosapentaenoic) (gm)'){column_name10 <- full_nutrition_data$Docosapentaenoic}
    if (input$Nutrition3 == 'PFA 22:6 (Docosahexaenoic) (gm)'){column_name10 <- full_nutrition_data$Docosahexaenoic}
    if (input$Nutrition3 == 'Plain water (gm)'){column_name10 <- full_nutrition_data$PlainWater}
    if (input$Nutrition3 == 'Tap water (gm)'){column_name10 <- full_nutrition_data$TapWater}
    
    full_nutrition_data <- cbind(full_nutrition_data, column_name10)

    if (input$Nutrition4 == 'Energy (kcal)'){column_name11 <- full_nutrition_data$Calories}
    if (input$Nutrition4 == 'Protein (gm)'){column_name11 <- full_nutrition_data$Protein}
    if (input$Nutrition4 == 'Carbohydrate (gm)'){column_name11 <- full_nutrition_data$Carbohydrates}
    if (input$Nutrition4 == 'Total sugars (gm)'){column_name11 <- full_nutrition_data$Sugar}
    if (input$Nutrition4 == 'Dietary fiber (gm)'){column_name11 <- full_nutrition_data$Fiber}
    if (input$Nutrition4 == 'Total fat (gm)'){column_name11 <- full_nutrition_data$TotalFat}
    if (input$Nutrition4 == 'Total saturated fatty acids (gm)'){column_name11 <- full_nutrition_data$SFAT}
    if (input$Nutrition4 == 'Total monounsaturated fatty acids (gm)'){column_name11 <- full_nutrition_data$MFAT}
    if (input$Nutrition4 == 'Total polyunsaturated fatty acids (gm)'){column_name11 <- full_nutrition_data$PFAT}
    if (input$Nutrition4 == 'Cholesterol (mg)'){column_name11 <- full_nutrition_data$Cholesterol}
    if (input$Nutrition4 == 'Vitamin E as alpha-tocopherol (mg)'){column_name11 <- full_nutrition_data$VitE}
    if (input$Nutrition4 == 'Retinol (mcg)'){column_name11 <- full_nutrition_data$Retinol}
    if (input$Nutrition4 == 'Vitamin A, RAE (mcg)'){column_name11 <- full_nutrition_data$VitA}
    if (input$Nutrition4 == 'Alpha-carotene (mcg)'){column_name11 <- full_nutrition_data$AlphaCaro}
    if (input$Nutrition4 == 'Beta-carotene (mcg)'){column_name11 <- full_nutrition_data$BetaCaro}
    if (input$Nutrition4 == 'Beta-cryptoxanthin (mcg)'){column_name11 <- full_nutrition_data$BetaCryp}
    if (input$Nutrition4 == 'Lycopene (mcg)'){column_name11 <- full_nutrition_data$Lycopene}
    if (input$Nutrition4 == 'Lutein + zeaxanthin (mcg)'){column_name11 <- full_nutrition_data$Lutein}
    if (input$Nutrition4 == 'Thiamin (Vitamin B1) (mg)'){column_name11 <- full_nutrition_data$Thiamin}
    if (input$Nutrition4 == 'Riboflavin (Vitamin B2) (mg)'){column_name11 <- full_nutrition_data$Riboflavin}
    if (input$Nutrition4 == 'Niacin (mg)'){column_name11 <- full_nutrition_data$Niacin}
    if (input$Nutrition4 == 'Vitamin B6 (mg)'){column_name11 <- full_nutrition_data$VitB6}
    if (input$Nutrition4 == 'Total folate (mcg)'){column_name11 <- full_nutrition_data$TotFolate}
    if (input$Nutrition4 == 'Folic acid (mcg)'){column_name11 <- full_nutrition_data$FolicAcid}
    if (input$Nutrition4 == 'Food folate (mcg)'){column_name11 <- full_nutrition_data$FoodFolate}
    if (input$Nutrition4 == 'Folate, DFE (mcg)'){column_name11 <- full_nutrition_data$Folate}
    if (input$Nutrition4 == 'Vitamin B12 (mcg)'){column_name11 <- full_nutrition_data$VitB12}
    if (input$Nutrition4 == 'Vitamin C (mg)'){column_name11 <- full_nutrition_data$VitC}
    if (input$Nutrition4 == 'Vitamin K (mcg)'){column_name11 <- full_nutrition_data$VitK}
    if (input$Nutrition4 == 'Calcium (mg)'){column_name11 <- full_nutrition_data$Calcium}
    if (input$Nutrition4 == 'Phosphorus (mg)'){column_name11 <- full_nutrition_data$Phosphorus}
    if (input$Nutrition4 == 'Magnesium (mg)'){column_name11 <- full_nutrition_data$Magnesium}
    if (input$Nutrition4 == 'Iron (mg)'){column_name11 <- full_nutrition_data$Iron}
    if (input$Nutrition4 == 'Zinc (mg)'){column_name11 <- full_nutrition_data$Zinc}
    if (input$Nutrition4 == 'Copper (mg)'){column_name11 <- full_nutrition_data$Copper}
    if (input$Nutrition4 == 'Sodium (mg)'){column_name11 <- full_nutrition_data$Sodium}
    if (input$Nutrition4 == 'Potassium (mg)'){column_name11 <- full_nutrition_data$Potassium}
    if (input$Nutrition4 == 'Selenium (mg)'){column_name11 <- full_nutrition_data$Selenium}
    if (input$Nutrition4 == 'Caffeine (mg)'){column_name11 <- full_nutrition_data$Caffeine}
    if (input$Nutrition4 == 'Theobromine (mg)'){column_name11 <- full_nutrition_data$Theobromine}
    if (input$Nutrition4 == 'Alcohol (gm)'){column_name11 <- full_nutrition_data$Alcohol}
    if (input$Nutrition4 == 'Moisture (mg)'){column_name11 <- full_nutrition_data$Moisture}
    if (input$Nutrition4 == 'SFA 4:0 (Butanoic) (gm)'){column_name11 <- full_nutrition_data$Butanoic}
    if (input$Nutrition4 == 'SFA 6:0 (Hexanoic) (gm)'){column_name11 <- full_nutrition_data$Hexanoic}
    if (input$Nutrition4 == 'SFA 8:0 (Octanoic) (gm)'){column_name11 <- full_nutrition_data$Octanoic}
    if (input$Nutrition4 == 'SFA 10:0 (Decanoic) (gm)'){column_name11 <- full_nutrition_data$Decanoic}
    if (input$Nutrition4 == 'SFA 12:0 (Dodecanoic) (gm)'){column_name11 <- full_nutrition_data$Dodecanoic}
    if (input$Nutrition4 == 'SFA 14:0 (Tetradecanoic) (gm)'){column_name11 <- full_nutrition_data$Tetradecanoic}
    if (input$Nutrition4 == 'SFA 16:0 (Hexadecanoic) (gm)'){column_name11 <- full_nutrition_data$Hexadecanoic}
    if (input$Nutrition4 == 'SFA 18:0 (Octadecanoic) (gm)'){column_name11 <- full_nutrition_data$Octadecanoic}
    if (input$Nutrition4 == 'MFA 16:1 (Hexadecenoic) (gm)'){column_name11 <- full_nutrition_data$Hexadecenoic}
    if (input$Nutrition4 == 'MFA 18:1 (Octadecenoic) (gm)'){column_name11 <- full_nutrition_data$Octadecenoic}
    if (input$Nutrition4 == 'MFA 20:1 (Eicosenoic) (gm)'){column_name11 <- full_nutrition_data$Eicosenoic}
    if (input$Nutrition4 == 'MFA 22:1 (Docosenoic) (gm)'){column_name11 <- full_nutrition_data$Docosenoic}
    if (input$Nutrition4 == 'PFA 18:2 (Octadecadienoic) (gm)'){column_name11 <- full_nutrition_data$Octadecadienoic}
    if (input$Nutrition4 == 'PFA 18:3 (Octadecatrienoic) (gm)'){column_name11 <- full_nutrition_data$Octadecatrienoic}
    if (input$Nutrition4 == 'PFA 18:4 (Octadecatetraenoic) (gm)'){column_name11 <- full_nutrition_data$Octadecatetraenoic}
    if (input$Nutrition4 == 'PFA 20:4 (Eicosatetraenoic) (gm)'){column_name11 <- full_nutrition_data$Eicosatetraenoic}
    if (input$Nutrition4 == 'PFA 20:5 (Eicosapentaenoic) (gm)'){column_name11 <- full_nutrition_data$Eicosapentaenoic}
    if (input$Nutrition4 == 'PFA 22:5 (Docosapentaenoic) (gm)'){column_name11 <- full_nutrition_data$Docosapentaenoic}
    if (input$Nutrition4 == 'PFA 22:6 (Docosahexaenoic) (gm)'){column_name11 <- full_nutrition_data$Docosahexaenoic}
    if (input$Nutrition4 == 'Plain water (gm)'){column_name11 <- full_nutrition_data$PlainWater}
    if (input$Nutrition4 == 'Tap water (gm)'){column_name11 <- full_nutrition_data$TapWater}
    
    full_nutrition_data <- cbind(full_nutrition_data, column_name11)    
        
    full_nutrition_data %>%
      filter(RIDAGEYR >= 2) %>%
      select(column_name9, column_name10, column_name11) %>%
      drop_na%>%
      ggplot(aes(x=column_name10, y = column_name11))+
      geom_point(aes(x=column_name10, y = column_name11, color = column_name9), alpha = 1/2)+
      geom_smooth(aes(column_name10, column_name11))+
      theme_light()+
      scale_colour_simpsons()+
      labs(
        title = "United States Daily Nutrition Scatterplot",
        caption = "NHANES 2001-2016 survey",
        x = input$Nutrition3,
        y = input$Nutrition4,
        color = input$Demographic4
      )
  })  
  
}

shinyApp(ui = ui, server = server)