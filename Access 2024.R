
################################ TO DO #################################### ####
#                                                                           ----
################################################################################
################################ PRELIMINARIES #################################
################################################################################
# a. Packages ----
# install.packages("readxl")

library(readxl)
library(tidyverse)
#library(ggplot2)
library(car)
#library(dplyr)
library(glmmTMB)
library(performance)
library(MASS)
library(MuMIn)

# b. Data ----
WC_excel <- "WC_AccessibilityPilot.xlsx"
data <- "Participant Data"

d <- read_excel(WC_excel, sheet = data)

d <- d |> dplyr::select(-Name)
d <- drop_na(d) # a few people didn't take post- survey, data can't be used

rm(data, WC_excel)
head(d)
names(d)

#                                                                           ----
################################################################################
######################### DATA CLEANING / PREP ############################ ####
################################################################################
# a. Mean SPIRES / Belonging scores ----
d <- d |>
  # Mean SPIRES score
  mutate(SPIRES_pre = (SPIRES1_pre + SPIRES2_pre + SPIRES3_pre + SPIRES4_pre_RC + 
                              SPIRES5_pre_RC) / 5,
         SPIRES_post = (SPIRES1_post + SPIRES2_post + SPIRES3_post + 
                               SPIRES4_post_RC + SPIRES5_post_RC) / 5) |>
  # Mean Belonging scores
  mutate(Belonging_pre = B_sc_pre + B_sw_pre + B_csc_pre + B_out_pre_RC + 
           B_acc_pre + B_res_pre + B_dis_pre_RC + B_val_pre + B_neg_pre_RC + 
           B_app_pre + B_ex_pre_RC + B_fit_pre + B_insig_pre_RC + B_ease_pre + 
           B_anx_pre_RC + B_comf_pre + B_tense_pre_RC + B_nerve_pre_RC + 
           B_cont_pre + B_calm_pre + B_inad_pre_RC) |>
  mutate(Belonging_post = B_sc_post + B_sw_post + B_csc_post + B_out_post_RC + 
           B_acc_post + B_res_post + B_dis_post_RC + B_val_post + B_neg_post_RC + 
           B_app_post + B_ex_post_RC + B_fit_post + B_insig_post_RC + B_ease_post + 
           B_anx_post_RC + B_comf_post + B_tense_post_RC + B_nerve_post_RC +
           B_cont_post + B_calm_post + B_inad_post_RC)

View(d)

d <- dplyr::select(d, c(ID:Fexp_none, SPIRES_pre, SPIRES_post,
                   Belonging_pre, Belonging_post, PIO_pre, PIO_post))

d <- d |>
  mutate(
    PIO_delta = PIO_post - PIO_pre,
    BEL_delta = Belonging_post - Belonging_pre,
    SPIRES_delta = SPIRES_post - SPIRES_pre) 
  
head(d2)
write.csv(d2, "WC_Access_Data_deltas.csv")


# b. Concatenate predictors ----
#     a. gender ----
d$gender_sel <- rowSums(d[,3:5])
d_chk <- filter(d, gender_sel >1) # 0
rm(d_chk)

d$Gender <- as.factor(case_when(
  d$Gender_man == 1 ~ "Man",
  d$Gender_woman == 1 ~ "Woman",
  d$Gender_NB ==1 ~ "Nonbinary"))

d <- dplyr::select(d, -"gender_sel")

#     b. lgbt ----
d <- d |> mutate(LGBT = ifelse(LGBT == 1, "LGBT", "Non-LGBT"))
d$LGBT <- as.factor(d$LGBT)


d2 <- dplyr::select(d, c(ID, Session, SPIRES_pre:Gender, LGBT))
#     c. prior field experience ----

d_fexp <- d %>%
  pivot_longer(cols = starts_with("Fexp_"), names_to = "Fexp", values_to = "Fexp_value") |>
  mutate(Fexp = recode_factor(Fexp, 
                              "Fexp_Vol" = "Field Exp: Volunteer", 
                              "Fexp_PS" = "Field Exp: ProfScientist", 
                              "Fexp_K12" = "Field Exp: K12",
                              "Fexp_Col" = "Field Exp: College",
                              "Fexp_nf" = "Field Exp: nf", 
                              "Fexp_none" = "Field Exp: None")) |>
  dplyr::select(ID, Fexp, Fexp_value)

#     d. race ----
d_race <- d |>
  pivot_longer(cols = starts_with("Race_"), names_to = "Race", values_to = "R_value") %>%
  mutate(Race = recode_factor(Race, "Race_W" = "Race: White", "Race_B" = "Race: Black", 
                              "Race_N" = "Race: Native American", "Race_A" = "Race: Asian", 
                              "Race_H" = "Race: Hispanic")) %>%
  dplyr::select(ID, Race, R_value)
#     e. disability ----
d_dis <- d |>
  pivot_longer(cols = starts_with("D_"), names_to = "Disability", values_to = "d_value") %>%
  mutate(Disability = recode_factor(Disability, 
                                    "D_phy" = "Disability: Physical", 
                                    "D_Men" = "Disability: Mental", 
                                    "D_Sen" = "Disability: Sensory", 
                                    "D_Learn" = "Disability: Learning", 
                                    "D_Chron" = "Disability: Chronic")) %>%
  dplyr::select(ID, Disability, d_value)

#     f. final concatenation ----
d2 <- d2 |>
  right_join(d_race, by = c("ID")) |>
  right_join(d_dis, by = c("ID")) |>
  right_join(d_fexp, by = c("ID")) 

d2 <- d2 |>
  filter(Fexp_value >0, d_value >0, R_value >0)

d2 <- d2 |>
  dplyr::select(-c(Fexp_value, d_value, R_value))

rm(d_race, d_dis, d_fexp)

d2$Session <- as.factor(d2$Session)
d2$ID <- as.factor(d2$ID)
#                                                                           ----
# b. BOOLEAN VER. OF DATASET ----
d3 <- d2

d3$Gender_man <- as.factor(d3$Gender_man)
d3$Gender_woman <- as.factor(d3$Gender_woman)
d3$Gender_NB <- as.factor(d3$Gender_NB)
d3$Race_W <- as.factor(d3$Race_W)
d3$Race_B <- as.factor(d3$Race_B)
d3$Race_N <- as.factor(d3$Race_N)
d3$Race_A <- as.factor(d3$Race_A)
d3$Race_H <- as.factor(d3$Race_H)
d3$LGBT <- as.factor(d3$LGBT)
d3$D_phy <- as.factor(d3$D_phy)
d3$D_Men <- as.factor(d3$D_Men)
d3$D_Sen <- as.factor(d3$D_Sen)
d3$D_Learn <- as.factor(d3$D_Learn)
d3$D_Chron <- as.factor(d3$D_Chron)
d3$Session <- as.factor(d3$Session)

  
################################################################################
##################### LINEAR MODELS (INCOMPLETE) ############################### ----

################################################################################
# a. SPIRES ----
m_SPIRES <- glmmTMB(data = d4, SPIRES_delta ~ Gender + Race + LGBT + Disability + Fexp + (1|Session), na.action = na.fail)

# model selection
ms_SPIRES<- stepAIC(m_SPIRES, scope = . ~ ., direction = "both", trace = TRUE)
summary(ms_SPIRES)


dredge <- dredge(m_SPIRES)
dredge # output results

# final model: AIC = 65.79
m_SPIRES <- glmmTMB(data = d3, SPIRES_delta ~ Gender_man + Gender_woman + LGBT + (1|Session))

check_collinearity(m_SPIRES, component = "conditional") # check for multicollinearity
check_overdispersion(m_SPIRES) # check for overdispersion - looks good

# summarize model output
Anova(m_SPIRES, type="III")
summary(m_SPIRES)

# b. PIO ----
m_PIO <- glmmTMB(data = d3, PIO_delta ~ Gender_man + Gender_woman + Gender_NB + Race_W +
                      Race_B + Race_N + Race_A + Race_H + LGBT + D_phy + D_Men + D_Sen + 
                      D_Learn + D_Chron + (1|Session), na.action = na.fail)

# model selection
ms_PIO<- stepAIC(m_PIO, scope = . ~ ., direction = "both", trace = TRUE)
summary(ms_PIO)

dredge_PIO <- dredge(m_PIO)
dredge_PIO # output results

# final model: AIC = 145.2
m_PIO <- glmmTMB(data = d3, PIO_delta ~ Gender_man + Gender_woman + Race_A + D_Chron + (1|Session))

check_collinearity(m_PIO, component = "conditional") # check for multicollinearity
check_overdispersion(m_PIO) # check for overdispersion - looks good

# summarize model output
Anova(m_PIO, type="III")
summary(m_PIO)

# c. BEL ----
m_BEL <- glmmTMB(data = d3, BEL_delta ~ Gender_man + Gender_woman + Gender_NB + Race_W +
                      Race_B + Race_N + Race_A + Race_H + LGBT + D_phy + D_Men + D_Sen + 
                      D_Learn + D_Chron + (1|Session), na.action = na.fail)

# model selection
ms_BEL<- stepAIC(m_BEL, scope = . ~ ., direction = "both", trace = TRUE)
summary(ms_BEL)

dredge_BEL <- dredge(m_BEL)
dredge_BEL # output results

# AIC = 65.79
m_BEL <- glmmTMB(data = d3, BEL_delta ~ Gender_man + Gender_woman + Race_N + Race_A + LGBT + 
                      D_Men + D_Sen + (1|Session))

check_collinearity(m_BEL, component = "conditional") # check for multicollinearity
check_overdispersion(m_BEL) # check for overdispersion - looks good

# summarize model output
Anova(m_BEL, type="III")
summary(m_BEL)



#                                                                           ----
########################## EXPLORATORY ANALYSIS ########################### ####
# How many disabilities did participants report? ----
d$D_total <- rowSums(d[c("D_phy", "D_Men", "D_Sen", "D_Learn", "D_Chron")])

range(d$D_total) # participants reported having 1-4 disability types
mean(d$D_total) # average of 1.58 disability types reported
median(d$D_total) # median of 1 reported

nrow(d) # 38 total participants
nrow(d[d$D_total>1,]) # 16 participants reported having more than 1 dis. type
nrow(d[d$D_total>2,]) # 5 reported having more than 2 dis. types
nrow(d[d$D_total>3,]) # 1 reported having more than 3 dis. types

disabilities <- apply(d[c("D_phy", "D_Men", "D_Sen", "D_Learn", "D_Chron")], MARGIN = 2, sum)
# most common disability types reported were mental disabilities (n = 15), 
# chronic (n = 14), physical (n=12), sensory (n = 11), and learning (n = 8) 

# disability type cooccurrences
nrow(d[d$D_phy == 1 & d$D_Men == 1, ]) #4
nrow(d[d$D_phy == 1 & d$D_Sen == 1, ]) #2
nrow(d[d$D_phy == 1 & d$D_Learn == 1, ]) #1
nrow(d[d$D_Chron == 1 & d$D_phy == 1, ]) #6 - most common
nrow(d[d$D_Men == 1 & d$D_Sen == 1, ]) #1
nrow(d[d$D_Men == 1 & d$D_Learn == 1, ]) #4
nrow(d[d$D_Chron == 1 & d$D_Men == 1, ]) #5
nrow(d[d$D_Sen == 1 & d$D_Learn == 1, ]) #1
nrow(d[d$D_Chron == 1 & d$D_Sen == 1, ]) #3
nrow(d[d$D_Chron == 1 & d$D_Learn == 1, ]) #2

# Mean score table ----

# mean score table by demographic gropus with pre/post measures
g_means <- d2 |> group_by(Gender) |>
  summarize(across(.cols = 3:11, .fns = mean))|> ungroup()|> rename(Demographic = Gender)

LGBT_means <- d2 |> group_by(LGBT) |>
  summarize(across(.cols = 3:11, mean))|> ungroup() |> rename(Demographic = LGBT)

r_means <- d2 |> group_by(Race) |>
  summarize(across(.cols = 3:11, mean))|> ungroup() |> rename(Demographic = Race)

d_means <- d2 |> group_by(Disability) |>
  summarize(across(.cols = 3:11, mean))|> ungroup() |> rename(Demographic = Disability)

fexp_means <- d2 |> group_by(Fexp) |>
  summarize(across(.cols = 3:11, mean))|> ungroup() |> rename(Demographic = Fexp)

Means_table<-rbind(g_means, LGBT_means, r_means, d_means, fexp_means)
rm(g_means, LGBT_means, r_means, d_means, fexp_means)

Means_table[,2:10] <- round(Means_table[,2:10], digits = 2) 
Means_table <- dplyr::select(Means_table, c(Demographic, SPIRES_pre, SPIRES_post,
                                            SPIRES_delta, Belonging_pre, Belonging_post, BEL_delta, 
                                            PIO_pre, PIO_post, PIO_delta))
tibble(Means_table)
gtsummary(Means_table)
write.csv(Means_table, file = "Means_table.csv")

# Graphs ----
#     preliminaries ----

# relevel
d2_long <- d2 |> group_by(ID, Session, Disability) |>
  pivot_longer(cols = c(3:4,11), names_to = "SPIRES_type", values_to = "SPIRES") |>
  mutate(SPIRES_type = as.factor(SPIRES_type))

d2_long$SPIRES_type <- fct_relevel(d2_long$SPIRES_type, c("SPIRES_pre", "SPIRES_post", 
                                                          "SPIRES_delta"))
  pivot_longer(cols = starts_with("PIO_"), names_to = "PIO_Type", values_to = "PIO")

  # Graphs: SPIRES by demographic ----
#        pre/post ----
ggplot(subset(d2_long,SPIRES_type != "SPIRES_delta")) +
                geom_boxplot(aes(Disability, SPIRES, fill = SPIRES_type))

ggplot(subset(d2_long,SPIRES_type != "SPIRES_delta")) +
  geom_boxplot(aes(Race, SPIRES, fill = SPIRES_type))

ggplot(subset(d2_long,SPIRES_type != "SPIRES_delta")) +
  geom_boxplot(aes(Gender, SPIRES, fill = SPIRES_type))

ggplot(subset(d2_long,SPIRES_type != "SPIRES_delta")) +
  geom_boxplot(aes(LGBT, SPIRES, fill = SPIRES_type))

#        delta ----

ggplot(subset(d2_long,SPIRES_type == "SPIRES_delta")) +
  geom_boxplot(aes(Disability, SPIRES, fill = SPIRES_type))

ggplot(subset(d2_long,SPIRES_type == "SPIRES_delta")) +
  geom_boxplot(aes(Gender, SPIRES, fill = SPIRES_type))