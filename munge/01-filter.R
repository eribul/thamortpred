
N <- function() c(n_distinct(df_shpr$LopNr), nrow(df_shpr))                       # pats   hips

# Endast 2008-2015 enligt data/linkage.R

# Endast primäroperationer - preselektion
df_shpr <-
  df_shpr_orig %>%
  distinct(LopNr, P_Side, .keep_all = TRUE)

# Endats THA
df_shpr <- filter(df_shpr, P_ProstType == "Totalprotes"); N()                     # 112818 127671

# Endast OA
df_shpr <- filter(df_shpr, P_DiaGrp == "Primär artros"); N()                      # 90137 102698


# Indikera opnr och tid mellan operationer för de som har två
df_shpr <-
  df_shpr %>%
  group_by(LopNr) %>%
  mutate(
    bilateral = any(opnr > 1),
    time_between = if_else(!bilateral, NA_integer_,
                           as.integer(max(P_SurgDate) - min(P_SurgDate))
    )
  ) %>%
  ungroup()

# Endast första höft
df_shpr <- filter(df_shpr, opnr == 1); N()                                       # 80805

# Ignorera om ytterligare THA inom 90 dagar efter första
df_shpr <- filter(df_shpr, is.na(time_between) | time_between > 90); N()         # 79888

# Endast cementfria
df_shpr <- filter(df_shpr,
                  P_AcetCupCemMix != "Cementfritt",
                  P_FemStemCemMix != "Cementfritt"); N()                          # 53718

df_shpr <- filter(df_shpr, is.na(P_KVA1) | P_KVA1 != "NFB62 - Primär total ytersättningspr"); N() # 53718
df_shpr <- filter(df_shpr, between(P_Age, 18, 100)); N()                          # 53718

df_shpr <- filter(df_shpr, P_BMI <= 50); N()                                      # 50761
df_shpr <- filter(df_shpr, P_ASA <= 5); N()                                       # 50152
df_shpr <- filter(df_shpr, !is.na(education)); N()                                # 49804
df_shpr <- filter(df_shpr, !is.na(civil_status)); N()                             # 49804
df_shpr <- filter(df_shpr, !is.na(P_TypeOfHospital)); N()                         # 49427

# Indikerar nya data för ny peroid
df_shpr <- mutate(df_shpr, new = P_SurgDate >= "2013-01-01")

cache("df_shpr")
