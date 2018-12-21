
N <- function() c(n_distinct(df_shpr$LopNr), nrow(df_shpr))                       # pats   hips

# Endast primäroperationer
df_shpr <-
  df_shpr_orig %>%
  distinct(LopNr, P_Side, .keep_all = TRUE); N()                                  # 146159 162951

# Endast OA
df_shpr <- filter(df_shpr, P_DiaGrp == "Primär artros"); N()                      # 90173 102734

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
df_shpr <- filter(df_shpr, opnr == 1); N()                                       # 77105 77105

# Ignorera om ytterligare THA inom ett år efter första
df_shpr <- filter(df_shpr, is.na(time_between) | time_between > 365); N()         # 70959

df_shpr <- filter(df_shpr, P_KVA1 != "NFB62 - Primär total ytersättningspr"); N() # 70959
df_shpr <- filter(df_shpr, between(P_Age, 18, 100)); N()                          # 70959
df_shpr <- filter(df_shpr, P_BMI <= 50); N()                                      # 67382
df_shpr <- filter(df_shpr, P_ASA <= 3); N()                                       # 66387
df_shpr <- filter(df_shpr, !is.na(education)); N()                                # 65925

# INdikerar nya data för nyu peroid
df_shpr <- mutate(df_shpr, new = P_SurgDate >= "2013-01-01")

cache("df_shpr")
