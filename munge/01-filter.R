
N <- function() c(n_distinct(df_shpr$LopNr), nrow(df_shpr))                       # pats   hips

# Endast prim�roperationer
df_shpr <-
  df_shpr_orig %>%
  distinct(LopNr, P_Side, .keep_all = TRUE); N()                                  # 146159 162951

# Endast OA
df_shpr <- filter(df_shpr, P_DiaGrp == "Prim�r artros"); N()                      # 90173 102734

# Indikera opnr och tid mellan operationer f�r de som har tv�
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

# Endast f�rsta h�ft
df_shpr <- filter(df_shpr, opnr == 1); N()                                       # 77105 77105

# Ignorera om ytterligare THA inom ett �r efter f�rsta
df_shpr <- filter(df_shpr, is.na(time_between) | time_between > 365); N()         # 70959

df_shpr <- filter(df_shpr, P_KVA1 != "NFB62 - Prim�r total yters�ttningspr"); N() # 70959
df_shpr <- filter(df_shpr, between(P_Age, 18, 100)); N()                          # 70959
df_shpr <- filter(df_shpr, P_BMI <= 50); N()                                      # 67382
df_shpr <- filter(df_shpr, P_ASA <= 3); N()                                       # 66387
df_shpr <- filter(df_shpr, !is.na(education)); N()                                # 65925

# INdikerar nya data f�r nyu peroid
df_shpr <- mutate(df_shpr, new = P_SurgDate >= "2013-01-01")

cache("df_shpr")