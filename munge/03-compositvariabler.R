# Förbered kompisitvariabler som ev kan ersätta enskilda diagnoser

# Fulfix!
# Fel i coder-paketet döpte "malignancy" till "malingnancy"
# Ska funka korrekt med senaste coder-version men vill inte köra om just nu!
df <- rename(df, CCI_malignancy = CCI_malingnancy)
df_ignore_fixation <- rename(df_ignore_fixation, CCI_malignancy = CCI_malingnancy)


# Make mutate statments from Excel-file -----------------------------------

# Define it!
categorization <-
  categorization.Blad1 %>%
  mutate_all(zoo::na.locf) %>%
  mutate_at(vars(new, old), ~ gsub("/| ", "_", tolower(.))) %>%
  mutate(
    new = paste0("c_", new),
  ) %>%
  unite("old", from, old) %>%
  group_by(new) %>%
  summarise(old = paste(old, collapse = " | ")) %>%
  ungroup() %>%
  mutate(
    new = syms(new),
    old = parse_exprs(old)
  )

cache("categorization")

# Do it!
c_cols <- bind_cols(pmap(categorization, ~transmute(df, !!.x := !!.y)))
c_cols_ignore_fixation <-
  bind_cols(pmap(categorization, ~transmute(df_ignore_fixation, !!.x := !!.y)))


# Combine some existing columns with new ones --------------------------------

comb <- function(df, c_cols) {
  df %>%
  select(
    everything(),
    -matches("^([CE]CI)|(Rx)_"),
    matches("index")
  ) %>%
  bind_cols(c_cols)

}

df <- comb(df, c_cols)
cache("df")

df_ignore_fixation <- comb(df_ignore_fixation, c_cols_ignore_fixation)
cache("df_ignore_fixation")

