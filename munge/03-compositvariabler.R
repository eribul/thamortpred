# Förbered kompisitvariabler som ev kan ersätta enskilda diagnoser

# Fulfix!
# Fel i coder-paketet döpte "malignancy" till "malingnancy"
# Ska funka korrekt med senaste coder-version men vill inte köra om just nu!
df <- rename(df, CCI_malignancy = CCI_malingnancy)


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

# Do it!
c_cols <- bind_cols(pmap(categorization, ~transmute(df, !!.x := !!.y)))


# Combine some existing columns with new ones --------------------------------


ECI_keep_text <-
  c("hypothyroidism", "coagulopathy", "obesity", "weight loss",
    "fluid electrolyte disorders")
cache("ECI_keep_text")
ECI_keep_vars <- paste0("ECI_", gsub(" ", "_", ECI_keep_text))

df <-
  df %>%
  select(
    everything(),
    -matches("^([CE]CI)|(Rx)_"),
    one_of(ECI_keep_vars),
    matches("index")
  ) %>%
  bind_cols(c_cols)

cache("df")
