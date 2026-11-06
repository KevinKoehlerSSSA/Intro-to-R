# =============================================================================
# ANNOTATED COPY of 01_build_conflict_income.R (Lab 2, sent 3 November 2026)
#
# Lines starting with  # >>  are my comments. Everything else is your code,
# unchanged. I ran the script on the course data (see note in section 1).
#
# THE SHORT VERSION
#   + The script runs, is well commented, and checks its own work: it counts
#     the rows after every step and asks which countries found no code and
#     which rows have no GDP. That is exactly the habit we want.
#   - But the table it builds cannot test the claim. It contains ONLY
#     country-years WITH an internal conflict (1,094 rows, 83 countries).
#     Peaceful country-years are missing, so there is nothing to compare
#     the conflict years with. (Lab sheet, Step 3, question 4.)
#   - The cause is in the prompt, not in the code: "Keep only those
#     observations that are classified as type_of_conflict = 3 or 4" was
#     applied to the FINAL table, not only to the UCDP file. The assistant
#     did what you wrote, not what you meant.
# =============================================================================

# Lab 2: conflict and income in one country-year table
#
# What this script does:
#   1. reads the UCDP/PRIO armed conflict data and the World Bank GDP data
#   2. keeps only the internal conflicts (type 3 and 4) of the years 1990-2025
#   3. turns the GDP file from wide (one row per country) to long (one row
#      per country-year)
#   4. joins the two files on the country code and the year
#   5. writes the result to output/conflict_income.csv

# >> Good: a plain-language summary at the top. Notice what it does NOT say:
# >> nowhere does it say which country-years end up in the final table.
# >> Step 4 says "joins the two files", but the join below starts from the
# >> conflict table, so the conflict table decides which rows survive.

library(tidyverse)

# The folder where the result is written
dir.create("output", showWarnings = FALSE)


# ---- 1. Read the two data files ----

# The conflict data: one row per conflict per year (a conflict between two sides).
# gwno_loc is read as text because it is a country code, not a number to compute with.
ucdp <- read_csv("data/UcdpPrioConflict_v26_1.csv",
                 col_types = cols(gwno_loc = col_character()))
nrow(ucdp)

# >> 2,816 rows. Correct: one row = one conflict in one year.
# >> Reading gwno_loc as text is a sensible choice. (In some UCDP rows the
# >> location holds two codes, "651, 666", which only text can store. None
# >> of those are internal conflicts, so it does not matter here.)
# >> read_csv() prints a "parsing issues" warning for this file. Look at it
# >> with problems(ucdp) before you ignore it.

# The GDP data: one row per country, one column per year.
# The first 4 lines of the file are notes about the download, so we skip them.
gdp <- read_csv("data/API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_1247.csv", skip = 4)
nrow(gdp)
ncol(gdp)

# >> Which GDP? PP.CD = GDP per capita, PPP, CURRENT international dollars.
# >> PPP is the right family for comparing countries. "Current" means the
# >> numbers are not adjusted for inflation, so 1995 and 2020 values are not
# >> on the same scale. That is fine if you compare countries WITHIN a year
# >> (e.g. income quartiles year by year); it is not fine if you compare
# >> income levels ACROSS years. The constant-price version is PP.KD.
# >> Write down which one you chose and why (lab sheet, Step 1).
# >>
# >> (I did not have your PP.CD file, so I ran the script on the course's
# >> PP.KD file. Same layout; the row counts below come from that run.)
# >>
# >> 266 rows: not all of them are countries. The World Bank file also has
# >> regions and income groups ("Arab World", "Low income", "World"...).
# >> Here that does no harm, because only conflict countries are kept. In a
# >> table of ALL countries you would have to remove them.
# >> 71 columns: the last one is empty (the file ends every line with a
# >> comma). read_csv() names it ...71 and tells you so in the Console.


# ---- 2. Keep the years 1990 to 2025 ----

ucdp_recent <- ucdp %>%
  filter(year >= 1990 & year <= 2025)
nrow(ucdp_recent)

# >> 1,609 rows. Fine. Is 1990 "the end of the Cold War"? 1989 or 1992 are
# >> also defensible. It is a choice; say why you made it.


# ---- 3. Keep only the internal conflicts ----

# type_of_conflict: 1 = extrasystemic, 2 = interstate, 3 = internal,
# 4 = internationalized internal (see data/ucdp-prio-acd-261.pdf)
ucdp_internal <- ucdp_recent %>%
  filter(type_of_conflict %in% c(3, 4))
nrow(ucdp_internal)

# >> 1,553 rows. Correct, and good that the comment cites the codebook.


# ---- 4. Make one row per country and year ----

# The same country-year can hold several conflicts (several sides fighting),
# so we summarise the conflict records into one row per country-year.
conflict_country_year <- ucdp_internal %>%
  group_by(gwno_loc, location, year) %>%
  summarise(
    conflict_type = paste(sort(unique(type_of_conflict)), collapse = "+"),
    n_conflicts = n(),
    intensity_level = max(intensity_level),
    .groups = "drop"
  )
nrow(conflict_country_year)

# >> 1,094 rows. This aggregation step is correct and well explained:
# >> before it, one row = one conflict-year; after it, one row = one
# >> country-year. n_conflicts and max(intensity_level) are good choices.
# >>
# >> One thing to think about: conflict_type stores values like "3+4" in
# >> 42 rows. That is TWO values in ONE cell, which breaks the third rule
# >> of tidy data. If you need the information, two 0/1 columns
# >> (any_internal, any_internationalized) would be tidier.
# >>
# >> And the main point: this is the moment where the table becomes
# >> "country-years WITH conflict". A country-year with no conflict has
# >> no row in UCDP, so it has no row here, and it never comes back.

# How many different countries are in the table?
conflict_country_year %>% distinct(gwno_loc) %>% nrow()

# >> 83 countries.


# ---- 5. Match the UCDP countries to the World Bank country codes ----

# The two files use different codes for the same country:
# UCDP uses the G-Wing number (gwno_loc), the World Bank uses three letters.
# We use the country names to match them, and a small table for the names
# that are spelled differently in the two files.

# >> "G-Wing" should be Gleditsch & Ward (G&W). The assistant got the
# >> name of the code system wrong in a comment. Small, but it shows that
# >> comments can be wrong too.
# >>
# >> The method: match on NAMES first, then fix the failures by hand. That
# >> works here, but it is fragile: names change ("Turkey" / "Turkiye")
# >> and are spelled differently in every dataset. The safer route is to
# >> match on CODES: the countrycode package converts G&W numbers to
# >> ISO-3 letters directly, e.g.
# >>   countrycode(conflict_country_year$gwno_loc, "gwn", "iso3c")
# >> and then you only fix the few codes it cannot convert.

wb_countries <- gdp %>%
  select(country_code = `Country Code`, country_name = `Country Name`)

different_names <- tribble(
  ~gwno_loc, ~location,         ~manual_code,
  "101", "Venezuela",           "VEN",
  "2",   "United States of America", "USA",
  "345", "Serbia (Yugoslavia)", "SRB",
  "346", "Bosnia-Herzegovina", "BIH",
  "365", "Russia (Soviet Union)", "RUS",
  "437", "Ivory Coast",         "CIV",
  "484", "Congo",               "COG",
  "490", "DR Congo (Zaire)",    "COD",
  "520", "Somalia",             "SOM",
  "630", "Iran",                "IRN",
  "640", "Turkey",              "TUR",
  "651", "Egypt",               "EGY",
  "652", "Syria",               "SYR",
  "678", "Yemen (North Yemen)", "YEM",
  "775", "Myanmar (Burma)",     "MMR",
  "811", "Cambodia (Kampuchea)", "KHM",
  "812", "Laos",                "LAO"
)

# >> This hand-made table was typed in by the assistant from its own
# >> "knowledge". I checked the 17 codes and they are right. But you
# >> cannot know that without checking. Any table an assistant types out
# >> of its head is a claim, not data.
# >>
# >> Two of these are judgement calls, not spelling fixes:
# >>   345 "Serbia (Yugoslavia)" -> SRB: Yugoslavia in the 1990s becomes
# >>       today's Serbia. Defensible, but it is a decision.
# >>   678 "Yemen (North Yemen)" -> YEM: unified Yemen since 1990. Fine.

country_bridge <- conflict_country_year %>%
  distinct(gwno_loc, location) %>%
  left_join(wb_countries, by = c("location" = "country_name")) %>%
  left_join(different_names, by = c("gwno_loc", "location")) %>%
  mutate(country_code = ifelse(is.na(country_code), manual_code, country_code))

# How many countries are still without a World Bank code?
sum(is.na(country_bridge$country_code))

# >> 0. Excellent that the script asks this question. This is the single
# >> most important check after any code conversion.
# >> To be complete, also print the bridge (print(country_bridge, n = 100))
# >> and read it: a match can exist and still be WRONG.

# Add the World Bank code to the country-year table
conflict_country_year <- conflict_country_year %>%
  left_join(country_bridge %>% select(gwno_loc, country_code), by = "gwno_loc")
nrow(conflict_country_year)

# >> Still 1,094 rows: the join did not multiply any rows. Good check.


# ---- 6. Turn the GDP file from wide to long ----

# One row per country-year, with the GDP per capita in the gdp_pc_ppp column
gdp_long <- gdp %>%
  select(country_code = `Country Code`, "1990":"2025") %>%
  pivot_longer(cols = "1990":"2025", names_to = "year", values_to = "gdp_pc_ppp") %>%
  mutate(year = as.numeric(year))
nrow(gdp_long)

# >> 9,576 rows = 266 "countries" x 36 years. Correct reshape, and
# >> converting year from text to a number is exactly the post-reshape
# >> check from the lab sheet. Well done.
# >>
# >> Notice: THIS table is what you needed as the starting point. It has
# >> every country in every year, including the peaceful ones.


# ---- 7. Join the GDP values onto the conflict table ----

# The join keeps every conflict country-year; country-years without a GDP
# value simply have a missing value in the gdp_pc_ppp column.
conflict_income <- conflict_country_year %>%
  left_join(gdp_long, by = c("country_code", "year")) %>%
  arrange(country_code, year)
nrow(conflict_income)

# >> HERE IS THE PROBLEM. A left_join() keeps all rows of the LEFT table
# >> (conflict_country_year) and only the matching rows of the right one.
# >> So the result is still 1,094 conflict country-years. The roughly
# >> 5,000 country-years without conflict are dropped.
# >>
# >> With this table you can say "the median conflict country-year had a
# >> GDP per capita of about X". You cannot say whether that is low or
# >> high, because you have nothing to compare it with. Every row has a
# >> conflict, so "conflict" is not a variable here: it is a constant.
# >>
# >> The fix is to turn the join around:
# >>   1. start from gdp_long (all countries, all years), after removing
# >>      the regions and income groups;
# >>   2. left_join() the conflict table onto it;
# >>   3. set conflict = 1 where a conflict row matched and 0 where it did
# >>      not (n_conflicts = 0 instead of NA).
# >> A prompt that would get you there:
# >>   "The final table must contain every country in the World Bank file
# >>    in every year 1990-2025, also the years without conflict. Add a
# >>    column conflict that is 1 if UCDP records an internal conflict
# >>    (type 3 or 4) in that country-year and 0 if not. Remove the World
# >>    Bank rows that are regions or income groups, not countries."

# How many country-years have no GDP value, and which countries are they?
sum(is.na(conflict_income$gdp_pc_ppp))
conflict_income %>%
  filter(is.na(gdp_pc_ppp)) %>%
  count(country_code, location)

# >> 57 rows have no GDP (in my run): Yemen 19, South Sudan 12,
# >> Afghanistan 11, Djibouti 5, Eritrea 3, Syria 3, Serbia 2, Somalia 1,
# >> Venezuela 1. Again: very good that the script asks.
# >> Now look at WHO is missing: some of the most violent conflicts in the
# >> data. Missing GDP is not random; war destroys statistics offices.
# >> Any graph or average that silently drops these rows drops the cases
# >> that matter most for your claim. We will come back to this when we
# >> make graphs (ggplot drops them with only a warning in the Console).


# ---- 8. Save the result ----

write_csv(conflict_income, "output/conflict_income.csv")

# >> Before you go on to Step 5, two more things to decide:
# >>   - Income in the SAME year as the conflict, or the year BEFORE?
# >>     War makes countries poorer. If you use same-year income, part of
# >>     the "poor countries fight" pattern may be "fighting makes poor".
# >>     Lagging income by one year is a simple first answer.
# >>   - What counts as "poorer"? A number (GDP per capita), or groups
# >>     (e.g. four income groups per year)? Decide before you prompt.
