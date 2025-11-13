

library(googlesheets4)
library(magrittr)

# old data
url <- "https://docs.google.com/spreadsheets/d/1Ng6_09EiVQJpuPULmN3IJ16Djba85GQM5VXZkV1jbzs"

gs4_auth(email = "devin.jl@gmail.com")

votes2 <- read_sheet(url, col_types = "c")

# inspect
votes2$yrdecid
votes2$rulemake
votes2$agencycd |> unique()

# clean for merge
votes2 %<>%
  mutate(rulemaking = rulemake |> as.numeric(),
         SCOTUS_year = yrdecid |> as.numeric(),
         case = as.character(usreprt),
         agency = str_to_upper(agencycd) |> as.list(),
         rule_comments_n = as.numeric(rulecomments))

votes2$case

votes2$rulecomments |> unique()




####################################

# new data
url <- "https://docs.google.com/spreadsheets/d/10cqoNeo2hQGzhX0zpIn74npS1uHsnkS7qrbHmiEtJyw/edit?gid=0#gid=0"

gs4_auth(email = "devin.jl@gmail.com")

sheet <- read_sheet(url, col_types = "c")

codebook2 <- sheet[1:2,] |>
  mutate_all(as.character) |>
  pivot_longer(everything())


codebook2 |>
  knitr::kable()

write_csv(codebook2, file = here::here("codebook2.csv"))
save(codebook2, file = here::here("codebook2.rda"))


# year
sheet$year
sheet$`Statute Year`
sheet$`Final Rule Year`


sheet %<>%
  drop_na(date) %>%
  mutate(SCOTUS_year = str_extract(date, "[1-2][0-9]{3}") |> as.numeric(),
         statute_year = `Statute Year` |>  str_extract("[1-2][0-9]{3}") |> as.numeric() ,
         rule_year = `Final Rule Year` |>  str_extract("[1-2][0-9]{3}") |> as.numeric())


# clean rulemaking
sheet$`Notice and comment rulemaking`
sheet$rule_comments

sheet %<>%
  mutate(agency = Agency |>
           str_remove_all(".*\\(|\\).*") |>
           str_squish() |>
           str_replace("^NA$", "_None" ) |>
           str_split("\n|;|,|/| and |:| or ") ,
    rulemaking = str_remove(`Notice and comment rulemaking`, "\\?| .*") |> as.numeric(),
    rule_comments_n = rule_comments |>
      str_replace("k$", "000") |>
      str_remove("~|\\+|,|\\.") |>
      as.numeric())

sheet$rulemaking

# Clean case

sheet %<>%
  mutate(case = Case |> str_remove_all(".*,| |\\.") |>
           str_replace("Ct", "US"))

sheet$case



# clean agency
sheet$agency |> unique()
distinct(sheet, rule_comments, rule_comments_n)

sheet |> unnest(agency)




##########################################################################

d <- full_join(votes2, sheet) |> select(SCOTUS_year,statute_year, rule_year, rulemaking, case, agency, rule_comments_n) |> distinct()
d

scotus_data_combined <- d

save(scotus_data_combined, file = here::here("data", "scotus_data_combined.rda"))

