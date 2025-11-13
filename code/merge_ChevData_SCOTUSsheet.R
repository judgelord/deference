

load(here::here("data", "ChevData.rda"))

ChevData <- d


library(googlesheets4)
library(magrittr)

url <- "https://docs.google.com/spreadsheets/d/10cqoNeo2hQGzhX0zpIn74npS1uHsnkS7qrbHmiEtJyw/edit?gid=0#gid=0"

gs4_auth(email = "devin.jl@gmail.com")

sheet <- read_sheet(url, col_types = "c")

codebook2 <- sheet[1:2,] |>
  mutate_all(as.character) |>
  pivot_longer(everything())


codebook2 |>
  knitr::kable()

write_csv(codebook2, file = here::here("codebook2.csv"))


sheet %<>%
  drop_na(date) %>%
  mutate(year = str_extract(date, "[1-2][0-9]{3}") |> as.numeric(),
         statute_year = `Statute Year` |>  str_extract("[1-2][0-9]{3}") |> as.numeric() ,
         rule_year = `Final Rule Year` |>  str_extract("[1-2][0-9]{3}") |> as.numeric())

sheet$year
sheet$`Notice and comment rulemaking`
sheet$`Final Rule Year`

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
sheet$agency |> unique()
distinct(sheet, rule_comments, rule_comments_n)

sheet |> unnest(agency)





ChevData %<>%
  mutate(rulemaking = RULEMAKE |> as.numeric(),
         year = YRDECID,
         Case = as.character( CASE_ID),
         agency = str_to_upper(AGENCYCD) |> as.list())

ChevData$YRDECID
ChevData$RULEMAKE
ChevData$AGENCYCD |> unique()


d <- full_join(ChevData, sheet) |> select(year,statute_year, rule_year, rulemaking, Case, agency, rule_comments_n) |> distinct()
d
