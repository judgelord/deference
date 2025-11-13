library(haven)
library(tidyverse)

# library(memisc)

file <- here::here("data", "ChevDataSynataxDec20042008", "ChevDataSynataxDec20042008", "basicinfo3.sav")

# d <- file |> memisc::spss.file() |> memisc::as_haven()

d <- haven::read_sav(file)

basicinfo3 <- d

save(basicinfo3, file =  here::here("data", "basicinfo3.rda"))
write.csv(basicinfo3, file = here::here("data", "basicinfo3.csv"))

# votes
file <- here::here("data", "ChevDataSynataxDec20042008", "ChevDataSynataxDec20042008", "votes.sav")
d <- haven::read_sav(file)


d1 <- d |>
  select(-JUST_ID, -JUSTICE, -VOTEDEF, -VOTEDIR, -ATTITUDE, -contains("IDEO"), -contains("ONCT")) |>
  distinct()

dup <- d1 |>
  # split cases
  select(-APASECTN, -ISSUE_NU, -APAMENTI, -contains("MAJ")) |>
  add_count(CASE_ID, sort = T) |>
  filter(n>1)

votes <- d
save(votes, file =  here::here("data", "votes.rda"))
write.csv(votes, file = here::here("data", "votes.csv"))

votes_case_level <- d1
save(votes_case_level , file =  here::here("data", "votes_case_level.rda"))
write.csv(votes_case_level , file = here::here("data", "votes_case_level.csv"))


ChevData <- left_join(basicinfo3, votes_case_level)
save(ChevData , file =  here::here("data", "ChevData.rda"))
write.csv(ChevData , file = here::here("data", "ChevData.csv"))

