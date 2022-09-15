library("googlesheets4")
library("tidyverse")
url_21 <- "https://docs.google.com/spreadsheets/d/1DqVZ9kTcBoOzebuJ_VmmS68zeJnjRlIrIyJRguIHoVI/edit#gid=0"
url_22 <- "https://docs.google.com/spreadsheets/d/1NGYy3iIHSekAqjfZ6vVN0V444ozSBW2cVG4MlUR1mvU/edit#gid=0"
df <- read_sheet(url_21)
colnames(df) <- c("dog", colnames(df)[-1])
df <- filter(df, !is.na(dog))
start_cols <- c("dog", "side", "total", "week")

total <- df[which(tolower(df$dog) == "total"):nrow(df),]

track <-
  df |> 
  slice_head(n = 1) |> 
  select(-all_of(start_cols))

df <-
  df[-c(1,which(tolower(df$dog) == "total"):nrow(df)),] |> 
  mutate(across(-all_of(start_cols), as.character)) |> 
  pivot_longer(cols = 5:last_col(), names_to = "date", values_to = "run") |> 
  mutate(run = if_else(run == "NULL", NA_character_, run),
         run = str_remove_all(run, "\\(|\\)|\\."))

df <- mutate(df, date = str_sub(date, 1, 10),
             date = lubridate::dmy(date))
df$run |> unique()
filter(df, is.na(date))

df |> 
  group_by(dog) |> 
  arrange(date) |> 
  mutate(c_t = cumsum(!is.na(run)),
         label = if_else(date == max(date), dog, NA_character_)) |> 
  ggplot(aes(x = date, y = c_t, color = dog)) +
  geom_line() +
  geom_text(aes(label = label), nudge_x = 10) +
  theme_minimal() +
  scale_x_date(breaks = scales::pretty_breaks(15))
