library(readxl)
X2021data <- read_excel("C:/Users/Utente/Desktop/2021data.xlsx")
View(X2021data)


library(dplyr)
library(stringr)
library(tidyverse)
library(tidytext)
library(wordcloud2)


original_data1 <- X2021data %>%
  group_by(`WHO CODE`) %>%
  mutate(line = row_number()) %>%  
  ungroup()

original_data1


tidy_data <- original_data1  %>%
  unnest_tokens(word, `TEXT TX`)

tidy_data


data(stop_words)

tidy_data <- tidy_data %>%
  anti_join(stop_words)



tidy_data %>% 
  count(word, sort = TRUE) 



library(ggplot2)

tidy_data %>%
  count(word, sort = TRUE) %>%
  filter(n > 10000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)



library(tidyr)

frequency <- bind_rows(mutate(tidy_data, `WHO CODE` = "06 Diseases of the nervous system"),
                       mutate(tidy_data, `WHO CODE` = "12 Diseases of the skin and subcutaneous tissue"), 
                       mutate(tidy_data, `WHO CODE` = "10 Diseases of the respiratory system")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(`WHO CODE`, word) %>%
  group_by(`WHO CODE`) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = `WHO CODE`, values_from = proportion) %>%
  pivot_longer(`06 Diseases of the nervous system`:`12 Diseases of the skin and subcutaneous tissue`,
               names_to = "WHO CODE", values_to = "proportion")

frequency


library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `10 Diseases of the respiratory system`, 
                      color = abs(`10 Diseases of the respiratory system` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~`WHO CODE`, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "10 Diseases of the respiratory system", x = NULL)

