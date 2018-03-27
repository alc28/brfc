# sandbox.R

library(stringr)
x <- c("apple", "banana", "pear")
str_view(x, "an")
str_view(x, ".a.")

dot <- "\\."
writeLines(dot)
str_view(c("abc", "a.c", "bef"), "a\\.c")

str_view(x, "^a")
str_view(x, "a$")

x <- c("apple pie", "apple", "apple cake")
str_view(x, "^apple$")
str_length(x)

str_view(c("grey", "gray"), "gr(e|a)y")

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "C[LX]+")
str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}?")
str_view(x, 'C[LX]+?')

fruit <- c("banana", "coconut", "cucumber", "jujube", "papaya", "salal berry")
str_view(fruit, "(..)\\1", match = TRUE)

x <- c("apple", "banana", "pear")
str_detect(x, "e")
sum(str_detect(words, "^t"))
mean(str_detect(words, "[aeiou]$"))

no_vowels_1 <- !str_detect(words, "[aeiou]")
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")
identical(no_vowels_1, no_vowels_2)

words[str_detect(words, "x$")]
str_subset(words, "x$")

df <- tibble(
  word = words,
  i = seq_along(word)
)

df %>%
  filter(str_detect(words, "x$"))

x <- c("apple", "banana", "pear")
str_count(x, "a")
mean(str_count(words, "[aeiou]"))

df %>%
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )

str_count("abababa", "aba")
str_view_all("abababa", "aba")

data(package = "stringr")
length(sentences)
head(sentences)

colors <- c("red", "orange", "yellow", "green", "blue", "purple")
color_match <- str_c(colors, collapse = "|")

has_color <- str_subset(sentences, color_match)
matches <- str_extract(has_color, color_match)
head(matches)


means <- c(0,1,2)

out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(10,1)
  out[[i]] <- rnorm(n, means[[i]])
}
str(out)


out[3]
str(unlist(out))
purrr::flatten_dbl(out)

