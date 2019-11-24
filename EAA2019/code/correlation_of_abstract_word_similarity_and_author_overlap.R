library(magrittr)

#### get the abstract pdf and prepare it for analysis ####
# download at https://www.e-a-a.org/EAA2019/Programme.aspx?WebsiteKey=92bb0c7f-79c0-4998-9804-b5db83f9a8b9&hkey=48f0a584-1ae8-4680-bcef-572c6fe82598&Program=3#Program
raw_pdf <- pdftools::pdf_text("~/Downloads/EAA2019_Abstract Book_22August.pdf")
pdf <- raw_pdf %>% strsplit("\n") %>% unlist %>% trimws

#### get organisers of regular sessions ####
reg_sessions_indizes <- grep("Format: Regular session", pdf)
organiser_indizes <- grep("^Organisers:", pdf)

mapping <- sapply(reg_sessions_indizes, function(x, y) {
    di <- x - y
    ifelse(di < 0, max(di) + 1, di) %>% which.min
  },
  organiser_indizes
)

start_indizes <- organiser_indizes[mapping]
stop_indizes <- reg_sessions_indizes - 1

organisers_indizes <- lapply(
  1:length(start_indizes), function(i) {
    start_indizes[i]:stop_indizes[i]
  }
)

organisers <- lapply(
  organisers_indizes, function(x, y) {
    paste(y[x], collapse = " ")
  },
  pdf
) %>% unlist

organisers_individuals <- lapply(
  organisers, function(x) {
    x %>% gsub("Organisers:", "", .) %>% gsub("\\s*\\([^\\)]+\\)" , "", .) %>% strsplit(" - ") %>% unlist %>% trimws
  }
)

#### measure closeness/overlap of organisers ####
inter_organisers <- function(x, y, z) {
  mapply(
    function(x, y) {
      length(intersect(x, y))
    },
    z[x], 
    z[y]
  )
}

s <- seq_along(organisers_individuals)
close_mat_organisers <- outer(s, s, inter_organisers, organisers_individuals)

#### get abstracts of regular sessions ####
abstracts <- which(pdf == "ABSTRACTS")

mapping <- sapply(reg_sessions_indizes, function(x, y) {
    di <- y - x
    ifelse(di < 0, max(di) + 1, di) %>% which.min
  },
  abstracts
)

start_indizes <- reg_sessions_indizes + 1
stop_indizes <- abstracts[mapping] - 1

abstract_indizes <- lapply(
  1:length(start_indizes), function(i) {
    start_indizes[i]:stop_indizes[i]
  }
)

abstracts <- lapply(
  abstract_indizes, function(x, y) {
    paste(y[x], collapse = " ")
  },
  pdf
) %>% unlist

abstracts_wordlists <- lapply(abstracts, function(x) {
  x %>% 
    strsplit(" ") %>% 
    unlist %>% 
    trimws %>%
    gsub("[^A-Za-z]", "", .) %>%
    tolower() %>%
    unique
    # TODO: Merge broken words
  }
)

#### remove most common words from abstracts ####
most_common_words <- abstracts_wordlists %>% 
  unlist %>%
  tibble::tibble(words = .) %>%
  dplyr::group_by(words) %>%
  dplyr::count() %>%
  dplyr::filter(
    n > round(length(abstracts)*0.3)
  ) %$%
  words

abstracts_wordlists_reduced <- lapply(
  abstracts_wordlists, function(x, y) {
    x[!(x %in% y)]
  }, 
  most_common_words
)

#### measure closeness/overlap of abstracts ####
inter_abstracts <- function(x, y, z) {
  mapply(
    function(x, y) {
      length(intersect(x, y))/mean(c(length(x), length(y)))
    },
    z[x], 
    z[y]
  )
}

s <- seq_along(abstracts_wordlists_reduced)
close_mat_abstracts <- outer(s, s, inter_abstracts, abstracts_wordlists_reduced)

#### distance correlation ####
cma <- close_mat_abstracts
cmo <- close_mat_organisers

cma_long <- reshape2::melt(cma) %>%
  tibble::as_tibble() %>%
  dplyr::arrange(dplyr::desc(value))

cmo_long <- reshape2::melt(cmo) %>%
  tibble::as_tibble() %>%
  dplyr::arrange(dplyr::desc(value))

complete_long <- dplyr::full_join(
  cma_long, cmo_long, by = c("Var1", "Var2"), suffix = c("_cma", "_cmo")
)

# remove autocorrelation  
complete_long <- complete_long %>%
  dplyr::filter(
    value_cma < 0.9
  )

library(ggplot2)
complete_long %>%
  ggplot(aes(x = as.factor(value_cmo), y = value_cma, group = as.factor(value_cmo))) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  xlab("author overlap") +
  ylab("abstract closeness")
