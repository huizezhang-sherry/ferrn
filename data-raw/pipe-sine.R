## code to prepare `pipe-sine` dataset goes here
library(spinebil) # devtools::install_github("uschiLaa/spinebil")
set.seed(123456)
sine1000 <- spinebil::sinData(6, 1000) %>% scale()
colnames(sine1000) <- paste0("V", 1:6)

set.seed(123456)
sine1000_8d <- spinebil::sinData(8, 1000) %>% scale()
colnames(sine1000_8d) <- paste0("V", 1:8)

set.seed(123456)
pipe1000 <- spinebil::pipeData(6, 1000) %>% scale()
colnames(pipe1000) <- paste0("V", 1:6)

set.seed(123456)
pipe1000_8d <- spinebil::pipeData(8, 1000) %>% scale()
colnames(pipe1000_8d) <- paste0("V", 1:8)

set.seed(123456)
pipe1000_10d <- spinebil::pipeData(10, 1000) %>% scale()
colnames(pipe1000_10d) <- paste0("V", 1:10)

set.seed(123456)
pipe1000_12d <- spinebil::pipeData(12, 1000) %>% scale()
colnames(pipe1000_12d) <- paste0("V", 1:12)

usethis::use_data(sine1000, overwrite = TRUE)
usethis::use_data(sine1000_8d, overwrite = TRUE)
usethis::use_data(pipe1000, overwrite = TRUE)
usethis::use_data(pipe1000_8d, overwrite = TRUE)
usethis::use_data(pipe1000_10d, overwrite = TRUE)
usethis::use_data(pipe1000_12d, overwrite = TRUE)
