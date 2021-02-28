# Rammon
Hello, I'm SKKU student for studying master degree


  git config --global user.email "you@example.com"
  git config --global user.name "Your Name"
  
install.packages(c("usethis", "remotes"))
remotes::install_github("rstudio/rmarkdown")
install.packages("postcards")

  
library(postcards)
create_postcard()
packageVersion("postcards")
  
usethis::use_github_pages(branch = "main", path = "/")
  
  