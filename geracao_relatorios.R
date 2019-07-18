## Packages
library(knitr)
library(rmarkdown)

## Loop
for (i in 1:100){
  rmarkdown::render(input = "relatorio.Rmd",
                    params = list(j = i),
                    output_format = "pdf_document",
                    output_file = paste("relatorio_", i, ".pdf", sep=''),
                    output_dir = "relatorios/")
}

