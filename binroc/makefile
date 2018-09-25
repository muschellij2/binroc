all: index.R index.pdf

index.R: index.Rmd
	- Rscript -e "knitr::purl('index.Rmd')"

index.pdf: index.Rmd
	- Rscript -e "rmarkdown::render('index.Rmd')"

clean:
	rm -f index.R index.pdf