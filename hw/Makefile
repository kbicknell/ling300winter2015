hwbase=hw1 hw2 project
hwhtml=$(addsuffix .html, $(hwbase))
all : $(hwhtml)

.PHONY : all clean

%.html : %.Rmd
	Rscript -e "require(rmarkdown); render('$<')"

clean :
	rm -f $(hwhtml)
