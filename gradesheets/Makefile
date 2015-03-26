# Produce PDFs from all Markdown files in a directory

# List files to be made by finding all *.md files and appending .pdf
PDFS :=	$(patsubst %.md,%.pdf,$(wildcard *.md))

# The all rule makes all the PDF files listed
all : $(PDFS)
                        
# This generic rule accepts PDF targets with corresponding Markdown 
# source, and makes them using pandoc
%.pdf : %.md
	pandoc $< -V geometry:margin=1in -o $@

# Remove all PDF outputs
clean :
	rm $(PDFS)

# Remove all PDF outputs then build them again
rebuild : clean all
