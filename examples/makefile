all:hask
	tectonic test.tex
ref:
	pdflatex ref.tex --halt-on-error

hask:
	stack run -- -o test.tex -i john.yaml

clean:
	rm -rf *.aux *.log

clean-all:
	rm -rf *.aux *.log *.pdf
