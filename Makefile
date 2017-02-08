
all: diff_ok LvInterpreter.pdf DemoLvInterpreter.pdf

DemoLvInterpreter.pdf: DemoLvInterpreter.tex
	pdflatex DemoLvInterpreter.tex

DemoLvInterpreter.tex: DemoLvInterpreter.lhs Lv_format.lhs
	~/.cabal/bin/lhs2TeX DemoLvInterpreter.lhs > DemoLvInterpreter.tex

LvInterpreter.pdf: LvInterpreter.tex
	pdflatex LvInterpreter.tex

LvInterpreter.tex: LvInterpreter.lhs Lv_format.lhs
	~/.cabal/bin/lhs2TeX LvInterpreter.lhs > LvInterpreter.tex

DemoLvInterpreter: DemoLvInterpreter.lhs LvInterpreter.lhs
	rm -f LvInterpreter.o LvInterpreter.hi DemoLvInterpreter.o DemoLvInterpreter.hi
	ghc -o DemoLvInterpreter -cpp DemoLvInterpreter.lhs

lv: lv.hs
	ghc -o lv lv.hs

lhs_lines: DemoLvInterpreter
	./DemoLvInterpreter 2>&1 | grep "^LvState" > lhs_lines

hs_lines: LvInterpreter_hs
	./lv 2>&1 | grep "^LvState" > hs_lines

diff_ok: works.txt lhs_lines
	diff works.txt lhs_lines > diff_ok
	[ `stat -c '%s' diff_ok` = 0 ]
#diff_ok: hs_lines lhs_lines
#	diff hs_lines lhs_lines > diff_ok
#	[ `stat -c '%s' diff_ok` = 0 ]

graph.avi: lv
	./lv 2>&1 | lua lv_viewer.lua
	n=$$(ls graph-*.dot | wc -l); for i in $$(seq -f "%07.0f" $$n); do echo $$i/$$n; dot -Tbmp graph-$$i.dot > graph-$$i.bmp; done
	ffmpeg -f image2 -framerate 25 -i graph-%07d.bmp graph.avi

clean:
	rm -f lv.hi lv.o lv LvInterpreter DemoLvInterpreter
	rm -f graph*bmp
	rm -f graph*dot
	rm -f graph.avi

