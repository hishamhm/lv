
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
	./DemoLvInterpreter 2>/dev/null > lhs_lines

hs_lines: LvInterpreter_hs
	./lv 2>/dev/null > hs_lines

diff_ok: works.txt lhs_lines
	diff works.txt lhs_lines > diff_ok
	[ `stat -c '%s' diff_ok` = 0 ] || { touch lhs_lines; exit 1; }
#diff_ok: hs_lines lhs_lines
#	diff hs_lines lhs_lines > diff_ok
#	[ `stat -c '%s' diff_ok` = 0 ]

graph.avi: graph/graph-0000001.png
	rm -f graph.avi && ffmpeg -f image2 -framerate 10 -i graph/graph-%07d.png graph.avi

graph/graph-0000001.png: graph/graph-0000001.dot
	n=$$(ls graph/graph-*.dot | wc -l); for i in $$(seq -f "%07.0f" $$n); do echo $$i/$$n; dot -Tpng graph/graph-$$i.dot > graph/graph-$$i.png; done

graph/graph-0000001.dot: DemoLvInterpreter lv_viewer.lua
	./DemoLvInterpreter 2> /dev/null | lua lv_viewer.lua

clean:
	rm -f lv.hi lv.o lv LvInterpreter DemoLvInterpreter
	rm -f graph/graph-*png
	rm -f graph/graph-*dot
	rm -f LvInterpreter.pdf DemoLvInterpreter.pdf
