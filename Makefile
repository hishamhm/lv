
all: tests LvInterpreter.pdf DemoLvInterpreter.pdf

DemoLvInterpreter.pdf: DemoLvInterpreter.tex
	pdflatex DemoLvInterpreter.tex

DemoLvInterpreter.tex: DemoLvInterpreter.lhs Lv_format.lhs demo/*.hs
	cpp -traditional DemoLvInterpreter.lhs | grep -v "^#" | ~/.cabal/bin/lhs2TeX  > DemoLvInterpreter.tex

LvInterpreter.pdf: LvInterpreter.tex
	pdflatex LvInterpreter.tex

LvInterpreter.tex: LvInterpreter.in.lhs Lv_format.lhs
	rm -f LvInterpreter.lhs
	lua cut_comments.lua --lhs2tex < LvInterpreter.in.lhs > LvInterpreter.lhs
	~/.cabal/bin/lhs2TeX LvInterpreter.lhs > LvInterpreter.tex; err=$$?; rm LvInterpreter.lhs; exit $$err

DemoLvInterpreter: DemoLvInterpreter.lhs LvInterpreter.in.lhs
	rm -f LvInterpreter.lhs
	cp LvInterpreter.in.lhs LvInterpreter.lhs
	rm -f LvInterpreter.o LvInterpreter.hi DemoLvInterpreter.o DemoLvInterpreter.hi
	ghc -o DemoLvInterpreter -I. -cpp DemoLvInterpreter.lhs; err=$$?; rm LvInterpreter.lhs; exit $$err

DemoLvInterpreter_nodebug: DemoLvInterpreter.lhs LvInterpreter.in.lhs
	rm -f LvInterpreter.lhs
	lua cut_comments.lua < LvInterpreter.in.lhs > LvInterpreter.lhs
	rm -f LvInterpreter.o LvInterpreter.hi DemoLvInterpreter.o DemoLvInterpreter.hi
	ghc -o DemoLvInterpreter -I. -cpp DemoLvInterpreter.lhs; err=$$?; rm LvInterpreter.lhs; exit $$err

lv: lv.hs
	ghc -o lv lv.hs

lhs_lines: DemoLvInterpreter
	./DemoLvInterpreter 2>/dev/null > lhs_lines

hs_lines: LvInterpreter_hs
	./lv 2>/dev/null > hs_lines

tests:
	./run_tests.sh

diff_ok: works.txt lhs_lines
	diff works.txt lhs_lines > diff_ok
	[ `stat -c '%s' diff_ok` = 0 ] || { touch lhs_lines; exit 1; }
#diff_ok: hs_lines lhs_lines
#	diff hs_lines lhs_lines > diff_ok
#	[ `stat -c '%s' diff_ok` = 0 ]

DemoLvInterpreter.mp4: DemoLvInterpreter
	./make_graph.sh DemoLvInterpreter

clean:
	rm -f lv.hi lv.o lv LvInterpreter DemoLvInterpreter
	rm -f graph/graph-*$(FORMAT)
	rm -f graph/graph-*dot
	rm -f LvInterpreter.tex
	rm -f LvInterpreter.pdf DemoLvInterpreter.pdf
