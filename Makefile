
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
	ghc -o DemoLvInterpreter DemoLvInterpreter.lhs

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

FRAMERATE=2
FORMAT=png
FORK=6

graph.mp4: graph/graph-0000001.$(FORMAT)
	rm -f graph.mp4 && ffmpeg -f image2 -framerate $(FRAMERATE) -i graph/graph-%07d.$(FORMAT) -vcodec h264 -qp 0 -preset veryslow graph.mp4

graph/graph-0000001.$(FORMAT): graph/graph-0000001.dot
	frames=$$(ls graph/graph-*.dot | wc -l); \
	time ( \
	for j in `seq 0 $$[$(FORK)-1]`; \
	do ( \
	   for i in $$(seq 1 $(FORK) $$frames); \
	   do \
	      n=`printf "%07.0f" $$[i+j]`; \
	      echo $$n/$$frames; \
	      dot -T$(FORMAT) graph/graph-$$n.dot > graph/graph-$$n.$(FORMAT) || rm graph/graph-$$n.$(FORMAT); \
	   done ) & \
	done; \
	wait; \
	)

graph/graph-0000001.dot: DemoLvInterpreter lv_viewer.lua
	rm -f graph/graph-*dot
	./DemoLvInterpreter 2> /dev/null | lua lv_viewer.lua $(FRAMERATE)
	rm -f graph/graph-*$(FORMAT)

clean:
	rm -f lv.hi lv.o lv LvInterpreter DemoLvInterpreter
	rm -f graph/graph-*$(FORMAT)
	rm -f graph/graph-*dot
	rm -f LvInterpreter.pdf DemoLvInterpreter.pdf
