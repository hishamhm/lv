
graph.avi: lv
	./lv 2>&1 | lua lv_viewer.lua
	n=$$(ls graph-*.dot | wc -l); for i in $$(seq -f "%07.0f" $$n); do echo $$i/$$n; dot -Tbmp graph-$$i.dot > graph-$$i.bmp; done
	ffmpeg -f image2 -framerate 25 -i graph-%07d.bmp graph.avi

lv: lv.hs
	ghc -o lv lv.hs

clean:
	rm -f lv.hi lv.o lv
	rm -f graph*bmp
	rm -f graph*dot
	rm -f graph.avi

