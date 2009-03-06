all:
	cd /home/alex/proj/hevolisa/ &&\
	cabal configure &&\
	cabal build &&\
	./dist/build/hevolisa/hevolisa -f mona_lisa_crop.png
