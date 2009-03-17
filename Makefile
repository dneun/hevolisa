SRC=/home/scvalex/proj/hevolisa

all:
	cd $(SRC) &&\
	cabal configure &&\
	cabal build &&\
	./dist/build/hevolisa/hevolisa -f wana.png

clean:
	cd $(SRC) &&\
	cabal clean

