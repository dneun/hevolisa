SRC=/home/scvalex/proj/hevolisa

all:
	cd $(SRC) &&\
	cabal configure &&\
	cabal build &&\
	./dist/build/hevolisa/hevolisa wana.png

clean:
	cd $(SRC) &&\
	cabal clean

