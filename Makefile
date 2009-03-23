SRC=/home/scvalex/proj/hevolisa

all:
	cd $(SRC) &&\
	cabal configure &&\
	cabal build &&\
	./dist/build/hevolisa/hevolisa --resize 3 alex_mic.png

clean:
	cd $(SRC) &&\
	cabal clean

