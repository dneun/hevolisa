SRC=/home/scvalex/proj/hevolisa

all:
	cd $(SRC) &&\
	cabal configure &&\
	cabal build &&\
	./dist/build/hevolisa/hevolisa --write-interval 200 --sample-size 0.8 --resize 3 alex_mic.png

clean:
	cd $(SRC) &&\
	cabal clean

