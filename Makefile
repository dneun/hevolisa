all:
	ghc --make Main

clean:
	rm *.o *.hi *.manifest 

clean-saves:
	rm *~


clean-doc:
	rm *.html *.js *.gif *.css

haddock:
	haddock -h *.hs

