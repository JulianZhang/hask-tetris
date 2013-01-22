all:
	ghc Main.hs Signal.hs Layout.hs Logic.hs Structure.hs Render.hs --make -O2 -o go.exe

clean:
	rm *.hi *.o go.exe -f