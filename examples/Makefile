
FLAGS = -Wall -Werror -O2 -i..
LIBS = -lgd -L/opt/local/lib
SRCS = $(wildcard *.hs)
EXES = $(subst .hs,.exe,$(SRCS))

.PHONY: all clean

default: $(EXES)

all: $(EXES)

%.exe: %.hs
	ghc --make $(FLAGS) $< -o $@ $(LIBS)

clean:
	-rm *.png *.exe *.o *.hi *~ dataurl.html
