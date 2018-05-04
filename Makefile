.POSIX:
.SUFFIXES:

all:
	idris \
		--package contrib \
		--output roguedris \
		Main.idr

clean:
	rm roguedris

play: all
	./roguedris
