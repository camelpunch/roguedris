.POSIX:
.SUFFIXES:

all:
	idris \
		--package contrib \
		--output roguedris \
		Main.idr

clean:
	rm -f roguedris

play: all
	./roguedris
