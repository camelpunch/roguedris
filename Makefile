.POSIX:
.SUFFIXES:

all:
	idris \
		--package contrib \
		--output roguedris \
		Main.idr

play: all
	./roguedris
