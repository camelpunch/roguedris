.POSIX:
.SUFFIXES:

all:
	idris \
	--output roguedris \
	Main.idr

play: all
	./roguedris
