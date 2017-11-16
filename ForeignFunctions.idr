module ForeignFunctions

%access export
%default total

%include c "curses.h"
%flag C "-lncurses"

public export
data Point : Type where
  MkPoint : (x :  Nat) -> (y : Nat) -> Point

addstr : String -> IO ()
addstr s
  = foreign FFI_C "addstr"
   (String -> IO ())
    s

mvaddstr : Point -> String -> IO Int
mvaddstr (MkPoint x y) s
  = foreign FFI_C "mvaddstr"
    (Int -> Int -> String -> IO Int)
    (fromNat y) (fromNat x) s

printw : String -> IO ()
printw s
  = foreign FFI_C "printw"
   (String -> IO ())
    s

refresh : IO ()
refresh
  = foreign FFI_C "refresh"
   (IO ())

echo : IO Int
echo = foreign FFI_C "echo" (IO Int)

noecho : IO Int
noecho = foreign FFI_C "noecho" (IO Int)

curs_set : Int -> IO Int
curs_set visibility = foreign FFI_C "curs_set" (Int -> IO Int) visibility

cbreak : IO Int
cbreak = foreign FFI_C "cbreak" (IO Int)

initscr : IO Int
initscr = foreign FFI_C "initscr" (IO Int)

timeout : Int -> IO ()
timeout n = foreign FFI_C "timeout" (Int -> IO ()) n

getch : IO (Char)
getch = foreign FFI_C "getch" (IO Char)

endwin : IO Int
endwin = foreign FFI_C "endwin" (IO Int)
