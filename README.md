# Russel! - A functional compiler for a small subset of Rust

    ______                              _   _
    | ___ \                            | | | |
    | |_/ /  _   _   ___   ___    ___  | | | |
    |    /  | | | | / __| / __|  / _ \ | | | |
    | |\ \  | |_| | \__ \ \__ \ |  __/ | | |_|
    \_| \_|  \__,_| |___/ |___/  \___| |_| (_)

The name was choosen after the compsci Steve RUSsel, who contributed to Lisp
and Spacewar!.

## Usage

0. Install https://hackage.haskell.org/package/ `optparse-applicative` -0.13.2.0/docs/Options-Applicative.html via cabal
1. Run `make` to compile the program
2. ` ./compiler some_input_file.rs` and it will generate an `out.asm` (check the help menu for more)
3. Run `make clean` to cleanup when you’re done

## Supported Syntax

- Function declaration and builtin functions (`println`, `read_line`)

        fn main () {
          read_line();
          println(42);
        }

        fn _auxiliary () {

        }

- Control Flow (`If`), Loops (`While`) and arithmetic and boolean expressions

        fn main () {
          let i = 0;
          while (i <= 1337)
          {
            if (i % 2 == 0) {
              println(1337);
            } else if (i == 42) {
              println(42);
            } else {
              println(31337);
            }
          }
        }

- Let attributions of Integers (of size 8, 16, 32, 64, 128) and Booleans as well
    as variables


        fn main () {
          let x = -1337;
          let y: u64 = 42;
          let z = true;
          let a = z;

          let xyz = if x == 42 {
                        1337;
                    }
                    else if y == false {
                        31337;
                    }
                    else {
                      42;
                    };
        }


Here’s a bit of crazy code that Russel! can handle pretty well :)


    fn main ()
    {
      let x = -1337;
      if x == 5 {
          let _foo: i32 = 5;
      }
      else if (x == 6)
      {
          let _foo: u128 = 6;
      }
      else {
          let _foo = 0;
      }
    }
    fn _spaceship () {
      let x: u64 = 1337;
              ;;               ;;
             ;;                 ;;
            ;;                   ;;
           ;;                     ;;
          ;;                       ;;
         ;;;         ;;;;;         ;;;
        ;;;       ;;;     ;;;       ;;;
       ;;;       ;   ;;;;;   ;       ;;;
      ; ;       ;  ;;;   ;;;  ;       ; ;
      ; ;;;;;;;;   ; ;;;;; ;   ;;;;;;;; ;
      ; ;;;;;;;;   ; ;;;;; ;   ;;;;;;;; ;
      ; ;       ;   ;;   ;;   ;       ; ;
       ;;;       ;   ;;;;;   ;       ;;;
        ;;;       ;;;     ;;;       ;;;
         ;;;         ;;;;;         ;;;
          ;;                       ;;
           ;;                     ;;
            ;;                   ;;
             ;;                 ;;
              ;;               ;;
      let _foo = if x == 5 {
                      42;
                }
                else if x == 6 {
                     -1337;
                }
                else {
                    31337;
                };;;;;;;
    }

## Authors

* **Diogo Cordeiro** - up*201705417*@fc.up.pt
* **Hugo Sales** - up*201704178*@fc.up.pt

## License

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA.

Additional permission under GNU GPL version 3 section 7

If you modify this program, or any covered work, by linking or
combining it with the OpenSSL project's OpenSSL library (or a
modified version of that library), containing parts covered by the
terms of the OpenSSL or SSLeay licenses, the Free Software Foundation
grants you additional permission to convey the resulting work.
Corresponding Source for a non-source form of such a combination
shall include the source code for the parts of OpenSSL used as well
as that of the covered work.
