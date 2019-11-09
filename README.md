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

1. Run `make` to compile the program
2. ` ./parser < some_input_file.rs`
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
            if (i % 2) {
              println(1337);
            } else if (i == 42) {
              println(42);
            } else {
              println(31337);
            }
          }
        }

- Let attributions of Integers (of size 8, 16, 32, 64, 128) and Booleans


        fn main () {
          let x = -1337;
          let y: u64 = 42;
          let z = true;

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
