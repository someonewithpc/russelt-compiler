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
