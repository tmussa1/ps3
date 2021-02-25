(* 
                         CS 51 Problem Set 3
            Higher Order Functional Programming -- Testing
 *)

open Bignum ;;
open CS51Utils ;;
open Absbook ;;
   
 let negate_test () =
    unit_test (negate {neg = false; coeffs = []} = {neg = true; coeffs = []})
              "negate with neg false and coeffs empty";
    unit_test (negate {neg = true; coeffs = []} = {neg = false; coeffs = []})
              "negate with neg true and coeffs empty";
    unit_test (negate {neg = false; coeffs = [1; 3; 5]} = {neg = true; coeffs = [1; 3; 5]})
              "negate with neg false and non-empty coeffs";
    unit_test (negate {neg = true; coeffs = [2; 4; 6]} = {neg = false; coeffs = [2; 4; 6]})
              "negate with neg true and non-empty coeffs" ;;
    
 let equal_test () = 
    unit_test (equal {neg = false; coeffs = []} {neg = false; coeffs = []} = true)
              "equal test with same sign and empty lists";
    unit_test (equal {neg = false; coeffs = []} {neg = true; coeffs = []} = false)
              "equal test with different sign and empty lists";
    unit_test (equal {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1; 2]} = true)
              "equal test with same sign and same non-empty lists";
    unit_test (equal {neg = false; coeffs = [1; 2]} {neg = true; coeffs = [1; 2]} = false)
              "equal test with different sign and same non-empty lists" ;;
 
 let less_test () = 
    unit_test (less {neg = false; coeffs = []} {neg = false; coeffs = []} = false)
              "less test with same sign and empty lists";
    unit_test (less {neg = false; coeffs = []} {neg = true; coeffs = []} = false)
              "less test with different sign and empty lists";
    unit_test (less {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1; 2]} = false)
              "less test with same sign and same non-empty lists";
    unit_test (less {neg = false; coeffs = [1; 2]} {neg = true; coeffs = [1; 2]} = false)
              "less test with different sign, first one positive and same non-empty lists";
    unit_test (less {neg = true; coeffs = [1; 2]} {neg = false; coeffs = [1; 2]} = true)
              "less test with different sign, first one negative and same non-empty lists";
    unit_test (less {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1; 2; 3]} = true)
              "less test with same sign and first list less";
    unit_test (less {neg = false; coeffs = [1; 2; 3]} {neg = false; coeffs = [1; 2]} = false)
              "less test with same sign and second list less" ;;

 let greater_test () = 
    unit_test (greater {neg = false; coeffs = []} {neg = false; coeffs = []} = false)
              "greater test with same sign and empty lists";
    unit_test (greater {neg = false; coeffs = []} {neg = true; coeffs = []} = true)
              "greater test with different sign and empty lists";
    unit_test (greater {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1; 2]} = false)
              "greater test with same sign and same non-empty lists";
    unit_test (greater {neg = false; coeffs = [1; 2]} {neg = true; coeffs = [1; 2]} = true)
              "greater test with different sign, first one positive and same non-empty lists";
    unit_test (greater {neg = true; coeffs = [1; 2]} {neg = false; coeffs = [1; 2]} = false)
              "greater test with different sign, first one negative and same non-empty lists";
    unit_test (greater {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1; 2; 3]} = false)
              "greater test with same sign and first list less";
    unit_test (greater {neg = false; coeffs = [1; 2; 3]} {neg = false; coeffs = [1; 2]} = true)
              "greater test with same sign and second list less" ;;

  let from_int_test () = 
    unit_test (from_int 0 = {neg = false; coeffs = [0]})
              "from_int zero input";
    unit_test (from_int max_int= {neg = false; coeffs = [4; 611; 686; 018; 427; 387; 903]})
              "from_int max_int";
    unit_test (from_int (min_int + 1) = {neg = true; coeffs = [4; 611; 686; 018; 427; 387; 903]})
              "from_int min_int";
    unit_test (from_int 987754098 = {neg = false; coeffs = [987; 754; 98]})
              "from_int large number with zero digit";
    unit_test (from_int ~-987436 = {neg = true; coeffs = [987; 436]})
              "from_int zero negative number";
    unit_test (from_int 42 = {neg = false; coeffs = [42]})
              "from_int small number" ;;

  let to_int_test () = 
    unit_test (to_int {neg = false; coeffs = []} = Some (0))
              "to_int empty list coefficients";
    unit_test (to_int {neg = false; coeffs = [498; 611; 686; 018; 427; 387; 903]} = None)
              "to_int > max_int input input";
    unit_test (to_int {neg = false; coeffs = [4; 611; 686; 018; 427; 387; 903]} = Some (max_int))
              "to_int max_int input";
    unit_test (to_int {neg = true; coeffs = [4; 611; 686; 018; 427; 387; 904]} = Some (min_int))
              "to_int min_int input";
    unit_test (to_int {neg = false; coeffs = [76]} = Some (76))
              "to_int small number";
    unit_test (to_int {neg = true; coeffs = [987; 436]} = Some (~-987436))
              "to_int negative number" ;;

  let plus_test () = 
    unit_test (plus {neg = false; coeffs = []} {neg = false; coeffs = []}  = 
              {neg = false; coeffs = []})
              "plus empty coefficients";
    unit_test (plus {neg = false; coeffs = [123; 456]} {neg = false; coeffs = [345; 678]}  = 
              {neg = false; coeffs = [469; 134]})
              "plus two positive numbers";
    unit_test (plus {neg = true; coeffs = [123; 456]} {neg = false; coeffs = [345; 678]}  = 
              {neg = false; coeffs = [222; 222]})
              "plus zero one positive and one negative and sum positive";
    unit_test (plus {neg = false; coeffs = [123; 456]} {neg = true; coeffs = [345; 678]}  = 
              {neg = true; coeffs = [1; 777; 778]})
              "plus zero one positive and one negative and sum negative";
    unit_test (plus {neg = true; coeffs = [123; 456; 987]} {neg = false; coeffs = [345; 678]}  = 
              {neg = true; coeffs = [1; 876; 888; 691]})
              "plus differing length coefficients";
    unit_test (plus {neg = true; coeffs = [658; 987]} {neg = true; coeffs = [987; 546]}  = 
              {neg = true; coeffs = [1; 646; 533]})
              "plus two negative numbers and sum negative" ;;

  let times_test () = 
    unit_test (times {neg = false; coeffs = []} {neg = false; coeffs = []}  = 
              {neg = false; coeffs = []})
              "times empty coefficients";
    unit_test (times {neg = false; coeffs = [123; 456]} {neg = false; coeffs = [345; 678]}  = 
              {neg = false; coeffs = [126; 295; 488]})
              "times two positive numbers";
    unit_test (times {neg = true; coeffs = [123; 456]} {neg = false; coeffs = [345; 678]}  = 
              {neg = true; coeffs = [126; 295; 488]})
              "times one positive and one negative and product positive";
    unit_test (times {neg = true; coeffs = [123; 456; 987]} {neg = false; coeffs = [345; 678]}  = 
              {neg = true; coeffs = [126; 296; 497; 701]})
              "times differing length coefficients";
    unit_test (times {neg = true; coeffs = [876; 986; 546; 123; 456]} {neg = true; coeffs = [875; 875; 983; 345; 678]}  = 
              {neg = false; coeffs = [3; 293; 961; 467; 239; 700; 736]})
              "times two really big numbers";
    unit_test (times {neg = true; coeffs = [658; 987]} {neg = true; coeffs = [987; 546]}  = 
              {neg = false; coeffs = [1; 10; 227; 71]})
              "times two negative numbers and product positive" ;;

 let test_all () =
   negate_test (),
   equal_test (),
   less_test (),
   greater_test (),
   from_int_test (),
   to_int_test (),
   plus_test (),
   times_test ()
   ;;
 
 let _ = test_all () ;;