(* 
                         CS 51 Problem Set 3
            Higher Order Functional Programming -- Testing
 *)

open Bignum ;;
open CS51Utils ;;
open Absbook ;;
   
 let negate_test () =
   unit_test ((negate {neg = false; coeffs = []}) = {neg = true; coeffs = []})
             "negate with neg false and coeffs empty";
   unit_test ((negate {neg = true; coeffs = []}) = {neg = false; coeffs = []})
             "negate with neg true and coeffs empty";
   unit_test ((negate {neg = false; coeffs = [1; 3; 5]}) = {neg = true; coeffs = [1; 3; 5]})
             "negate with neg false and non-empty coeffs";
   unit_test ((negate {neg = true; coeffs = [2; 4; 6]}) = {neg = false; coeffs = [2; 4; 6]})
             "negate with neg true and non-empty coeffs" ;;
  
 let equal_test () = 
  unit_test ((equal {neg = false; coeffs = []} {neg = false; coeffs = []}) = true)
            "equal test with same sign and empty lists";
  unit_test ((equal {neg = false; coeffs = []} {neg = true; coeffs = []}) = false)
            "equal test with different sign and empty lists";
  unit_test ((equal {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1; 2]}) = true)
            "equal test with same sign and same non-empty lists";
  unit_test ((equal {neg = false; coeffs = [1; 2]} {neg = true; coeffs = [1; 2]}) = false)
            "equal test with different sign and same non-empty lists" ;;
 
 let less_test () = 
  unit_test ((less {neg = false; coeffs = []} {neg = false; coeffs = []}) = false)
            "less test with same sign and empty lists";
  unit_test ((less {neg = false; coeffs = []} {neg = true; coeffs = []}) = false)
            "less test with different sign and empty lists";
  unit_test ((less {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1; 2]}) = false)
            "less test with same sign and same non-empty lists";
  unit_test ((less {neg = false; coeffs = [1; 2]} {neg = true; coeffs = [1; 2]}) = false)
            "less test with different sign, first one positive and same non-empty lists";
  unit_test ((less {neg = true; coeffs = [1; 2]} {neg = false; coeffs = [1; 2]}) = true)
            "less test with different sign, first one negative and same non-empty lists";
  unit_test ((less {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1; 2; 3]}) = true)
            "less test with same sign and first list less";
  unit_test ((less {neg = false; coeffs = [1; 2; 3]} {neg = false; coeffs = [1; 2]}) = false)
            "less test with same sign and second list less";;

 let greater_test () = 
  unit_test ((greater {neg = false; coeffs = []} {neg = false; coeffs = []}) = false)
            "greater test with same sign and empty lists";
  unit_test ((greater {neg = false; coeffs = []} {neg = true; coeffs = []}) = true)
            "greater test with different sign and empty lists";
  unit_test ((greater {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1; 2]}) = false)
            "greater test with same sign and same non-empty lists";
  unit_test ((greater {neg = false; coeffs = [1; 2]} {neg = true; coeffs = [1; 2]}) = true)
            "greater test with different sign, first one positive and same non-empty lists";
  unit_test ((greater {neg = true; coeffs = [1; 2]} {neg = false; coeffs = [1; 2]}) = false)
            "greater test with different sign, first one negative and same non-empty lists";
  unit_test ((greater {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1; 2; 3]}) = false)
            "greater test with same sign and first list less";
  unit_test ((greater {neg = false; coeffs = [1; 2; 3]} {neg = false; coeffs = [1; 2]}) = true)
            "greater test with same sign and second list less";;

 let test_all () =
   negate_test (),
   equal_test (),
   less_test (),
   greater_test ()
   ;;
 
 let _ = test_all () ;;