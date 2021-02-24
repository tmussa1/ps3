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
 
 let test_all () =
   negate_test ()
   ;;
 
 let _ = test_all () ;;