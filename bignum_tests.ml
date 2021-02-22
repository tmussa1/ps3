(* 
                         CS 51 Problem Set 3
            Higher Order Functional Programming -- Testing
 *)

open Bignum ;;
open Test_simple ;; 
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

 let comparison_helper_test () =
   unit_test ((comparison_helper (fun (num1, num2) -> num1 = num2) (fun (x, y) -> x && y) [] []) = true)
             "comparison_helper empty lists";
   unit_test ((comparison_helper (fun (num1, num2) -> num1 = num2) (fun (x, y) -> x && y) [1; 2] [1; 2]) = true)
             "comparison_helper same lists with two elements if they are equal";
   unit_test ((comparison_helper (fun (num1, num2) -> num1 = num2) (fun (x, y) -> x && y) [1; 3] [1; 2]) = false)
             "comparison_helper one list greater checking if they are equal";
   unit_test ((comparison_helper (fun (num1, num2) -> num1 = num2) (fun (x, y) -> x && y) [1; 2; 3] [1; 2]) = false)
             "comparison_helper two lists with differing length if they are equal";
   unit_test ((comparison_helper (fun (num1, num2) -> num1 > num2) (fun (x, y) -> x || y) [] []) = false)
             "comparison_helper empty lists if one is greater than the other";
   unit_test ((comparison_helper (fun (num1, num2) -> num1 > num2) (fun (x, y) -> x || y) [1; 2] [1; 2]) = false)
             "comparison_helper same lists if one is greater than the other";
   unit_test ((comparison_helper (fun (num1, num2) -> num1 > num2) (fun (x, y) -> x || y) [1; 2; 3] [1; 2]) = true)
             "comparison_helper two lists differing length if one is greater than the other";
   unit_test ((comparison_helper (fun (num1, num2) -> num1 > num2) (fun (x, y) -> x || y) [1; 3] [1; 2]) = true)
             "comparison_helper one list greater than the other checking if it is indeed greater";
   unit_test ((comparison_helper (fun (num1, num2) -> num1 < num2) (fun (x, y) -> x || y) [] []) = false)
             "comparison_helper empty lists if one is less than the other";
   unit_test ((comparison_helper (fun (num1, num2) -> num1 < num2) (fun (x, y) -> x || y) [1; 2] [1; 2]) = false)
             "comparison_helper same lists if one is less than the other";
   unit_test ((comparison_helper (fun (num1, num2) -> num1 < num2) (fun (x, y) -> x || y) [1; 2] [1; 2; 3]) = true)
             "comparison_helper two lists differing length if one is less than the other";
   unit_test ((comparison_helper (fun (num1, num2) -> num1 > num2) (fun (x, y) -> x || y) [1; 2] [1; 3]) = true)
             "comparison_helper one list less than the other checking if it is indeed less";;
 
 let test_all () =
   negate_test (),
   comparison_helper_test ()
   ;;
 
 let _ = test_all () ;;