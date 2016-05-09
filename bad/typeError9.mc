/*
 * typeError.9
 * Author: Maja Zalewska
 * index nr: 336088
 * 
 * (1) Function returning different type than declared
 * (2) Function returning reference
 */

 int foo() {
 	return ;; // (1) returning void
 }

 void foo1() {
 	return 10; // (1) returning an int
 }

 // (2) returning a reference is forbidden
 int & foo() {
 	return 10;
 }