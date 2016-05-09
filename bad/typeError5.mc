/*
 * typeError.5
 * Author: Maja Zalewska
 * index nr: 336088
 * 
 * Passing a constant to function as reference
 */
 
 int foo (int &a) {
 	return a;
 }

 print(foo(1));