/*
 * typeError10.mc
 * Author: Maja Zalewska
 * index nr: 336088
 * 
 * Static binding in blocks
 */

 int foo() {
 	print(x); // variable x not yet defined -> error
 	return 10;
 }

 int x = 1;