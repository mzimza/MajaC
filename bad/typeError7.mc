/*
 * typeError7.mc
 * Author: Maja Zalewska
 * index nr: 336088
 * 
 * (1) Declaring a struct of not previously defined struct type
 * (2) Usage of previoulsy not declared variable/function
 */

 struct s a; // (1)
 x = 1; // (2)
 print(foo()); // (2)