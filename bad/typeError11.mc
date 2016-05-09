/*
 * typeError11.mc
 * Author: Maja Zalewska
 * index nr: 336088
 * 
 * (1) Declaration of struct within a struct
 * (2) Loop in struct declaration / struct not yet defined
 */

 struct s { struct x {int a} s}; // (1)
 struct s { struct s a}; // (2)