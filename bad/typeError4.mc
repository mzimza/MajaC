/*
 * typeError.4
 * Author: Maja Zalewska
 * index nr: 336088
 * 
 * Access of non array/struct/tuple variable as array/struct/tuple results in error
 */
 
 struct s {int field};

 int a = 10;
 print (a[1]); // not an array
 print (a.field); // not a struct
 (int x, bool b) = a; // not a tuple