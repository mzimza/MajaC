/* Author Maja Zalewska
 * Index: 336088
 * Example of MajaC usage
 */

// declaring and assigning values to variables
int a; // declaration
a = 0; // simple assign
bool b = true; // bool
bool c = False; // bool capitalised
(int, int, bool) t = (1, 2, 1); // tuple
struct str_type { int a; bool b; } d; // struct type declaration
struct str_type d = {108, True}; // initialize whole struct
d.b = true; // assigning value to a struct's field
int arr[2] = { 1, 2 }; // array declaration with assignment
bool arr2[1]; // simple array declaration
arr2[0] = false; // assign value at index in array

// simple function without parameters
int foo() {
   return 1;
}

// function with arguments passed by value and reference
bool foo1(int a, int & b) {
   bool result = false;
   if a == 1 {
      b = 10;
      result = true;
   }
   return result;
}

// function with nested function
int foo2(int a) {
   int b = 1;
   int foo3(int a) {
      return (a+b); 
   }
   return foo3(a);
}

// if-else
if b != c && true {
   if a >= 0 {
      print (1);
   }
   else {
      print (0);
   }  
} 
else {
   while b == c {
      b = !b;
   }
}

// for loop
for int i = 0; i < 2; i = i + 1; {
   print (i);
}
