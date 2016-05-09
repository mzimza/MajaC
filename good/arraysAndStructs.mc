/*
 * arraysAndStructs.mc
 * Author: Maja Zalewska
 * index nr: 336088
 * 
 * (1A) Declaration of an array of specified length with initialising it to default values (int -> 0, bool -> false)
 * (2A) Declaration of an array with assignment from an intialising list
 * (3A) Accessing array's elements
 * (4A) Passing an array to function
 * (5A) Array of structs
 * (1S) Definition of a struct type
 * (2S) Definition of a struct type with declaration of a struct variable (initialised to default values)
 * (3S) Accessing struct's elements
 * (4S) Assigning to struct
 * (5S) Passing struct to functions
 * (6S) Struct with array 
 */

[int] tab [2]; // (1A)
print(tab); // [0,0]
[[bool]] tab2 [2][1]; // (1A)
print(tab2); // [[False],[False]]

[int] tab3 [] = {1, 2, 3, 4}; // (2A)
print(tab3); // [1,2,3,4]
[[int]] tab4 [] = {{10},{20}}; // (2A)

tab[1] = 3; // (3A)
print(tab); // [0,3]

tab4[0] = {20}; // (3A)
[int] tmp [1];
tmp = tab4[1];
tmp[0] = 2 * tab4[0][0];
tab4[1] = tmp;
print(tab4); // [[20],[40]]

// (4A)
[int] foo([int] a, [int] & b) {
	b = {1, 2, 3};
	a[1] = 5;
	return a;
}

[int] ref [] = {5, 6, 7};
[int] result [1]; // Initialise result as an array of default values of length 1, but then assign a longer array
result = foo({1, 2, 3}, ref);
print(result); // [1,5,3]
print(ref); // [1,2,3]

void foo1(int & a) {
	a = -10;
	return ;;
}

foo1(tab4[0][0]);
print(tab4); // [[-10],[40]]

// (5A)(1S)(2S)
struct s {int a; bool b}; // (1S)
struct ss {struct s str; int x} str1; // (2S)
[[struct ss]] tab5 [2][1]; // (5A)
print(tab5); // [[struct ss: {"str": struct s: {"a": 0,"b": False},"x": 0}],[struct ss: {"str": struct s: {"a": 0,"b": False},"x": 0}]]

struct s str2;
print(str2.b); // (3S) False
str2.a = 108; // (4S)
str2.b = true; // (4S)
print(str2); // struct s: {"a": 108,"b": True}

struct s tmp  = tab5[0][0].str;
tmp.a = 10; // (4S)
[struct ss] tmp2 [1];
tmp2 = tab5[0];
struct ss tmp3 = tmp2[0];
tmp3.str = tmp;
tmp2[0] = tmp3;
tab5[0] = tmp2;
print(tab5); // [[struct ss: {"str": struct s: {"a": 10,"b": False},"x": 0}],[struct ss: {"str": struct s: {"a": 0,"b": False},"x": 0}]]

foo1(tab5[0][0].str.a);
print(tab5); // [[struct ss: {"str": struct s: {"a": -10,"b": False},"x": 0}],[struct ss: {"str": struct s: {"a": 0,"b": False},"x": 0}]]

// (5S)
struct s foo2(struct s a, struct s & b) {
	b.a = 11;
	b.b = false;
	a.a = 22;
	return a;
}

struct s ref2;
struct s res;
struct s res2;
res.a = 1;
res.b = true;
res2 = res;

res2 = foo2(res, ref2);

print(res2); // struct s: {"a": 22,"b": True}
print(ref2); // struct s: {"a": 11,"b": False}
print(res); // struct s: {"a": 1,"b": True}

// (6S)
struct sss {[struct s] tab [1]} str3;
print(str3); // struct sss: {"tab": [struct s: {"a": 0,"b": False}]}

if str3.tab[0].a == 0 {
	str3.tab = {res, res2};
}
print(str3); // struct sss: {"tab": [struct s: {"a": 1,"b": True},struct s: {"a": 22,"b": True}]}