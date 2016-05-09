/*
 * typeError.6
 * Author: Maja Zalewska
 * index nr: 336088
 * 
 * (1) Passing argument of a different type to function
 * (2) Passing wrong number of parameters to function
 */

void foo (int a, bool b) {
	return ;;
}

foo(false, true); // (1)
foo(false); // (2)
 