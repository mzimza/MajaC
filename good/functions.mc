/*
 * functions.mc
 * Author: Maja Zalewska
 * index nr: 336088
 * 
 * (1) Functions without parameters
 * (2) Functions with parameters passed by value and reference
 * (3) Recursive functions
 * (4) Functions changing global variables / static binding
 * (5) Function within a function
 */

// (1)
int countTo10() {
	for int i = 0; i < 10; i = i +1; {
		print(i); // 1, 2, ..., 9
	}
 	return 1;
}

int x = countTo10(); 

// (2)
void foo(int a, bool &b) {
	while a < 10 {
 		a = a + 1;
 	}
 	b = !b;
 	return ;;
}

bool b = true;
foo(5, b);
print(b); // False
foo(x, b);
print(x); // 1
print(b); // True

// (3)
int fibo (int n) {
	int m;
	if n == 0 {
		m = 0;
	}
	else {
		if n == 1 {
			m = 1;
		}
		else {
			m = fibo(n-1) + fibo(n-2);
		}
	}
	return m;
}

print(fibo(8)); // 21


// (4)
int multiply(int times) {
	x = x * times;
	return x;
}

multiply(4);
print(x); // 4

// (5)
bool isPowerOf(int n, int base) {
	int power(int n, int base) {
		int m = 1;
		if n > 1 {
			m = power(n-1, base);
		}
		return base * m;
	}
	int m = 1;
	int p = power(m, base);
	while p < n && base > 0 && n > 0 {
		m = m+1;
		p = power(m, base);
	}
	return n == p || n == base;
}

print(isPowerOf(10, 2)); // False
print(isPowerOf(8, 2)); // True
print(isPowerOf(0, 1)); // False
print(isPowerOf(1, 0)); // False
print(isPowerOf(0, 0)); // True
print(isPowerOf(1, 1)); // True