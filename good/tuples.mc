/*
 * tuples.mc
 * Author: Maja Zalewska
 * index nr: 336088
 * 
 * (1) Declaration of a tuple
 * (2) Retrieving values from tuple
 * (3) Tuples in functions
 * (4) Local variables
 */

// (1)
(int, bool) tup;
tup = (1, true);
print(tup); // (1,True)


struct s {(int, bool) t} str;

(int, struct s, [int]) tup2 = (10, str, {1, 2, 3});
print(tup2); //(10,struct s: {"t": (0,False)},[1,2,3])

// (2)(4)
if true {
	[int] tab [2];
	int len;
	([int], int) tup = ({1, 2}, 2); // (4) previous declaration of tup is covered by this one
	(tab, len) = tup;
	print(tab); // [1,2]
	print(len); // 2

	(int a, bool b) = (10, true);
	print(a); // 10
	print(b); // True
}

// (4) back to the previous declaration of tup
print(tup); // (1,True)

// (3)
void multiplyArray (([int], int) & arr, int times) {
	[int] tab [1];
	int len;
	(tab, len) = arr;
	for int i = 0; i < len; i = i + 1; {
		tab[i] = tab[i] * times;
	}
	arr = (tab, len);
	return ;;
}

[int] tab [] = {1, 2, 3};
([int], int) pair = (tab, 3);
multiplyArray(pair, 3);
print(pair); // ([3,6,9],3)

(int, bool) toTuple(int a, bool b) {
	return (a, b);
}

print(toTuple(10, true)); // (10,True)

