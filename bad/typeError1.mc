/*
 * typeError1.mc
 * Author: Maja Zalewska
 * index nr: 336088
 *
 * All conditions must be of boolean type
 */

if 2+2 {
   print (false);
}

while 5 {
   print (false);
}

for int x = 1; x+2; x = x+1; {
	print(false);
}