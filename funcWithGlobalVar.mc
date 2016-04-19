int x = 3;

int foo() {
   print (x);
   x = 5;
   return 0;
}

int y = foo();
print (y);
print (x);
