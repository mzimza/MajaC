int x = 0;

int foo() {
   if x < 5 {
      x = x + 1;
      int x = foo();
   }
   return x;
}

print (foo());
