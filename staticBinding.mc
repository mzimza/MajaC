
int foo() {
   if x < 5 {
      x = x + 1;
      int x = foo();
   }
   return x;
}

int x = 0;
print (foo());
