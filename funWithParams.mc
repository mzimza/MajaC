int foo(int a, int & b)
{
   print (a);
   print (b);
   b = 20;
   return b;
}
int c = 2;
int a = 10;
print(foo(c, a));
print(a);
foo(2, a);
