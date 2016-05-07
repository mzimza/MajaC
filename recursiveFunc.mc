int x = 0;

int fibo(int n) {
//   print(666);
//   print(n);
//   print(666);
   int m;
//   m = 108;
   if n == 0 {
      m = 0 ;
   } else {
      if n == 1 {
  //       print(777);
         m = 1;
    //     print(m); 
      }
      else {
      //   print(888);
         int a = fibo(n-2);
        // print(a);
         int b = fibo(n-1);
        // print(b);
        // print(1119);
        // print (a);
         m = a+b;

        // print(m);
        // print(999);
   
      }
   }
 //  print(555);
 //  print(m);
 //  print(555);
   return m;

}

print(1111);
print (fibo(5));

