# CalculatorProject


```            
Euclid Algorithm
int gcd(int a, int b) {
   if (b == 0)
   	return a;
   return gcd(b, a % b);
} 
int main() {
   int a , b;
   cout<<"Enter the values of a and b: "<<endl;
   cin>>a>>b;
   cout<<"GCD of "<< a <<" and "<< b <<" is "<< gcd(a, b);
   return 0;
}

void GCD(int a, int b) {
	Cout << "GCD of "<< a <<" and "<< b <<" is ";
	int temp = 0;
	While (!(b == 0))
	{
		temp = a % b;
		a = b;
		b = temp;
	}
	Cout << a;
}
	

Fibonacci
void Fib(int n){
    Int t1 = 0, t2 = 1, nextTerm = 0;
    cout << " " << t1;
    cout << t2 << " ";
    for (int i = 3; i <= n; ++i)
    {
        nextTerm = t1 + t2;
        t1 = t2;
        t2 = nextTerm;
        cout << nextTerm << " ";
    }
}
```
