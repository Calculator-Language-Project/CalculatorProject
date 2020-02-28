# CalculatorProject


            
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

Int tempA = a;
Int tempB = b;
While (b !=0)
{
	Int temp = a % b;
	a = b
	b = temp;
}
Cout << "GCD of "<< tempA <<" and "<< tempB <<" is "<< a;
	

Fibonacci
void Fib(int n){
    Int t1 = 0, t2 = 1, nextTerm = 0;
    for (int i = 1; i <= n; ++i)
    {
        if(i == 1){
            cout << " " << t1;
            continue;
        }
        if(i == 2) {
            cout << t2 << " ";
            continue;
        }
        nextTerm = t1 + t2;
        t1 = t2;
        t2 = nextTerm;
        cout << nextTerm << " ";
    }
}
