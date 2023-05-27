#include<iostream>
using namespace std;
int main(){
    int a=11;
    int b=13;
    int c=12;
    int * pa=&a;
    int * pb=&b;
    int * pc=&c;
    pb=pa;
    pa=pc;
    cout<<*pa<<*pb<<*pc<<endl;
}