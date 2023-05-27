#include<iostream>
using namespace std;
int main(){
    int a=12;
    int b=33;
    int c=5;
    int* pa=&a;
    int* pb=&b;
    int* pc=&c;
    pb=pc;
    pa=pb;
    cout<<*pa<<*pb<<*pc<<endl;;
}