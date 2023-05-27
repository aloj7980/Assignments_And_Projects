/*
CSCI 2270 - Spring 2021
Midterm 1
Question 1 starter code.

*/


#include <iostream>

using namespace std;

void trim(int *a,int &length,int target);

void testPrint(int *p0, int length);

int main(){
   
    int a_test[] = {3, 2, 5 ,1, 0, 8, 4};
    int test_length = 7;
   
   
    /*
    Test 1
    */
    cout << "\n---------------------\n";
    
    int * a0 = new int[test_length];
    for(int i = 0; i<test_length; i++){
        a0[i] = a_test[i];
    }

    int target = 0; 
    
    /*
        * TODO your function call goes here. It should look like:
       
    */ 
   
    trim(a0, test_length, target);

    cout << "Test 1: \n"  << "expected >> 8 4 " << endl;
    testPrint(a0, test_length);
    delete [] a0;

    return 0;
}




void trim(int *a, int &length, int target){
    int index=0;
    while(a[index]!=target && index<length){
        index++;
    }
    index++;
    int b_length=length-index;
    int *b=new int[b_length];
    int i=0;
    while(index<length){
        b[i]=a[index];
        index++;
        i++;
    }
    length=b_length;
    for(int j=0;j<b_length;j++){
        a[j]=b[j];
    }
}


void testPrint(int *p0, int length){
    
    cout << "result   >> ";

    for(int i = 0; i<length; i++){
        cout << p0[i] << " ";
    }
    cout << "\n---------------------\n\n" << endl;
}