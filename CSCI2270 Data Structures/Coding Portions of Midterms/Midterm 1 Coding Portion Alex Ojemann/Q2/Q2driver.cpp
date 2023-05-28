#include <iostream>
#include <string>
using namespace std;

// any file that uses the LinkedList class must include this header file
#include "LinkedList.hpp"

int main(){
   
	/*
	Test cases are provided only to help you test your code. Each
	submission will be individually inspected by the teaching staff.

	*/

	// Sample Linked List
	char testStringll[] = {'n','a','d','i','a'};
	LinkedList ll;
	int n = 5;
	for(int i = 0; i<n; i++){
		ll.insert(nullptr,testStringll[i]);
	}

    /*
    Test A
    */



	cout << "\n------------------------------------------\n";

	cout << "Test A:" << endl; 
	cout << "original >> ";
	ll.displayList();

	/* Call to Reverse function  */
	ll.ReverseList();
	cout << "\nexpected >> n->a->d->i->a->END" << endl;
	cout << "result   >> ";
	ll.displayList();

	cout << "\n------------------------------------------\n";

    /*
		FEEL FREE TO ADD YOUR OWN TEST CASES
    */



	return 0;
}
