#include <iostream> // predefined header file (defined for you)
#include <string>
using namespace std;

#include "LinkedList.hpp" // your own header file
// <> vs "" -- compiler looks for the file in the C++ implementation
// vs in the current directory


LinkedList::LinkedList(){ // constructor definition
	head = nullptr;
}

LinkedList::~LinkedList(){
	Node* crawler;
	while(head!=nullptr){
		crawler = head->next;
		delete head;
		head = crawler;
	}
}


void LinkedList::insert(Node* afterMe, char newValue){

	if(head == nullptr){
		// first case: empty list
		// add new node and make head point to it
		head = new Node;
		head->key = newValue;
		head->next = nullptr; // what if we forget this line?
	}
	else if(afterMe == nullptr){
		// This condition implies that the list is not empty, but
		// the caller wants to add node before the head node
		Node* newNode = new Node;
		newNode->key = newValue;
		newNode->next = head;
		head = newNode;
	}
	else{
		Node* newNode = new Node;
		newNode->key = newValue;
		newNode->next = afterMe->next;
		afterMe->next = newNode;
	}


}
void LinkedList::displayList(){
	Node* crawler = head;
	while( crawler != nullptr ){
		cout << crawler->key << "->";
		crawler = crawler->next;
	}

	cout << "END" << endl;
}


void LinkedList::ReverseList(){
	//Does nothing if size is 0 or 1 because it's it's own inverse
    if(head==NULL || head->next==NULL){
        return;
    }
	//Current used to iterate through list
    Node* current=head;
	//The previous will be set as the next node each iteration
	Node* previous=NULL;
    while(current!=NULL){
		//temp used to store original node after current
		Node* temp=current->next;
		//Current's next node set to previous
		current->next=previous;
		//previous and current are each moved one spot down the list
		previous=current;
		current=temp;
	}
	//Previous, which in this case is the last node of the original list, 
	//is set as the new head.
	head=previous;
}


