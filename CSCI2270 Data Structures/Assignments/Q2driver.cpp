#include <iostream>
#include <string>
using namespace std;

// any file that uses the LinkedList class must include this header file

struct Node{
	char key;
	Node *next;
};

class LinkedList{
private:
	Node* head;

public:
	LinkedList(); // constructor declaration

	~LinkedList(); // destructor declaration

	void insert(Node* afterMe, char newValue);
	// Precondition: afterMe is a valid pointer to a node in the linked list.
	// newValue is a valid string.

	// Postcondition: a new node is created and newValue is stored as its key.
	// The new node is added after node containing afterMe.

	void displayList();
	// Precondition: the head node is defined.

	// Post condition: display the key values of the entire list, starting with
	// first node and ending with last node.

	void ReverseList();
	// Precondition: the head node is defined
	
	// Postcondition: the order of the list is reversed.

};
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
