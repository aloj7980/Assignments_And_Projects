// LinkedList.hpp - interface file (header file)
#ifndef LinkedList_H
#define LinkedList_H

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





#endif
