#include <iostream>
#include "bst.hpp"

using namespace std;

/***************************
Test Tree:

         20
	   /    \
     15      30
	/       /   \
   10      25    40
     \       \
	  11       45

***************************/

int main() {
	BST tree;

	// Root
	tree.root = new Node{20, NULL, NULL};
	// h = 1
	tree.root->leftChild = new Node{15, NULL, NULL};
	tree.root->rightChild = new Node{30, NULL, NULL};
	// h = 2
	tree.root->leftChild->leftChild = new Node{10, NULL, NULL};
	tree.root->rightChild->leftChild = new Node{25, NULL, NULL};
	tree.root->rightChild->rightChild = new Node{40, NULL, NULL};
	// h = 3
	tree.root->leftChild->leftChild->rightChild = new Node{11, NULL, NULL};
	tree.root->rightChild->rightChild->leftChild = new Node{35, NULL, NULL};

	cout << "Searching for 11:" << endl;
	cout << "\texpected: 4" << endl;
	cout << "\t     got: " << tree.searchCounter(11) << endl;

	cout << "Searching for 20:" << endl;
	cout << "\texpected: 1" << endl;
	cout << "\t     got: " << tree.searchCounter(20) << endl;

	cout << "Searching for 38:" << endl;
	cout << "\texpected: 4" << endl;
	cout << "\t     got: " << tree.searchCounter(38) << endl;

	cout << "Searching for 16:" << endl;
	cout << "\texpected: 2" << endl;
	cout << "\t     got: " << tree.searchCounter(16) << endl;

	return 0;
}
