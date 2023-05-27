#include <iostream>
using namespace std;

BST::BST() {
	root = NULL;
}

BST::~BST() {
	deleteTree(root);
	root = NULL;
}
void BST::deleteTree(Node* node) {
	if (node != NULL) {
		deleteTree(node->leftChild);
		deleteTree(node->rightChild);
		delete node;
	}
}

/*
** Implement the following function to return the count of comparisons, 
**   you may implement helper functions.
*/
int helper(Node* n,int target){
	if(n==NULL){
		return 0;
	}
	if(n->key==target){
		return 1;
	}else if(n->key>target){
		return helper(n->leftChild,target)+1;
	}else{
		return helper(n->rightChild,target)+1;
	}
}
int BST::searchCounter(int target) {
	// your code here!
	return helper(root,target);
}
