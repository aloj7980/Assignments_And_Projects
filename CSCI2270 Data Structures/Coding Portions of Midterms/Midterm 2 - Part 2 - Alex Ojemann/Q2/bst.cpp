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
	//returns 0 if current node is null
	if(n==NULL){
		return 0;
	}
	//returns 1 if the node is the target to include the last search
	if(n->key==target){
		return 1;
		//returns 1 plus this function with the left child if the target is less than the current node's key
	}else if(n->key>target){
		return helper(n->leftChild,target)+1;
		//returns 1 plus this function with the right child if the target is greater than the current node's key
	}else{
		return helper(n->rightChild,target)+1;
	}
}
int BST::searchCounter(int target) {
	// your code here!
	//calls helper method that recursively counts number of searches to find the target starting at the root
	return helper(root,target);
}
