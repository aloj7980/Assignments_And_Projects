/*
	DO NOT MODIFY!
*/

struct Node
{
	int key;
	Node* leftChild = NULL;
	Node* rightChild = NULL;

	Node(int k, Node* left, Node* right) { 
		key = k;
		leftChild = left;
		rightChild = right;
	}
};

class BST
{
	public:
		Node* root;
		
		BST();
		~BST();
		
		void deleteTree(Node* root);
		int searchCounter(int target); // To be implement by you!

};
