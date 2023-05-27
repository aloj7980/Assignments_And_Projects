#include "minigit.hpp"
#include<iostream>
#include<experimental/filesystem>
using namespace std;
miniGit::miniGit(){
    head=NULL;
}
miniGit::~miniGit(){
    delete head;
}
//creates a new repository as the head of the DLL to begin the program
void miniGit::newRepo(){
    std::filesystem::remove_all(".minigit");
    std::filesystem::create_directory(".minigit");
    doublyNode* newNode=new doublyNode;
    newNode->head=NULL;
    newNode->previous=NULL;
    newNode->next=NULL;
    newNode->commitNumber=0;
    head=newNode;
    return;
}
//adds a file (or updates its version if it already exists) to the current commit
void miniGit::addFile(string fileName){
    if(head==NULL){
        return;
    }
    doublyNode* Node=head;
    while(Node->next!=NULL){
        Node=Node->next;
    }
    singlyNode* n=Node->head;
    if(Node->head==NULL){
        std::filesystem::copy_file(fileName,"0_"+fileName);
        singlyNode* newNode=new singlyNode;
        newNode->fileName=fileName;
        newNode->versionNumber=0;
        newNode->fileVersion="0_"+fileName;
        newNode->next=NULL;
        Node->head=newNode;
        cout<<fileName<<" has been added."<<endl;
        return;
    }
    if(Node->head->fileName==fileName){
        Node->head->versionNumber++;
        Node->head->fileVersion=to_string(Node->head->versionNumber)+"_"+fileName;
        std::filesystem::copy_file(fileName,to_string(Node->head->versionNumber)+"_"+fileName);
        cout<<fileName<<" has been updated to the latest version."<<endl;
        return;
    }
    while(n->next!=NULL){
        if(n->fileName==fileName){
            n->versionNumber++;
            n->fileVersion=to_string(n->versionNumber)+"_"+fileName;
            std::filesystem::copy_file(fileName,to_string(n->versionNumber)+"_"+fileName);
            cout<<fileName<<" has been updated to this version."<<endl;
            return;
        }
        n=n->next;
    }
    std::filesystem::copy_file(fileName,"0_"+fileName);
    singlyNode* newNode=new singlyNode;
    newNode->fileName=fileName;
    newNode->versionNumber=0;
    newNode->fileVersion="0_"+fileName;
    newNode->next=NULL;
    n->next=newNode;
    cout<<fileName<<" has been added."<<endl;
}
//removes a file that has previously been added from the current commit
void miniGit::removeFile(string fileName){
    if(head==NULL){
        cout<<"Cannot delete file because no DLL nodes exist."<<endl;
        return;
    }
    doublyNode* Node=head;
    while(Node->next!=NULL){
        Node=Node->next;
    }
    if(Node->head==NULL){
        cout<<"Cannot delete file because no SLL nodes exist."<<endl;
        return;
    }
    singlyNode* n=Node->head;
    if(Node->head->next==NULL){
        if(Node->head->fileName==fileName){
            singlyNode* temp=Node->head;
            Node->head=NULL;
            delete temp;
            cout<<fileName<<" has been deleted."<<endl;
        }else{
            cout<<"Cannot delete file because this file was not found."<<endl;
        }
        return;
    }
    if(Node->head->fileName==fileName){
        singlyNode* temp=Node->head;
        Node->head=Node->head->next;
        delete temp;
        cout<<fileName<<" has been deleted."<<endl;
        return;
    }
    while(n->next!=NULL){
        if(fileName==n->next->fileName){
            singlyNode* temp=n->next;
            n->next=n->next->next;
            delete temp;
            cout<<fileName<<" has been deleted."<<endl;
            return;
        }
        n=n->next;
    }
    cout<<"Cannot delete file because this file was not found."<<endl;
}
//commits the current added files and creates a new DLL node for the next commit
void miniGit::commit(){
    doublyNode* Node=head;
    while(Node->next!=NULL){
        Node=Node->next;
    }
    doublyNode* newNode=new doublyNode;
    newNode->head=Node->head;
    newNode->previous=Node;
    newNode->next=NULL;
    newNode->commitNumber=Node->commitNumber+1;
    Node->next=newNode;
    cout<<"Your changes have been committed to the repository."<<endl;
}
//restores the files from an old commit to the current commit
void miniGit::checkout(int commitNum){
    doublyNode* Node=head;
    for(int i=0;i<commitNum;i++){
        Node=Node->next;
    }
    doublyNode* Node2=head;
    while(Node2->next!=NULL){
        Node2=Node2->next;
    }
    Node2->head=Node->head;
    cout<<"Checkout successful"<<endl;
}

