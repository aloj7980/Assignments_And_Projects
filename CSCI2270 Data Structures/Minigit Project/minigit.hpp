#ifndef MINIGIT_H
#define MINIGIT_H
#include<iostream>
#include<experimental/filesystem>
struct singlyNode{
    std::string fileName;
    int versionNumber;
    std::string fileVersion;
    singlyNode* next;
};
struct doublyNode{
    int commitNumber;
    singlyNode* head;
    doublyNode* previous;
    doublyNode* next;
};
class miniGit{
    public:
        miniGit();
        ~miniGit();
        void newRepo();
        void addFile(std::string fileName);
        void removeFile(std::string fileName);
        void commit();
        void checkout(int commitNum);
        doublyNode* head;
};

#endif