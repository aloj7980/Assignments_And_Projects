#include<iostream>
#include "minigit.cpp"
#include<experimental/filesystem>
using namespace std;
int main(int argc,char* argv[]){
    miniGit m;
    string a;
    cout<<"Enter 1 to initialize a new repository"<<endl;
    cin>>a;
    //If "1" is entered, creates the new repository and enters the program that allows you to add, remove, commit, and checkout
    if(a=="1"){
        m.newRepo();
        while(true){
            string b="";
            cout<<"Enter 1 to add a file."<<endl;
            cout<<"Enter 2 to remove a file."<<endl;
            cout<<"Enter 3 to commit."<<endl;
            cout<<"Enter 4 to checkout."<<endl;
            cout<<"Enter 5 to exit program."<<endl;
            cin>>b;
            //If "1" is entered, adds a file to the current commit if the file exists
            if(b=="1"){
                string fileName;
                cout<<"Enter name of file to be added."<<endl;
                cin>>fileName;
                while(true){
                    if(std::filesystem::exists(fileName)){
                        break;
                    }else{
                        cout<<"File does not exist. Try again."<<endl;
                        cin>>fileName;
                    }
                }
                m.addFile(fileName);
            //If "2" is entered, removes a file from the current commit 
            }else if(b=="2"){
                string f;
                cout<<"Enter name of file to be removed"<<endl;
                cin>>f;
                m.removeFile(f);
            //If "3" is entered, commits the files that have been added
            }else if(b=="3"){
                m.commit();
            //If "4" is entered, restores the files from a previous commit to the current commit
            }else if(b=="4"){
                string g;
                cout<<"Enter commit number to replace current commit:"<<endl;
                cin>>g;
                int h=stoi(g);
                m.checkout(h);
            //If "5" is entered, exits the program
            }else if(b=="5"){
                cout<<"Goodbye!"<<endl;
                return 0;
            }
        }
    }
    return 0;
}