#include<iostream>
#include<fstream>
using namespace std;
int addToArrayAsc(float sortedArray[], int numElements, float newValue);
int main(int argc,char* argv[]){
    string fileName;
    fileName=argv[1];
    ifstream myFile;
    myFile.open(fileName);
    if(!myFile.is_open()){
        cout<<"Failed to open the file."<<endl;
        return 0;
    }
        string line;
        float array[100];
        int i=0;
        while(getline(myFile, line)){
            float num=stof(line);
            addToArrayAsc(array,i,num);
            i++;
            for(int j=0;j<i;j++){
                cout<<array[j];
                if(j<i-1){
                    cout<<",";
                }
            }
            cout<<endl;
        }
}
int addToArrayAsc(float sortedArray[], int numElements, float newValue){
    sortedArray[numElements]=newValue;
    for(int i=0;i<numElements;i++){
        for(int j=0;j<numElements-i;j++){
            if(sortedArray[j]>sortedArray[j+1]){
                float temp = sortedArray[j];
                sortedArray[j]=sortedArray[j+1];
                sortedArray[j+1]=temp;
            }
        }
    }
    return numElements+1;
}