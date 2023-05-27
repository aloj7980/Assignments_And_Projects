#include<iostream>
#include<fstream>
using namespace std;
struct wordRecord{
    string word;
    int count;
};
void getIgnoreWords(string ignoreWordFileName, string ignoreWords[])
{
    ifstream fin;
    fin.open(ignoreWordFileName);
    string line;
    int count=0;
    while(getline(fin,line)){
        ignoreWords[count]=line;
        count++;
    }
}
bool isIgnoreWord(string word, string ignoreWords[])
{
    int len=50;
    for(int i=0;i<len;i++){
        if(ignoreWords[i]==word){
            return true;
        }
    }
    return false;
}
int getTotalNumberNonIgnoreWords(wordRecord distinctWords[], int length)
{
    int c=0;
    for(int i=0;i<length;i++){
        c+=distinctWords[i].count;
    }
    return c;
}
void sortArray(wordRecord distinctWords[], int length)
{
    for(int i=0;i<length;i++){
        for(int j=0;j<length-i;j++){
            if(distinctWords[j].count<distinctWords[j+1].count){
                wordRecord temp = distinctWords[j];
                distinctWords[j]=distinctWords[j+1];
                distinctWords[j+1]=temp;
            }
        }
    }
}
void printTenFromN(wordRecord distinctWords[], int N, int totalNumWords)
{
    int k=N+10;
    for(int i=N;i<k;i++){
          float p = (float) distinctWords[i].count / totalNumWords;
          int p1=(int)(p*100000);
          if(p*100000-(float)(p1)>=.5){
            p1++;
          }
          float p2=(float)(p1);
          float p3=p2/100000;
          if(p1%100==0){
            cout<<p3<<"00 - "<<distinctWords[i].word<<endl;
          }else if(p1%10==0){
            cout<<p3<<"0 - "<<distinctWords[i].word<<endl;
          }else{
            cout<<p3<<" - "<<distinctWords[i].word<<endl;
          }
    }
}
wordRecord* resize(wordRecord* arrayPtr, int capacity)
{
	int newCapacity = (capacity)*2;
	wordRecord *newArray = new wordRecord[newCapacity];
	for(int i=0;i<capacity;i++){
		newArray[i]=arrayPtr[i];
	}
	delete[] arrayPtr;
	arrayPtr=nullptr;
	capacity = newCapacity;
	return newArray;
}
int main(int argc, char* argv[]){
    if(argc!=3){
        cout << "Usage: Assignment2Solution <number of words> <inputfilename.txt> <ignoreWordsfilename.txt>"<<endl;
        return -1;
    }else{
        int num = stoi(argv[0]);
        string fileName=argv[1];
        string fileName2=argv[2];
        int capacity=100;
        wordRecord* words;
        words=new wordRecord[capacity];
        int countArrayDoubles=0;
        string commonWords[50];
        getIgnoreWords(fileName2,commonWords);
        ifstream fin2;
        fin2.open(fileName);
        string line;
        int i=0;
        while(getline(fin2,line)){
            if(i>=capacity){
                words=resize(words,capacity);
                countArrayDoubles++;
            }
            if(!isIgnoreWord(line,commonWords)){
                bool q=true;
                for(int j=0;j<i;j++){
                    if(words[j].word==line){
                        q=false;
                        words[j].count=words[j].count+1;
                    }
                }
                if(q){
                    wordRecord w={};
                    w.word=line;
                    w.count=1;
                    words[i]=w;
                }
            }
            i++;
        }
        sortArray(words,i);
        cout<<"Array Doubled: "<<countArrayDoubles<<endl;
        cout<<"Distinct non-common words: "<<i<<endl;
        cout<<"Total non-common words: "<<getTotalNumberNonIgnoreWords(words,i)<<endl;
        cout<<"Probability of next 10 words form rank "<<num<<endl;
        cout<<"---------------------------------------"<<endl;
        printTenFromN(words, num, getTotalNumberNonIgnoreWords(words,i)+50);
    }
}