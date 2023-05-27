#include<iostream>
#include<fstream>
#include <sstream>
using namespace std;
struct studentData{
    string studentName;
    int homework;
    int recitation;
    int quiz;
    int exam;
    double average;
}
;
void addStudentData(studentData students[] ,string studentName,int homework,int recitation,int quiz,int exam,int length);
char calcLetter(double avg);
void printList(const studentData students,int length);
void addStudentData(studentData students[] ,string studentName,int homework,int recitation,int quiz,int exam,int length){
    double average=(double(homework)+double(recitation)+double(quiz)+double(exam))/4;
    studentData s = {};
    s.studentName=studentName;
    s.homework=homework;
    s.recitation=recitation;
    s.quiz=quiz;
    s.exam=exam;
    s.average=average;
    students[length]=s;
}
char calcLetter(double avg){
    if(avg>=90){
        return 'A';
    }else if(avg>=80){
        return 'B';
    }else if(avg>=70){
        return 'C';
    }else if(avg>=60){
        return 'D';
    }else{
        return 'F';
    }
}
void printList(const studentData students[],int length){
    for(int i=0;i<length;i++){
        cout<<students[i].studentName<<" earned "<<students[i].average<<" which is a(n) "<<calcLetter(students[i].average)<<endl;
    }
}
int main(int argc,char* argv[]){
    string fileName;
    string outFileName;
    string lower;
    string upper;
    fileName=argv[1];
    outFileName=argv[2];
    lower=argv[3];
    char lowerChar=lower[0];
    upper=argv[4];
    char upperChar=upper[0];
    ifstream myFile;
    myFile.open(fileName);
    studentData array[10];
    string line;
    int i=0;
    while(getline(myFile,line)){
        string name;
        int one,two,three,four;
        int j=0;
        while(true){
            if(line[j]==','){
                break;
            }
            name=name+line[j];
            j++;
        }
        stringstream s(line.substr(j+1,2));
        s>>one;
        stringstream t(line.substr(j+4,2));
        t>>two;
        stringstream u(line.substr(j+7,2));
        u>>three;
        stringstream v(line.substr(j+10,3));
        stringstream w(line.substr(j+10,2));
        if(name=="Tim Thomas"){
            v>>four;
        }else{
            w>>four;
        }
        addStudentData(array,name,one,two,three,four,i);
        i++;
    }
    myFile.close();
    ofstream myFile2;
    myFile2.open(outFileName);
    printList(array,i);
    for(int j=0;j<i;j++){
        if(calcLetter(array[j].average)<=lowerChar && calcLetter(array[j].average)>=upperChar){
            myFile2<<array[j].studentName<<","<<array[j].average<<","<<calcLetter(array[j].average)<<endl;
        }
    }
    myFile2.close();
}