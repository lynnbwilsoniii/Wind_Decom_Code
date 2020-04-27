#include <iostream>
#include <string>
#include <fstream>
#include <cstdlib>

using namespace std;

int main(){
	string func1 = "char *smsgver();\n";
	string func2 = "char *smsgtimes();\n";
	string func3 = "char *smsghms();\n";
	string func4 = "char *smsgtmodes(int nedb);\n";
	
	ifstream inFile;
	inFile.open("libsms_wrap.c");
	
	if(!inFile) {
		cout<<"Can not find file: libsms_wrap.c"<<endl;
		exit(1);
	}
	
	ofstream outFile;
	outFile.open("libsms_wrap_temp.c");
	
	outFile<<func1;
	outFile<<func2;
	outFile<<func3;
	outFile<<func4;
	
	string input_line;
	
	while(!inFile.eof()){
		getline(inFile, input_line);
		outFile<<input_line<<endl;
	}
	
	system("cp libsms_wrap_temp.c libsms_wrap.c");
	return 0;
}
