#include <iostream>
#include <fstream>
#include <string>
#include <cstdlib>
#include <sstream>

using namespace std;

int main(){
	system("getconf LONG_BIT > sys_bit");
	
	ifstream inFile;
	inFile.open("sys_bit");
	
	int sys_arch = 0;
	inFile>>sys_arch;
	
	inFile.close();
	
	ostringstream cmd;
	cmd<<"make -f libsms_swig.mk FC3 BIT="<<sys_arch;
	
	system(cmd.str().c_str());
	
	return 0;
}
