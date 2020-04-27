
//*********************Header Guard
#ifndef WTDCLV2_H
#define WTDCLV2_H

//*********************Include any dependencies throughout all the pieces
#include <string>
#include <math.h>
#include <vector>

//**********************Define structures to be used
struct effRecord {
  double eoqEff;
  double startEff;
  double stopEff;
  double tripleEff;
};

struct STICSRecord {
  double year;
  double doy;
  double sector;
  double EStep;
  double eoq;
  double vel;
  std::string ion;
  double df;
};

struct moments {
	double f0;
	double f1;
	double f2;
	double n;
	double v_0;
	double v_th;
	double n_err;
	double v_0_err;
	double v_th_err;
};

//structure for the moment file broken into sectors
struct momentsBySector {
	int sector;
	double f0;
	double f0_err;
	double f1;
	double f1_err;
	double f2;
	double f2_err;
	
	momentsBySector()
	{
		sector = 0;
		f0 = 0.0;
		f0_err = 0.0;
		f1 = 0.0;
		f1_err = 0.0;
		f2 = 0.0;
		f2_err = 0.0;
	}
};

//structure for the SWE file
struct SWERecord {
  double year;
  double day;
  double fracday;
  double Vx;
  double Vy;
  double Vz;
};

//********************Define any global constants
const double PI = 4.0*atan(1.0);
const double SEC_IN_DAY = 24.0*60.0*60.0;

#endif
