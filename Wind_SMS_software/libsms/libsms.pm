# This file was automatically generated by SWIG
package libsms;
require Exporter;
require DynaLoader;
@ISA = qw(Exporter DynaLoader);
package libsmsc;
bootstrap libsms;
package libsms;
@EXPORT = qw( );

# ---------- BASE METHODS -------------

package libsms;

sub TIEHASH {
    my ($classname,$obj) = @_;
    return bless $obj, $classname;
}

sub CLEAR { }

sub FIRSTKEY { }

sub NEXTKEY { }

sub FETCH {
    my ($self,$field) = @_;
    my $member_func = "swig_${field}_get";
    $self->$member_func();
}

sub STORE {
    my ($self,$field,$newval) = @_;
    my $member_func = "swig_${field}_set";
    $self->$member_func($newval);
}

sub this {
    my $ptr = shift;
    return tied(%$ptr);
}


# ------- FUNCTION WRAPPERS --------

package libsms;

*smsOpenFile = *libsmsc::smsOpenFile;
*smsCloseFile = *libsmsc::smsCloseFile;
*smsReadCycle = *libsmsc::smsReadCycle;
*smsDecommHDB = *libsmsc::smsDecommHDB;
*smsDecompress = *libsmsc::smsDecompress;
*smsGetBits = *libsmsc::smsGetBits;
*smssdbg = *libsmsc::smssdbg;
*smssetdbg = *libsmsc::smssetdbg;
*smsstrace = *libsmsc::smsstrace;
*smssettrace = *libsmsc::smssettrace;
*smssinccycover = *libsmsc::smssinccycover;
*smsgnedb = *libsmsc::smsgnedb;
*smsgbitrate = *libsmsc::smsgbitrate;
*smssqualover = *libsmsc::smssqualover;
*smsgedbqual = *libsmsc::smsgedbqual;
*smsgver = *libsmsc::smsgver;
*smsgtime = *libsmsc::smsgtime;
*smsgtimes = *libsmsc::smsgtimes;
*smsgyear = *libsmsc::smsgyear;
*smsgdoy = *libsmsc::smsgdoy;
*smsghms = *libsmsc::smsghms;
*smsgss1970 = *libsmsc::smsgss1970;
*smsIsLeapYear = *libsmsc::smsIsLeapYear;
*smsIncrementDate = *libsmsc::smsIncrementDate;
*smsdedbhdr = *libsmsc::smsdedbhdr;
*smsgscirec = *libsmsc::smsgscirec;
*smsgrevcount = *libsmsc::smsgrevcount;
*smsgxfsr = *libsmsc::smsgxfsr;
*smsgxdvs = *libsmsc::smsgxdvs;
*smsgxdvstab = *libsmsc::smsgxdvstab;
*smsgxnpha = *libsmsc::smsgxnpha;
*smsdxpha = *libsmsc::smsdxpha;
*smsgxenergy = *libsmsc::smsgxenergy;
*smsgxeoq = *libsmsc::smsgxeoq;
*smsgxtof = *libsmsc::smsgxtof;
*smsgxessd = *libsmsc::smsgxessd;
*smsgxmass = *libsmsc::smsgxmass;
*smsgxmoq = *libsmsc::smsgxmoq;
*smsgxeqtab = *libsmsc::smsgxeqtab;
*smscxdvs2eq = *libsmsc::smscxdvs2eq;
*smsgtmpvtab = *libsmsc::smsgtmpvtab;
*smsgtmpvtab_raw = *libsmsc::smsgtmpvtab_raw;
*smsgtmnvtab = *libsmsc::smsgtmnvtab;
*smsgtmnvtab_raw = *libsmsc::smsgtmnvtab_raw;
*smsgthmr = *libsmsc::smsgthmr;
*smsgtsmr = *libsmsc::smsgtsmr;
*smsgtbr0 = *libsmsc::smsgtbr0;
*smsgtbr1 = *libsmsc::smsgtbr1;
*smsgtbr2 = *libsmsc::smsgtbr2;
*smsgtomr = *libsmsc::smsgtomr;
*smsgtfsr = *libsmsc::smsgtfsr;
*smsgtrsr = *libsmsc::smsgtrsr;
*smsgtdcr = *libsmsc::smsgtdcr;
*smsgttcr = *libsmsc::smsgttcr;
*smsgtssd = *libsmsc::smsgtssd;
*smsgtdvs = *libsmsc::smsgtdvs;
*smsgtdvstab = *libsmsc::smsgtdvstab;
*smsgtmode = *libsmsc::smsgtmode;
*smsgtmodes = *libsmsc::smsgtmodes;
*smsgtnpha = *libsmsc::smsgtnpha;
*smsdtpha = *libsmsc::smsdtpha;
*smsgtstopid = *libsmsc::smsgtstopid;
*smsgted = *libsmsc::smsgted;
*smsgtsector = *libsmsc::smsgtsector;
*smsgtssdid = *libsmsc::smsgtssdid;
*smsgttofd = *libsmsc::smsgttofd;
*smsgtstart = *libsmsc::smsgtstart;
*smsgtrange = *libsmsc::smsgtrange;
*smsgteoq = *libsmsc::smsgteoq;
*smsgttof = *libsmsc::smsgttof;
*smsgtessd = *libsmsc::smsgtessd;
*smsgtmass = *libsmsc::smsgtmass;
*smsgtmoq = *libsmsc::smsgtmoq;
*smsgtnm = *libsmsc::smsgtnm;
*smsgtnq = *libsmsc::smsgtnq;
*smsgteqtab = *libsmsc::smsgteqtab;
*smsctdvs2eq = *libsmsc::smsctdvs2eq;
*smsgmmr = *libsmsc::smsgmmr;
*smsgmbr0_s = *libsmsc::smsgmbr0_s;
*smsgmbr0_ns = *libsmsc::smsgmbr0_ns;
*smsgmbr1_s = *libsmsc::smsgmbr1_s;
*smsgmbr1_ns = *libsmsc::smsgmbr1_ns;
*smsgmfsr = *libsmsc::smsgmfsr;
*smsgmfsrb = *libsmsc::smsgmfsrb;
*smsgmmdcr = *libsmsc::smsgmmdcr;
*smsgmufsr_s = *libsmsc::smsgmufsr_s;
*smsgmufsr_ns = *libsmsc::smsgmufsr_ns;
*smsgmrsr_s = *libsmsc::smsgmrsr_s;
*smsgmrsr_ns = *libsmsc::smsgmrsr_ns;
*smsgmdcr_s = *libsmsc::smsgmdcr_s;
*smsgmdcr_ns = *libsmsc::smsgmdcr_ns;
*smsgmmfsr = *libsmsc::smsgmmfsr;
*smsgmdvs = *libsmsc::smsgmdvs;
*smsgmdvstab = *libsmsc::smsgmdvstab;
*smsgmcalmode = *libsmsc::smsgmcalmode;
*smsgmnpha = *libsmsc::smsgmnpha;
*smsgmeqtab = *libsmsc::smsgmeqtab;
*smscmdvs2eq = *libsmsc::smscmdvs2eq;
*smsgmsector = *libsmsc::smsgmsector;
*smsgmrange = *libsmsc::smsgmrange;
*smsgmanode = *libsmsc::smsgmanode;
*smsgmtof = *libsmsc::smsgmtof;
*smsgmtofns = *libsmsc::smsgmtofns;
*smsgmmass = *libsmsc::smsgmmass;
*smsgmmoq = *libsmsc::smsgmmoq;
*smsgchk = *libsmsc::smsgchk;
*smsghdb = *libsmsc::smsghdb;
*smsgdtempsc = *libsmsc::smsgdtempsc;
*smsgdtemp = *libsmsc::smsgdtemp;
*smsgxaetempsc = *libsmsc::smsgxaetempsc;
*smsgxssdtemp = *libsmsc::smsgxssdtemp;
*smsgmaetempsc = *libsmsc::smsgmaetempsc;
*smsgmaetemp = *libsmsc::smsgmaetemp;
*smsgtssdtempsc = *libsmsc::smsgtssdtempsc;
*smsgtaetempsc = *libsmsc::smsgtaetempsc;
*smsgtaetemp = *libsmsc::smsgtaetemp;
*smsgtssdtemp = *libsmsc::smsgtssdtemp;

# ------- VARIABLE STUBS --------

package libsms;

1;