// TESTTERM.C
// Demonstrates new API functions UserErrTerm and SetDump
// Requires _MAYDAY.C support module
// Compile as register-based, stack-checking turned off for _MAYDAY.C
// Link via: WLINK system causeway file testterm,_mayday

#include <stdio.h>
#include <string.h>
#include <i86.h>

// Note: struct InfoListStruct has been changed for CauseWay v5.0!
//    - segment registers ds,es,fs,gs are 32-bit
//    - cr1 has been removed

#pragma pack( push, 1 )
struct InfoListStruct {
	unsigned int ebp;
	unsigned int edi;
	unsigned int esi;
	unsigned int edx;
	unsigned int ecx;
	unsigned int ebx;
	unsigned int eax;
	unsigned int gs;
	unsigned int fs;
	unsigned int es;
	unsigned int ds;
	unsigned int eip;
	short unsigned int cs;
	short unsigned int reserved1;
	unsigned int eflags;
	unsigned int esp;
	short unsigned int ss;
	short unsigned int reserved2;
	short unsigned int tr;
	unsigned int cr0;
	unsigned int cr2;
	unsigned int cr3;
	unsigned int csAddr;
	unsigned int dsAddr;
	unsigned int esAddr;
	unsigned int fsAddr;
	unsigned int gsAddr;
	unsigned int ssAddr;
	short unsigned int Exception;
	unsigned int Code;
};
#pragma pack( pop )

extern struct InfoListStruct InfoList;

void far __loadds Mayday(void);

int main(void)
{
	setup();
	croaker();
	return(0);
}

// setup the vector to Mayday() routine when croaker() faults
void setup(void)
{
	struct SREGS sregs;
	union REGS regs;

	segread(&sregs);
	sregs.ds=FP_SEG(Mayday);	// vector address
	regs.x.esi=FP_OFF(Mayday);
	sregs.es=FP_SEG(&InfoList);	// info dump address
	regs.x.edi=FP_OFF(&InfoList);
	regs.h.cl=1;	// 32-bit routine
	regs.w.ax=0xff31;	// UserErrTerm
	int386x(0x31,&regs,&regs,&sregs);
	regs.w.ax=0xff30;	// SetDump
	regs.h.cl=0;
	int386(0x31,&regs,&regs);
}

void croaker(void)
{
    int i,peekaboo=0x12345678;

    printf("%d ",*(int *)peekaboo);     // if this one doesn't fault,
    peekaboo=0x87654321;                // then
    printf("%d\n",*(int *)peekaboo);    // this one will
}
