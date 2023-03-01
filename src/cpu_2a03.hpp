//this code was initially made in 2006 as 6507 emulator
//it is stripped down version without tracking timings etc

typedef union 
{
	unsigned short hl;
#ifdef LOW_ENDIAN
	struct { unsigned char h,l; };
#else
	struct { unsigned char l,h; };
#endif
} regPair;



struct cpuStruct {
	unsigned char A;
	unsigned char X;
	unsigned char Y;
	unsigned char S;
	unsigned char P;
	regPair PC;
	bool jam;
};

cpuStruct CPU;


#define AC 		CPU.A
#define XR		CPU.X
#define YR		CPU.Y
#define SR		CPU.S
#define PR		CPU.P
#define PCH		CPU.PC.h
#define PCL		CPU.PC.l
#define PCW		CPU.PC.hl

#define	FLG_C	1
#define	FLG_Z	2
#define	FLG_I	4
#define	FLG_D	8
#define	FLG_B	16
#define	FLG_R	32
#define	FLG_V	64
#define	FLG_S	128



//установка флагов SZ по результатам операции

#define PR_SET_SZ(val)	{ PR&=~(FLG_S|FLG_Z); PR|=((val)&128)|((val)?0:FLG_Z); }

//чтение адреса/операнда по разным схемам адресации

#define READ_VAL_IMM()  mem_rd(PCW+1)
#define READ_ADR_ABS()	{ adr.l=mem_rd(PCW+1); adr.h=mem_rd(PCW+2); }
#define READ_ADR_ABX()	{ READ_ADR_ABS(); adr.hl+=XR; }
#define READ_ADR_ABY()	{ READ_ADR_ABS(); adr.hl+=YR; }
#define READ_ADR_ZPG()	{ adr.l=READ_VAL_IMM(); adr.h=0; }
#define READ_ADR_ZPX()	{ READ_ADR_ZPG(); adr.hl+=XR; adr.h=0; }
#define READ_ADR_ZPY()	{ READ_ADR_ZPG(); adr.hl+=YR; adr.h=0; }
#define READ_ADR_IDX()	{ adr.l=READ_VAL_IMM()+XR; adr.h=0; adr.hl=mem_rd(adr.hl)+(mem_rd(adr.hl+1)<<8); }
#define READ_ADR_IDY()	{ READ_ADR_ZPG(); off=adr.hl; adr.l=mem_rd(off); adr.h=mem_rd((off+1)&0xff); adr.hl+=YR; }

//работа с регистром флагов

#define SEC()	{ PR|=FLG_C;  PCW++; }
#define SED()	{ PR|=FLG_D;  PCW++; }
#define SEI()	{ PR|=FLG_I;  PCW++; }
#define CLC()	{ PR&=~FLG_C; PCW++; }
#define CLD()	{ PR&=~FLG_D; PCW++; }
#define CLI()	{ PR&=~FLG_I; PCW++; }
#define CLV()	{ PR&=~FLG_V; PCW++; }

//перепихивание в регистрах

#define TAX()	{ XR=AC; PR_SET_SZ(XR); PCW++; }
#define TAY()	{ YR=AC; PR_SET_SZ(YR); PCW++; }
#define TXA()	{ AC=XR; PR_SET_SZ(AC); PCW++; }
#define TYA()	{ AC=YR; PR_SET_SZ(AC); PCW++; }
#define TSX()	{ XR=SR; PR_SET_SZ(XR); PCW++; }
#define TXS()	{ SR=XR;                PCW++; }

//загрузка регистров

#define LDR_OP(x)	{ x=mem_rd(adr.hl); PR_SET_SZ(x); }

#define LDA_IMM()	{ adr.hl=PCW+1;   LDR_OP(AC); PCW+=2; }
#define LDA_ZPG()	{ READ_ADR_ZPG(); LDR_OP(AC); PCW+=2; }
#define LDA_ZPX()	{ READ_ADR_ZPX(); LDR_OP(AC); PCW+=2; }
#define LDA_ABS()	{ READ_ADR_ABS(); LDR_OP(AC); PCW+=3; }
#define LDA_ABX()	{ READ_ADR_ABX(); LDR_OP(AC); PCW+=3; }
#define LDA_ABY()	{ READ_ADR_ABY(); LDR_OP(AC); PCW+=3; }
#define LDA_IDX()	{ READ_ADR_IDX(); LDR_OP(AC); PCW+=2; }
#define LDA_IDY()	{ READ_ADR_IDY(); LDR_OP(AC); PCW+=2; }

#define LDX_IMM()	{ adr.hl=PCW+1;   LDR_OP(XR); PCW+=2; }
#define LDX_ZPG()	{ READ_ADR_ZPG(); LDR_OP(XR); PCW+=2; }
#define LDX_ZPY()	{ READ_ADR_ZPY(); LDR_OP(XR); PCW+=2; }
#define LDX_ABS()	{ READ_ADR_ABS(); LDR_OP(XR); PCW+=3; }
#define LDX_ABY()	{ READ_ADR_ABY(); LDR_OP(XR); PCW+=3; }

#define LDY_IMM()	{ adr.hl=PCW+1;   LDR_OP(YR); PCW+=2; }
#define LDY_ZPG()	{ READ_ADR_ZPG(); LDR_OP(YR); PCW+=2; }
#define LDY_ZPX()	{ READ_ADR_ZPX(); LDR_OP(YR); PCW+=2; }
#define LDY_ABS()	{ READ_ADR_ABS(); LDR_OP(YR); PCW+=3; }
#define LDY_ABX()	{ READ_ADR_ABX(); LDR_OP(YR); PCW+=3; }

//сохранение регистров

#define STR_OP(x)	{ mem_wr(adr.hl,x); }

#define STA_ZPG()	{ READ_ADR_ZPG(); STR_OP(AC); PCW+=2; }
#define STA_ZPX()	{ READ_ADR_ZPX(); STR_OP(AC); PCW+=2; }
#define STA_ABS()	{ READ_ADR_ABS(); STR_OP(AC); PCW+=3; }
#define STA_ABX()	{ READ_ADR_ABX(); STR_OP(AC); PCW+=3; }
#define STA_ABY()	{ READ_ADR_ABY(); STR_OP(AC); PCW+=3; }
#define STA_IDX()	{ READ_ADR_IDX(); STR_OP(AC); PCW+=2; }
#define STA_IDY()	{ READ_ADR_IDY(); STR_OP(AC); PCW+=2; }

#define STX_ZPG()	{ READ_ADR_ZPG(); STR_OP(XR); PCW+=2; }
#define STX_ZPY()	{ READ_ADR_ZPY(); STR_OP(XR); PCW+=2; }
#define STX_ABS()	{ READ_ADR_ABS(); STR_OP(XR); PCW+=3; }

#define STY_ZPG()	{ READ_ADR_ZPG(); STR_OP(YR); PCW+=2; }
#define STY_ZPX()	{ READ_ADR_ZPX(); STR_OP(YR); PCW+=2; }
#define STY_ABS()	{ READ_ADR_ABS(); STR_OP(YR); PCW+=3; }

//инкременты-декременты

#define DEC_OP(x)	{ x--; PR_SET_SZ(x); }
#define INC_OP(x)	{ x++; PR_SET_SZ(x); }

#define DEX()		{ DEC_OP(XR); PCW++; }
#define DEY()		{ DEC_OP(YR); PCW++; }
#define DEC_ZPG()	{ READ_ADR_ZPG(); ph=mem_rd(adr.hl); DEC_OP(ph); mem_wr(adr.hl,ph); PCW+=2; }
#define DEC_ZPX()	{ READ_ADR_ZPX(); ph=mem_rd(adr.hl); DEC_OP(ph); mem_wr(adr.hl,ph); PCW+=2; }
#define DEC_ABS()	{ READ_ADR_ABS(); ph=mem_rd(adr.hl); DEC_OP(ph); mem_wr(adr.hl,ph); PCW+=3; }
#define DEC_ABX()	{ READ_ADR_ABX(); ph=mem_rd(adr.hl); DEC_OP(ph); mem_wr(adr.hl,ph); PCW+=3; }


#define INX()		{ INC_OP(XR); PCW++; }
#define INY()		{ INC_OP(YR); PCW++; }
#define INC_ZPG()	{ READ_ADR_ZPG(); ph=mem_rd(adr.hl); INC_OP(ph); mem_wr(adr.hl,ph); PCW+=2; }
#define INC_ZPX()	{ READ_ADR_ZPX(); ph=mem_rd(adr.hl); INC_OP(ph); mem_wr(adr.hl,ph); PCW+=2; }
#define INC_ABS()	{ READ_ADR_ABS(); ph=mem_rd(adr.hl); INC_OP(ph); mem_wr(adr.hl,ph); PCW+=3; }
#define INC_ABX()	{ READ_ADR_ABX(); ph=mem_rd(adr.hl); INC_OP(ph); mem_wr(adr.hl,ph); PCW+=3; }


//логические операции

#define AND_OP(x)	{ AC&=x; PR_SET_SZ(AC); }
#define ORA_OP(x)	{ AC|=x; PR_SET_SZ(AC); }
#define EOR_OP(x)	{ AC^=x; PR_SET_SZ(AC); }

#define AND_IMM()	{ AND_OP(READ_VAL_IMM());                 PCW+=2; }
#define AND_ZPG()	{ READ_ADR_ZPG(); AND_OP(mem_rd(adr.hl)); PCW+=2; }
#define AND_ZPX()	{ READ_ADR_ZPX(); AND_OP(mem_rd(adr.hl)); PCW+=2; }
#define AND_ABS()	{ READ_ADR_ABS(); AND_OP(mem_rd(adr.hl)); PCW+=3; }
#define AND_ABX()	{ READ_ADR_ABX(); AND_OP(mem_rd(adr.hl)); PCW+=3; }
#define AND_ABY()	{ READ_ADR_ABY(); AND_OP(mem_rd(adr.hl)); PCW+=3; }
#define AND_IDX()	{ READ_ADR_IDX(); AND_OP(mem_rd(adr.hl)); PCW+=2; }
#define AND_IDY()	{ READ_ADR_IDY(); AND_OP(mem_rd(adr.hl)); PCW+=2; }

#define ORA_IMM()	{ ORA_OP(READ_VAL_IMM());                 PCW+=2; }
#define ORA_ZPG()	{ READ_ADR_ZPG(); ORA_OP(mem_rd(adr.hl)); PCW+=2; }
#define ORA_ZPX()	{ READ_ADR_ZPX(); ORA_OP(mem_rd(adr.hl)); PCW+=2; }
#define ORA_ABS()	{ READ_ADR_ABS(); ORA_OP(mem_rd(adr.hl)); PCW+=3; }
#define ORA_ABX()	{ READ_ADR_ABX(); ORA_OP(mem_rd(adr.hl)); PCW+=3; }
#define ORA_ABY()	{ READ_ADR_ABY(); ORA_OP(mem_rd(adr.hl)); PCW+=3; }
#define ORA_IDX()	{ READ_ADR_IDX(); ORA_OP(mem_rd(adr.hl)); PCW+=2; }
#define ORA_IDY()	{ READ_ADR_IDY(); ORA_OP(mem_rd(adr.hl)); PCW+=2; }

#define EOR_IMM()	{ EOR_OP(READ_VAL_IMM());                 PCW+=2; }
#define EOR_ZPG()	{ READ_ADR_ZPG(); EOR_OP(mem_rd(adr.hl)); PCW+=2; }
#define EOR_ZPX()	{ READ_ADR_ZPX(); EOR_OP(mem_rd(adr.hl)); PCW+=2; }
#define EOR_ABS()	{ READ_ADR_ABS(); EOR_OP(mem_rd(adr.hl)); PCW+=3; }
#define EOR_ABX()	{ READ_ADR_ABX(); EOR_OP(mem_rd(adr.hl)); PCW+=3; }
#define EOR_ABY()	{ READ_ADR_ABY(); EOR_OP(mem_rd(adr.hl)); PCW+=3; }
#define EOR_IDX()	{ READ_ADR_IDX(); EOR_OP(mem_rd(adr.hl)); PCW+=2; }
#define EOR_IDY()	{ READ_ADR_IDY(); EOR_OP(mem_rd(adr.hl)); PCW+=2; }

//стек

#define PUSH(val)	{ mem_wr(0x100|SR,val); SR--; }
#define PULL(val)	{ SR++; val=mem_rd(0x100|SR); }

#define PHA()		{ PUSH(AC); PCW++; }
#define PHP()		{ PUSH(PR); PCW++; }
#define PLA()		{ PULL(AC); PCW++; }
#define PLP()		{ PULL(PR); PCW++; }

//безусловные переходы

#define JSR()		{ PCW+=2; PUSH(PCH); PUSH(PCL); adr.l=mem_rd(PCW-1); adr.h=mem_rd(PCW); PCW=adr.hl; }
#define RTS()		{ PULL(PCL); PULL(PCH); PCW++; }
#define RTI()		{ PULL(PR); PULL(PCL); PULL(PCH); PCW++; }
#define JMP_ABS()	{ READ_ADR_ABS(); PCW=adr.hl; }
#define JMP_IDR()	{ READ_ADR_ABS(); PCL=mem_rd(adr.hl); adr.l++; PCH=mem_rd(adr.hl); }

//бранчи (условные переходы)

#define BRANCH(cond)	{ if(cond) { off=READ_VAL_IMM(); PCW+=2; if((off&128)) PCW-=((off^0xff)+1); else PCW+=off; } else { PCW+=2; }}

#define BCS()		{ BRANCH((PR&FLG_C)); }
#define BEQ()		{ BRANCH((PR&FLG_Z)); }
#define BMI()		{ BRANCH((PR&FLG_S)); }
#define BVS()		{ BRANCH((PR&FLG_V)); }
#define BCC()		{ BRANCH(!(PR&FLG_C)); }
#define BNE()		{ BRANCH(!(PR&FLG_Z)); }
#define BPL()		{ BRANCH(!(PR&FLG_S)); }
#define BVC()		{ BRANCH(!(PR&FLG_V)); }

//сдвиги

#define ASL_OP(x)	{ if((x&128)) PR|=FLG_C; else PR&=~FLG_C; x<<=1; PR_SET_SZ(x); }
#define LSR_OP(x)	{ if((x&1))   PR|=FLG_C; else PR&=~FLG_C; x>>=1; PR_SET_SZ(x); }
#define ROL_OP(x)	{ pr=PR; if((x&128)) PR|=FLG_C; else PR&=~FLG_C; x<<=1; if(pr&FLG_C) x|=1;   PR_SET_SZ(x); }
#define ROR_OP(x)	{ pr=PR; if((x&1))   PR|=FLG_C; else PR&=~FLG_C; x>>=1; if(pr&FLG_C) x|=128; PR_SET_SZ(x); }

#define ASL_ACC()	{ ASL_OP(AC); PCW+=1; }
#define ASL_ZPG()	{ READ_ADR_ZPG(); ph=mem_rd(adr.hl); ASL_OP(ph); mem_wr(adr.hl,ph); PCW+=2; }
#define ASL_ZPX()	{ READ_ADR_ZPX(); ph=mem_rd(adr.hl); ASL_OP(ph); mem_wr(adr.hl,ph); PCW+=2; }
#define ASL_ABS()	{ READ_ADR_ABS(); ph=mem_rd(adr.hl); ASL_OP(ph); mem_wr(adr.hl,ph); PCW+=3; }
#define ASL_ABX()	{ READ_ADR_ABX(); ph=mem_rd(adr.hl); ASL_OP(ph); mem_wr(adr.hl,ph); PCW+=3; }

#define LSR_ACC()	{ LSR_OP(AC); PCW+=1; }
#define LSR_ZPG()	{ READ_ADR_ZPG(); ph=mem_rd(adr.hl); LSR_OP(ph); mem_wr(adr.hl,ph); PCW+=2; }
#define LSR_ZPX()	{ READ_ADR_ZPX(); ph=mem_rd(adr.hl); LSR_OP(ph); mem_wr(adr.hl,ph); PCW+=2; }
#define LSR_ABS()	{ READ_ADR_ABS(); ph=mem_rd(adr.hl); LSR_OP(ph); mem_wr(adr.hl,ph); PCW+=3; }
#define LSR_ABX()	{ READ_ADR_ABX(); ph=mem_rd(adr.hl); LSR_OP(ph); mem_wr(adr.hl,ph); PCW+=3; }

#define ROL_ACC()	{ ROL_OP(AC); PCW+=1; }
#define ROL_ZPG()	{ READ_ADR_ZPG(); ph=mem_rd(adr.hl); ROL_OP(ph); mem_wr(adr.hl,ph); PCW+=2; }
#define ROL_ZPX()	{ READ_ADR_ZPX(); ph=mem_rd(adr.hl); ROL_OP(ph); mem_wr(adr.hl,ph); PCW+=2; }
#define ROL_ABS()	{ READ_ADR_ABS(); ph=mem_rd(adr.hl); ROL_OP(ph); mem_wr(adr.hl,ph); PCW+=3; }
#define ROL_ABX()	{ READ_ADR_ABX(); ph=mem_rd(adr.hl); ROL_OP(ph); mem_wr(adr.hl,ph); PCW+=3; }

#define ROR_ACC()	{ ROR_OP(AC); PCW+=1; }
#define ROR_ZPG()	{ READ_ADR_ZPG(); ph=mem_rd(adr.hl); ROR_OP(ph); mem_wr(adr.hl,ph); PCW+=2; }
#define ROR_ZPX()	{ READ_ADR_ZPX(); ph=mem_rd(adr.hl); ROR_OP(ph); mem_wr(adr.hl,ph); PCW+=2; }
#define ROR_ABS()	{ READ_ADR_ABS(); ph=mem_rd(adr.hl); ROR_OP(ph); mem_wr(adr.hl,ph); PCW+=3; }
#define ROR_ABX()	{ READ_ADR_ABX(); ph=mem_rd(adr.hl); ROR_OP(ph); mem_wr(adr.hl,ph); PCW+=3; }

//операции побитовых сравнений

#define BIT_OP(x)	{ PR&=~(FLG_S|FLG_V|FLG_Z); if((x&128)) PR|=FLG_S; if((x&64)) PR|=FLG_V; ph=AC&x; if(ph==0) PR|=FLG_Z; }

#define BIT_ZPG()	{ READ_ADR_ZPG(); ph=mem_rd(adr.hl); BIT_OP(ph); PCW+=2; }
#define BIT_ABS()	{ READ_ADR_ABS(); ph=mem_rd(adr.hl); BIT_OP(ph); PCW+=3; }

//сравнения

#define CMP_OP(x)	{ ph=x; PR&=~FLG_C; if(AC>=ph) PR|=FLG_C; ph=AC-x; PR_SET_SZ(ph); }
#define CPX_OP(x)	{ ph=x; PR&=~FLG_C; if(XR>=ph) PR|=FLG_C; ph=XR-x; PR_SET_SZ(ph); }
#define CPY_OP(x)	{ ph=x; PR&=~FLG_C; if(YR>=ph) PR|=FLG_C; ph=YR-x; PR_SET_SZ(ph); }

#define CMP_IMM()	{                 CMP_OP(READ_VAL_IMM()); PCW+=2; }
#define CMP_ZPG()	{ READ_ADR_ZPG(); CMP_OP(mem_rd(adr.hl)); PCW+=2; }
#define CMP_ZPX()	{ READ_ADR_ZPX(); CMP_OP(mem_rd(adr.hl)); PCW+=2; }
#define CMP_ABS()	{ READ_ADR_ABS(); CMP_OP(mem_rd(adr.hl)); PCW+=3; }
#define CMP_ABX()	{ READ_ADR_ABX(); CMP_OP(mem_rd(adr.hl)); PCW+=3; }
#define CMP_ABY()	{ READ_ADR_ABY(); CMP_OP(mem_rd(adr.hl)); PCW+=3; }
#define CMP_IDX()	{ READ_ADR_IDX(); CMP_OP(mem_rd(adr.hl)); PCW+=2; }
#define CMP_IDY()	{ READ_ADR_IDY(); CMP_OP(mem_rd(adr.hl)); PCW+=2; }

#define CPX_IMM()	{                 CPX_OP(READ_VAL_IMM()); PCW+=2; }
#define CPX_ZPG()	{ READ_ADR_ZPG(); CPX_OP(mem_rd(adr.hl)); PCW+=2; }
#define CPX_ABS()	{ READ_ADR_ABS(); CPX_OP(mem_rd(adr.hl)); PCW+=3; }

#define CPY_IMM()	{                 CPY_OP(READ_VAL_IMM()); PCW+=2; }
#define CPY_ZPG()	{ READ_ADR_ZPG(); CPY_OP(mem_rd(adr.hl)); PCW+=2; }
#define CPY_ABS()	{ READ_ADR_ABS(); CPY_OP(mem_rd(adr.hl)); PCW+=3; }

//математика (с поддержкой decimal mode)

#define ADC_OP(x)	{ alu=AC+x+((PR&FLG_C)?1:0); if((alu&0xff00)) PR|=FLG_C; else PR&=~FLG_C; \
					if((AC&128)!=(alu&128)) PR|=FLG_V; else PR&=~FLG_V; AC=alu&0xff; PR_SET_SZ(AC); } \
		
#define SBC_OP(x)	{ alu=AC-x-((PR&FLG_C)?0:1); if((alu&0xff00)) PR&=~FLG_C; else PR|=FLG_C; \
					if((AC&128)!=(alu&128)) PR|=FLG_V; else PR&=~FLG_V; AC=alu&0xff; PR_SET_SZ(AC); }

#define ADC_IMM()	{                 ph=READ_VAL_IMM(); ADC_OP(ph); PCW+=2; }
#define ADC_ZPG()	{ READ_ADR_ZPG(); ph=mem_rd(adr.hl); ADC_OP(ph); PCW+=2; }
#define ADC_ZPX()	{ READ_ADR_ZPX(); ph=mem_rd(adr.hl); ADC_OP(ph); PCW+=2; }
#define ADC_ABS()	{ READ_ADR_ABS(); ph=mem_rd(adr.hl); ADC_OP(ph); PCW+=3; }
#define ADC_ABX()	{ READ_ADR_ABX(); ph=mem_rd(adr.hl); ADC_OP(ph); PCW+=3; }
#define ADC_ABY()	{ READ_ADR_ABY(); ph=mem_rd(adr.hl); ADC_OP(ph); PCW+=3; }
#define ADC_IDX()	{ READ_ADR_IDX(); ph=mem_rd(adr.hl); ADC_OP(ph); PCW+=2; }
#define ADC_IDY()	{ READ_ADR_IDY(); ph=mem_rd(adr.hl); ADC_OP(ph); PCW+=2; }

#define SBC_IMM()	{                 SBC_OP(READ_VAL_IMM()); PCW+=2; }
#define SBC_ZPG()	{ READ_ADR_ZPG(); SBC_OP(mem_rd(adr.hl)); PCW+=2; }
#define SBC_ZPX()	{ READ_ADR_ZPX(); SBC_OP(mem_rd(adr.hl)); PCW+=2; }
#define SBC_ABS()	{ READ_ADR_ABS(); SBC_OP(mem_rd(adr.hl)); PCW+=3; }
#define SBC_ABX()	{ READ_ADR_ABX(); SBC_OP(mem_rd(adr.hl)); PCW+=3; }
#define SBC_ABY()	{ READ_ADR_ABY(); SBC_OP(mem_rd(adr.hl)); PCW+=3; }
#define SBC_IDX()	{ READ_ADR_IDX(); SBC_OP(mem_rd(adr.hl)); PCW+=2; }
#define SBC_IDY()	{ READ_ADR_IDY(); SBC_OP(mem_rd(adr.hl)); PCW+=2; }

//разные операции

#define BRK()		{ CPU.jam=true; }//adr.hl=PCW+2; PUSH(adr.h); PUSH(adr.l); PR|=FLG_B; PUSH(PR); PCW++; }
#define NOP()		{ PCW++; }

//недокументированные операции - разные

#define JAM()		{ CPU.jam=true; }

#define DOP(x)		{ PCW+=2; }
#define TOP_IMM()	{ PCW+=3; }
#define TOP_ABX()	{ READ_ADR_ABX(); PCW+=3; }

//недокументированные операции - загрузка регистров

#define LAX_OP()	{ AC=mem_rd(adr.hl); XR=AC; PR_SET_SZ(AC); }

#define LAX_ZPG()	{ READ_ADR_ZPG(); LAX_OP(); PCW+=2; }
#define LAX_ZPY()	{ READ_ADR_ZPY(); LAX_OP(); PCW+=2; }
#define LAX_ABS()	{ READ_ADR_ABS(); LAX_OP(); PCW+=3; }
#define LAX_ABY()	{ READ_ADR_ABY(); LAX_OP(); PCW+=3; }
#define LAX_IDX()	{ READ_ADR_IDX(); LAX_OP(); PCW+=2; }
#define LAX_IDY()	{ READ_ADR_IDY(); LAX_OP(); PCW+=2; }

//недокументированные операции - декремент/сравнение

#define DCP_ZPG()	{ READ_ADR_ZPG(); ph=mem_rd(adr.hl); ph--; mem_wr(adr.hl,ph); CMP_OP(ph); PCW+=2; }
#define DCP_ZPX()	{ READ_ADR_ZPX(); ph=mem_rd(adr.hl); ph--; mem_wr(adr.hl,ph); CMP_OP(ph); PCW+=2; }
#define DCP_ABS()	{ READ_ADR_ABS(); ph=mem_rd(adr.hl); ph--; mem_wr(adr.hl,ph); CMP_OP(ph); PCW+=3; }
#define DCP_ABX()	{ READ_ADR_ABX(); ph=mem_rd(adr.hl); ph--; mem_wr(adr.hl,ph); CMP_OP(ph);  PCW+=3; }
#define DCP_ABY()	{ READ_ADR_ABY(); ph=mem_rd(adr.hl); ph--; mem_wr(adr.hl,ph); CMP_OP(ph);  PCW+=3; }
#define DCP_IDX()	{ READ_ADR_IDX(); ph=mem_rd(adr.hl); ph--; mem_wr(adr.hl,ph); CMP_OP(ph); PCW+=2; }
#define DCP_IDY()	{ READ_ADR_IDY(); ph=mem_rd(adr.hl); ph--; mem_wr(adr.hl,ph); CMP_OP(ph); PCW+=2; }

//недокументированные операции - сдвиги

#define SLO_ZPG()	{ READ_ADR_ZPG(); ph=mem_rd(adr.hl); ASL_OP(ph); mem_wr(adr.hl,ph); ORA_OP(ph); PCW+=2; }
#define SLO_ZPX()	{ READ_ADR_ZPX(); ph=mem_rd(adr.hl); ASL_OP(ph); mem_wr(adr.hl,ph); ORA_OP(ph); PCW+=2; }
#define SLO_ABS()	{ READ_ADR_ABS(); ph=mem_rd(adr.hl); ASL_OP(ph); mem_wr(adr.hl,ph); ORA_OP(ph); PCW+=3; }
#define SLO_ABX()	{ READ_ADR_ABX(); ph=mem_rd(adr.hl); ASL_OP(ph); mem_wr(adr.hl,ph); ORA_OP(ph); PCW+=3; }
#define SLO_ABY()	{ READ_ADR_ABY(); ph=mem_rd(adr.hl); ASL_OP(ph); mem_wr(adr.hl,ph); ORA_OP(ph); PCW+=3; }
#define SLO_IDX()	{ READ_ADR_IDX(); ph=mem_rd(adr.hl); ASL_OP(ph); mem_wr(adr.hl,ph); ORA_OP(ph); PCW+=2; }
#define SLO_IDY()	{ READ_ADR_IDY(); ph=mem_rd(adr.hl); ASL_OP(ph); mem_wr(adr.hl,ph); ORA_OP(ph); PCW+=2; }

//недокументированные операции - логические операции

#define LAS_ABY()	{ READ_ADR_ABY(); AC=mem_rd(adr.hl)&SR; SR=AC; XR=AC; PR_SET_SZ(AC); PCW+=3; }



//код ядра

inline void cpu_reset(void)
{
	AC=0;
	XR=0;
	YR=0;
	PR=FLG_Z|FLG_R;
	SR=0xff;
	PCL=mem_rd(0xfffc);
	PCH=mem_rd(0xfffd);
	CPU.jam=false;
}


inline void cpu_tick(void)
{
	unsigned char ph,pr;
	short int off,alu;
	regPair adr;

	switch(mem_rd(PCW))
	{
	case 0x69: ADC_IMM();	break;
	case 0x65: ADC_ZPG();	break;
	case 0x75: ADC_ZPX();	break;
	case 0x6d: ADC_ABS();	break;
	case 0x7d: ADC_ABX();	break;
	case 0x79: ADC_ABY();	break;
	case 0x61: ADC_IDX();	break;
	case 0x71: ADC_IDY();	break;
		
	case 0x29: AND_IMM();	break;
	case 0x25: AND_ZPG();	break;
	case 0x35: AND_ZPX();	break;
	case 0x2d: AND_ABS();	break;
	case 0x3d: AND_ABX();	break;
	case 0x39: AND_ABY();	break;
	case 0x21: AND_IDX();	break;
	case 0x31: AND_IDY();	break;
		
	case 0x0a: ASL_ACC();	break;
	case 0x06: ASL_ZPG();	break;
	case 0x16: ASL_ZPX();	break;
	case 0x0e: ASL_ABS();	break;
	case 0x1e: ASL_ABX();	break;
		
	case 0x24: BIT_ZPG();	break;
	case 0x2c: BIT_ABS();	break;
		
	case 0x10: BPL();		break;
	case 0x30: BMI();		break;	
	case 0x50: BVC();		break;
	case 0x70: BVS();		break;
	case 0x90: BCC();		break;
	case 0xb0: BCS();		break;
	case 0xd0: BNE();		break;
	case 0xf0: BEQ();		break;
		
	case 0x00: BRK();		break;

	case 0xc9: CMP_IMM();	break;
	case 0xc5: CMP_ZPG();	break;
	case 0xd5: CMP_ZPX();	break;
	case 0xcd: CMP_ABS();	break;
	case 0xdd: CMP_ABX();	break;
	case 0xd9: CMP_ABY();	break;
	case 0xc1: CMP_IDX();	break;
	case 0xd1: CMP_IDY();	break;

	case 0xe0: CPX_IMM();	break;
	case 0xe4: CPX_ZPG();	break;
	case 0xec: CPX_ABS();	break;

	case 0xc0: CPY_IMM();	break;
	case 0xc4: CPY_ZPG();	break;
	case 0xcc: CPY_ABS();	break;
	
	case 0xc6: DEC_ZPG();	break;
	case 0xd6: DEC_ZPX();	break;
	case 0xce: DEC_ABS();	break;
	case 0xde: DEC_ABX();	break;
	
	case 0x49: EOR_IMM();	break;
	case 0x45: EOR_ZPG();	break;
	case 0x55: EOR_ZPX();	break;
	case 0x4d: EOR_ABS();	break;
	case 0x5d: EOR_ABX();	break;
	case 0x59: EOR_ABY();	break;
	case 0x41: EOR_IDX();	break;
	case 0x51: EOR_IDY();	break;

	case 0x18: CLC();		break;
	case 0x38: SEC();		break;
	case 0x58: CLI();		break;
	case 0x78: SEI();		break;
	case 0xb8: CLV();		break;
	case 0xd8: CLD();		break;
	case 0xf8: SED();		break;

	case 0xe6: INC_ZPG();	break;
	case 0xf6: INC_ZPX();	break;
	case 0xee: INC_ABS();	break;
	case 0xfe: INC_ABX();	break;

	case 0x4c: JMP_ABS();	break;
	case 0x6c: JMP_IDR();	break;

	case 0x20: JSR();		break;

	case 0xa9: LDA_IMM();	break;
	case 0xa5: LDA_ZPG();	break;
	case 0xb5: LDA_ZPX();	break;
	case 0xad: LDA_ABS();	break;
	case 0xbd: LDA_ABX();	break;
	case 0xb9: LDA_ABY();	break;
	case 0xa1: LDA_IDX();	break;
	case 0xb1: LDA_IDY();	break;

	case 0xa2: LDX_IMM();	break;
	case 0xa6: LDX_ZPG();	break;
	case 0xb6: LDX_ZPY();	break;
	case 0xae: LDX_ABS();	break;
	case 0xbe: LDX_ABY();	break;

	case 0xa0: LDY_IMM();	break;
	case 0xa4: LDY_ZPG();	break;
	case 0xb4: LDY_ZPX();	break;
	case 0xac: LDY_ABS();	break;
	case 0xbc: LDY_ABX();	break;

	case 0x4a: LSR_ACC();	break;
	case 0x46: LSR_ZPG();	break;
	case 0x56: LSR_ZPX();	break;
	case 0x4e: LSR_ABS();	break;
	case 0x5e: LSR_ABX();	break;

	case 0x09: ORA_IMM();	break;
	case 0x05: ORA_ZPG();	break;
	case 0x15: ORA_ZPX();	break;
	case 0x0d: ORA_ABS();	break;
	case 0x1d: ORA_ABX();	break;
	case 0x19: ORA_ABY();	break;
	case 0x01: ORA_IDX();	break;
	case 0x11: ORA_IDY();	break;
		
	case 0xaa: TAX();		break;	
	case 0x8a: TXA();		break;
	case 0xca: DEX();		break;
	case 0xe8: INX();		break;
	case 0xa8: TAY();		break;
	case 0x98: TYA();		break;
	case 0x88: DEY();		break;			
	case 0xc8: INY();		break;

	case 0x2a: ROL_ACC();	break;
	case 0x26: ROL_ZPG();	break;
	case 0x36: ROL_ZPX();	break;
	case 0x2e: ROL_ABS();	break;
	case 0x3e: ROL_ABX();	break;

	case 0x6a: ROR_ACC();	break;
	case 0x66: ROR_ZPG();	break;
	case 0x76: ROR_ZPX();	break;
	case 0x6e: ROR_ABS();	break;
	case 0x7e: ROR_ABX();	break;

	case 0x40: RTI();		break;
	case 0x60: RTS();		break;
	
	case 0xe9: SBC_IMM();	break;
	case 0xeb: SBC_IMM();	break;
	case 0xe5: SBC_ZPG();	break;
	case 0xf5: SBC_ZPX();	break;
	case 0xed: SBC_ABS();	break;
	case 0xfd: SBC_ABX();	break;	
	case 0xf9: SBC_ABY();	break;
	case 0xe1: SBC_IDX();	break;
	case 0xf1: SBC_IDY();	break;

	case 0x85: STA_ZPG();	break;
	case 0x95: STA_ZPX();	break;
	case 0x8d: STA_ABS();	break;
	case 0x9d: STA_ABX();	break;
	case 0x99: STA_ABY();	break;
	case 0x81: STA_IDX();	break;
	case 0x91: STA_IDY();	break;

	case 0x9a: TXS();		break;	
	case 0xba: TSX();		break;
	case 0x48: PHA();		break;
	case 0x68: PLA();		break;
	case 0x08: PHP();		break;
	case 0x28: PLP();		break;
	
	case 0x86: STX_ZPG();	break;
	case 0x96: STX_ZPY();	break;
	case 0x8e: STX_ABS();	break;
					
	case 0x84: STY_ZPG();	break;
	case 0x94: STY_ZPX();	break;
	case 0x8c: STY_ABS();	break;

	case 0xa7: LAX_ZPG();	break;
	case 0xb7: LAX_ZPY();	break;
	case 0xaf: LAX_ABS();	break;
	case 0xbf: LAX_ABY();	break;
	case 0xa3: LAX_IDX();	break;
	case 0xb3: LAX_IDY();	break;

	case 0x1a: NOP();		break;
	case 0x3a: NOP();		break;
	case 0x5a: NOP();		break;
	case 0x7a: NOP();		break;
	case 0xda: NOP();		break;
	case 0xfa: NOP();		break;
	case 0xea: NOP();		break;

	case 0x04: DOP(3);		break;
	case 0x14: DOP(4);		break;
	case 0x34: DOP(4);		break;
	case 0x44: DOP(3);		break;
	case 0x54: DOP(4);		break;
	case 0x64: DOP(3);		break;
	case 0x74: DOP(4);		break;
	case 0x80: DOP(2);		break;
	case 0x82: DOP(2);		break;
	case 0x89: DOP(2);		break;
	case 0xc2: DOP(2);		break;
	case 0xd4: DOP(4);		break;
	case 0xe2: DOP(2);		break;
	case 0xf4: DOP(4);		break;

	case 0x0c: TOP_IMM();	break;
	case 0x1c: TOP_ABX();	break;
	case 0x3c: TOP_ABX();	break;
	case 0x5c: TOP_ABX();	break;
	case 0x7c: TOP_ABX();	break;
	case 0xdc: TOP_ABX();	break;
	case 0xfc: TOP_ABX();	break;

	case 0xc7: DCP_ZPG();	break;
	case 0xd7: DCP_ZPX();	break;
	case 0xcf: DCP_ABS();	break;
	case 0xdf: DCP_ABX();	break;
	case 0xdb: DCP_ABY();	break;
	case 0xc3: DCP_IDX();	break;
	case 0xd3: DCP_IDY();	break;

	case 0x07: SLO_ZPG();	break;
	case 0x17: SLO_ZPX();	break;
	case 0x0f: SLO_ABS();	break;
	case 0x1f: SLO_ABX();	break;
	case 0x1b: SLO_ABY();	break;
	case 0x03: SLO_IDX();	break;
	case 0x13: SLO_IDY();	break;

	case 0x02: JAM();		break;
	case 0x12: JAM();		break;
	case 0x22: JAM();		break;
	case 0x32: JAM();		break;
	case 0x42: JAM();		break;
	case 0x52: JAM();		break;
	case 0x62: JAM();		break;
	case 0x72: JAM();		break;
	case 0x92: JAM();		break;
	case 0xb2: JAM();		break;
	case 0xd2: JAM();		break;
	case 0xf2: JAM();		break;

	case 0xbb: LAS_ABY();	break;

	default:
		break;
	}

	return;
}

