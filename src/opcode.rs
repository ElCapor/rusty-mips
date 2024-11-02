use strum::FromRepr;
/*AUTOGEN*/
#[derive(FromRepr, Debug, PartialEq)]
#[repr(u8)]
pub enum OPCODE {
	HALT=0,
	ADD=1,
	ADDU=2,
	ADDI=3,
	ADDIU=4,
	SUB=5,
	SUBU=6,
	SUBI=7,
	SUBIU=8,
	DIV=9,
	DIVU=10,
	MULT=11,
	MULTU=12,
	AND=13,
	ANDI=14,
	OR=15,
	NOR=16,
	ORI=17,
	XOR=18,
	XORI=19,
	LW=20,
	SW=21,
	BEQ=22,
	BNEQ=23,
}