use strum::FromRepr;
/*AUTOGEN*/
#[derive(FromRepr, Debug, PartialEq)]
#[repr(u8)]
pub enum OPCODE {
	HALT=0,
	ADD=1,
	SUB=2,
	LW=3,
	SW=4,
	BEQ=5,
	ADDIB=6,
	ADDIW=7,
	ADDID=8,
	BNEQ=9,
}