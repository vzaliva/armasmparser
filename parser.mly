%{
open Asmast
%}

%token COMMA
%token MINUS
%token DOT
%token COLON
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token HASH  
%token EOF
%token EOL
%token SECTION

%token DIR_ADDRSIG
%token DIR_ADDRSIG_SYM
%token DIR_ASCII
%token DIR_ASCIZ
%token DIR_BYTE
%token DIR_CFI_DEF_CFA
%token DIR_CFI_DEF_CFA_OFFSET
%token DIR_CFI_ENDPROC
%token DIR_CFI_OFFSET
%token DIR_CFI_STARTPROC
%token DIR_CHERICAP
%token DIR_COMM
%token DIR_FILE
%token DIR_GLOBL
%token DIR_HWORD
%token DIR_IDENT
%token DIR_LOC
%token DIR_LOCAL
%token DIR_P2ALIGN
%token DIR_SECTION
%token DIR_SIZE
%token DIR_TEXT
%token DIR_TYPE
%token DIR_WORD
%token DIR_XWORD


%token INST_ADD
%token INST_ADRP
%token INST_B
%token INST_BL
%token INST_B_NE
%token INST_CLRPERM
%token INST_CLRTAG
%token INST_LDP
%token INST_LDR
%token INST_LDRB
%token INST_LDUR
%token INST_LSL
%token INST_MOV
%token INST_RET
%token INST_SCBNDS
%token INST_SCBNDSE
%token INST_STP
%token INST_STR
%token INST_STRB
%token INST_STRH
%token INST_SUB
%token INST_SUBS

%token <string> HEX
%token <int>    UINT
%token <string> IDENT
%token <string> STRING
%token <string> OTHERDIR
%token <string> RPREFIX

%start <Asmast.program> prog 

%%

i_uint:
  | x=UINT { x }

i_int:
  | x=UINT { x }
  | MINUS x = UINT { -x }

expr:
  | IDENT { () }
  | IDENT MINUS IDENT { () }
  | i_int  { () }
  | i_uint { () }

directive:
  | DIR_FILE n=i_uint dir=STRING fname=STRING IDENT HEX
    {
      File (n,dir,fname)
    }
  | DIR_FILE STRING
    {
      OtherDir "file"
    }
  | DIR_LOC f=i_uint l=i_uint c=i_uint n=IDENT
    {
      LocationExt (f,l,c,n)
    }
  | DIR_LOC f=i_uint l=i_uint c=i_uint n=IDENT x=i_uint
    {
      LocationExt1 (f,l,c,n,x)
    }
  | DIR_LOC f=i_uint l=i_uint c=i_uint
    {
      Location (f,l,c)
    }
  | DIR_ADDRSIG 
    {
      OtherDir "ADDRSIG"
    }
  | DIR_ADDRSIG_SYM IDENT
    {
      OtherDir "ADDRSIG_SYM"
    }
  | DIR_ASCII STRING
    {
      OtherDir "ASCII"
    }
  | DIR_ASCIZ STRING
    {
      OtherDir "ASCIZ"
    }
  | DIR_BYTE i_uint
    {
      OtherDir "BYTE"
    }
  | DIR_CFI_DEF_CFA IDENT COMMA i_int
    {
      OtherDir "CFI_DEF_CFA"
    }
  | DIR_CFI_DEF_CFA_OFFSET 
    {
      OtherDir "CFI_DEF_CFA_OFFSET"
    }
  | DIR_CFI_ENDPROC
    {
      OtherDir "CFI_ENDPROC"
    }
  | DIR_CFI_OFFSET IDENT COMMA i_int
    {
      OtherDir "CFI_OFFSET"
    }
  | DIR_CFI_STARTPROC IDENT
    {
      OtherDir "CFI_STARTPROC"
    }
  | DIR_CHERICAP IDENT
    {
      OtherDir "CHERICAP"
    }
  | DIR_COMM
    {
      OtherDir "COMM"
    }
  | DIR_GLOBL IDENT
    {
      OtherDir "GLOBL"
    }
  | DIR_HWORD expr
    {
      OtherDir "HWORD"
    }
  | DIR_IDENT STRING
    {
      OtherDir "IDENT"
    }
  | DIR_LOCAL
    {
      OtherDir "LOCAL"
    }
  | DIR_P2ALIGN UINT
    {
      OtherDir "P2ALIGN"
    }
  | DIR_SECTION IDENT COMMA STRING COMMA SECTION COMMA i_uint
    {
      OtherDir "SECTION"
    }
  | DIR_SECTION IDENT COMMA STRING COMMA SECTION
    {
      OtherDir "SECTION"
    }
  | DIR_SECTION STRING COMMA STRING COMMA SECTION
    {
      OtherDir "SECTION"
    }
  | DIR_SIZE IDENT COMMA expr
    {
      OtherDir "SIZE"
    }
  | DIR_TEXT
    {
      OtherDir "TEXT"
    }
  | DIR_TYPE IDENT COMMA SECTION
    {
      OtherDir "TYPE"
    }
  | DIR_WORD expr
    {
      OtherDir "WORD"
    }
  | DIR_XWORD expr
    {
      OtherDir "XWORD"
    }
 

simplearg:
  | RPREFIX IDENT {()}
  | IDENT {()}
  | HASH i_int {()}

arg:
  | simplearg
  | LBRACKET separated_list(COMMA,simplearg) RBRACKET {()}

instruction:
 | INST_ADD separated_list(COMMA,arg) { OtherInstr "ADD" }
 | INST_ADRP separated_list(COMMA,arg) { OtherInstr "ADRP" }
 | INST_B separated_list(COMMA,arg) { OtherInstr "B" }
 | INST_BL separated_list(COMMA,arg) { OtherInstr "BL" }
 | INST_B_NE separated_list(COMMA,arg) { OtherInstr "B.NE" }
 | INST_CLRPERM separated_list(COMMA,arg) { OtherInstr "CLRPERM" }
 | INST_CLRTAG separated_list(COMMA,arg) { OtherInstr "CLRTAG" }
 | INST_LDP separated_list(COMMA,arg) { OtherInstr "LDP" }
 | INST_LDR separated_list(COMMA,arg) { OtherInstr "LDR" }
 | INST_LDRB separated_list(COMMA,arg) { OtherInstr "LDRB" }
 | INST_LDUR separated_list(COMMA,arg) { OtherInstr "LDUR" }
 | INST_LSL separated_list(COMMA,arg) { OtherInstr "LSL" }
 | INST_MOV separated_list(COMMA,arg) { OtherInstr "MOV" }
 | INST_RET separated_list(COMMA,arg) { OtherInstr "RET" }
 | INST_SCBNDS separated_list(COMMA,arg) { CheriInstr "INST_SCBNDS" }
 | INST_SCBNDSE separated_list(COMMA,arg) { CheriInstr "INST_SCBNDSE" }
 | INST_STP separated_list(COMMA,arg) { OtherInstr "STP" }
 | INST_STR separated_list(COMMA,arg) { OtherInstr "STR" }
 | INST_STRB separated_list(COMMA,arg) { OtherInstr "STRB" }
 | INST_STRH separated_list(COMMA,arg) { OtherInstr "STRH" }
 | INST_SUB separated_list(COMMA,arg)    { OtherInstr "SUB" }
 | INST_SUBS separated_list(COMMA,arg) { OtherInstr "SUBS" }

statement:
  | l = IDENT COLON EOL+  { Label l }
  | i = instruction EOL+ { Instr i }
  | d = directive EOL+
    {
      Directive d
    }

prog: v = list(statement) EOF
    {
      v
    }

