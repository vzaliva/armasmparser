%{
open Asmast
%}


%token EXCL
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
%token DIR_CFI_SECTIONS
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
%token DIR_DATA
%token DIR_TYPE
%token DIR_WORD
%token DIR_XWORD

%token INST_ADD
%token INST_ADDS
%token INST_ADR
%token INST_ADRP
%token INST_AND
%token INST_ANDS
%token INST_B
%token INST_B_EQ
%token INST_B_NE
%token INST_B_GT
%token INST_B_GE
%token INST_B_LT
%token INST_B_LE
%token INST_B_HI
%token INST_BFI
%token INST_BIC
%token INST_BL
%token INST_BLR
%token INST_BR
%token INST_CBNZ
%token INST_CBZ
%token INST_CINC
%token INST_CLRPERM
%token INST_CLRTAG
%token INST_CNEG
%token INST_CSEL
%token INST_CSET
%token INST_CSINC
%token INST_EOR
%token INST_FADD
%token INST_FCVT
%token INST_FDIV
%token INST_FMOV
%token INST_FMUL
%token INST_FSUB
%token INST_GCVALUE
%token INST_LDP
%token INST_LDR
%token INST_LDRB
%token INST_LDRH
%token INST_LDRSW
%token INST_LDUR
%token INST_LDURSW
%token INST_MOV
%token INST_MOVI
%token INST_MOVK
%token INST_MRS
%token INST_MUL
%token INST_MVN
%token INST_ORN
%token INST_ORR
%token INST_RET
%token INST_SBFIZ
%token INST_SCBNDS
%token INST_SCBNDSE
%token INST_SCVTF
%token INST_SDIV
%token INST_SEAL
%token INST_SMADDL
%token INST_STP
%token INST_STR
%token INST_STRB
%token INST_STRH
%token INST_STUR
%token INST_SUB
%token INST_SUBS
%token INST_TBNZ
%token INST_TBZ
%token INST_UBFX
%token INST_UDIV

%token LSL
%token LSR
%token ASR
%token ROR

%token UXTB
%token SXTB
%token UXTH
%token SXTH
%token UXTW
%token SXTW

                        
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

cfi_directive:
  | DIR_CFI_DEF_CFA n=IDENT COMMA v=i_int
    {
        Cfi_def_cfa (n,v)
    }
  | DIR_CFI_DEF_CFA_OFFSET v=i_int
    {
      Cfi_def_cfa_offset v
    }
  | DIR_CFI_ENDPROC
    {
        Cfi_end_proc
    }
  | DIR_CFI_OFFSET n=IDENT COMMA v=i_int
    {
        Cfi_offset (n, v)
    }
  | DIR_CFI_STARTPROC i=IDENT
    {
        Cfi_start_proc (Some i)
    }
  | DIR_CFI_STARTPROC
    {
        Cfi_start_proc None
    }
  | DIR_CFI_SECTIONS l=separated_list(COMMA,IDENT)
    {
        Cfi_sections l
    }
                
directive:
  | d=cfi_directive
    {
      CFI d
    }
  | DIR_FILE n=i_uint dir=STRING fname=STRING IDENT? HEX?
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
  | DIR_DATA
    {
      OtherDir "DATA"
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
 

literalarg:     
  | HASH HEX {()}
  | HASH i_int {()}

simplearg:
  | RPREFIX IDENT {()}
  | IDENT {()}
  | literalarg {()}

operand:
  | LSL literalarg {()}
  | LSR literalarg {()}
  | ASR literalarg {()}
  | ROR literalarg {()}
  | UXTB literalarg? {()}
  | SXTB literalarg? {()}
  | UXTH literalarg? {()}
  | SXTH literalarg? {()}
  | UXTW literalarg? {()}
  | SXTW literalarg? {()}
  | simplearg {()}
                
arg:
  | operand {()}
  | LBRACKET separated_list(COMMA,operand) RBRACKET EXCL? {()}

instruction:
 | INST_ADD separated_list(COMMA,arg) { OtherInstr "ADD" }
 | INST_ADDS separated_list(COMMA,arg) { OtherInstr "ADDS" }
 | INST_ADR separated_list(COMMA,arg) { OtherInstr "ADR" }
 | INST_ADRP separated_list(COMMA,arg) { OtherInstr "ADRP" }
 | INST_AND separated_list(COMMA,arg) { OtherInstr "AND" }
 | INST_ANDS separated_list(COMMA,arg) { OtherInstr "ANDS" }
 | ASR separated_list(COMMA,arg) { OtherInstr "ASR" }
 | INST_B separated_list(COMMA,arg) { OtherInstr "B" }
 | INST_B_EQ separated_list(COMMA,arg) { OtherInstr "B.EQ" }
 | INST_B_NE separated_list(COMMA,arg) { OtherInstr "B.NE" }
 | INST_B_GT separated_list(COMMA,arg) { OtherInstr "B.GT" }
 | INST_B_GE separated_list(COMMA,arg) { OtherInstr "B.GE" }
 | INST_B_LT separated_list(COMMA,arg) { OtherInstr "B.LT" }
 | INST_B_LE separated_list(COMMA,arg) { OtherInstr "B.LE" }
 | INST_B_HI separated_list(COMMA,arg) { OtherInstr "B.HI" }
 | INST_BFI separated_list(COMMA,arg) { OtherInstr "BFI" }
 | INST_BIC separated_list(COMMA,arg) { OtherInstr "BIC" }
 | INST_BL separated_list(COMMA,arg) { OtherInstr "BL" }
 | INST_BLR separated_list(COMMA,arg) { OtherInstr "BLR" }
 | INST_BR separated_list(COMMA,arg) { OtherInstr "BR" }
 | INST_CBNZ separated_list(COMMA,arg) { OtherInstr "CBNZ" }
 | INST_CBZ separated_list(COMMA,arg) { OtherInstr "CBZ" }
 | INST_CINC separated_list(COMMA,arg) { OtherInstr "CINC" }
 | INST_CLRPERM separated_list(COMMA,arg) { OtherInstr "CLRPERM" }
 | INST_CLRTAG separated_list(COMMA,arg) { OtherInstr "CLRTAG" }
 | INST_CNEG separated_list(COMMA,arg) { OtherInstr "CNEG" }
 | INST_CSEL separated_list(COMMA,arg) { OtherInstr "CSEL" }
 | INST_CSET separated_list(COMMA,arg) { OtherInstr "CSET" }
 | INST_CSINC separated_list(COMMA,arg) { OtherInstr "CSINC" }
 | INST_EOR separated_list(COMMA,arg) { OtherInstr "EOR" }
 | INST_FADD separated_list(COMMA,arg) { OtherInstr "FADD" }
 | INST_FCVT separated_list(COMMA,arg) { OtherInstr "FCVT" }
 | INST_FDIV separated_list(COMMA,arg) { OtherInstr "FDIV" }
 | INST_FMOV separated_list(COMMA,arg) { OtherInstr "FMOV" }
 | INST_FMUL separated_list(COMMA,arg) { OtherInstr "FMUL" }
 | INST_FSUB separated_list(COMMA,arg) { OtherInstr "FSUB" }
 | INST_GCVALUE separated_list(COMMA,arg) { OtherInstr "GCVALUE" }
 | INST_LDP separated_list(COMMA,arg) { OtherInstr "LDP" }
 | INST_LDR separated_list(COMMA,arg) { OtherInstr "LDR" }
 | INST_LDRB separated_list(COMMA,arg) { OtherInstr "LDRB" }
 | INST_LDRH separated_list(COMMA,arg) { OtherInstr "LDRH" }
 | INST_LDRSW separated_list(COMMA,arg) { OtherInstr "LDRSW" }
 | INST_LDUR separated_list(COMMA,arg) { OtherInstr "LDUR" }
 | INST_LDURSW separated_list(COMMA,arg) { OtherInstr "LDURSW" }
 | LSL separated_list(COMMA,arg) { OtherInstr "LSL" }
 | LSR separated_list(COMMA,arg) { OtherInstr "LSR" }
 | INST_MOV separated_list(COMMA,arg) { OtherInstr "MOV" }
 | INST_MOVI separated_list(COMMA,arg) { OtherInstr "MOVI" }
 | INST_MOVK separated_list(COMMA,arg) { OtherInstr "MOVK" }
 | INST_MRS separated_list(COMMA,arg) { OtherInstr "MRS" }
 | INST_MUL separated_list(COMMA,arg) { OtherInstr "MUL" }
 | INST_MVN separated_list(COMMA,arg) { OtherInstr "MVN" }
 | INST_ORN separated_list(COMMA,arg) { OtherInstr "ORN" }
 | INST_ORR separated_list(COMMA,arg) { OtherInstr "ORR" }
 | INST_RET separated_list(COMMA,arg) { OtherInstr "RET" }
 | INST_SBFIZ separated_list(COMMA,arg) { OtherInstr "SBFIZ" }
 | INST_SCBNDS separated_list(COMMA,arg) { OtherInstr "SCBNDS" }
 | INST_SCBNDSE separated_list(COMMA,arg) { OtherInstr "SCBNDSE" }
 | INST_SCVTF separated_list(COMMA,arg) { OtherInstr "SCVTF" }
 | INST_SDIV separated_list(COMMA,arg) { OtherInstr "SDIV" }
 | INST_SEAL separated_list(COMMA,arg) { OtherInstr "SEAL" }
 | INST_SMADDL separated_list(COMMA,arg) { OtherInstr "SMADDL" }
 | INST_STP separated_list(COMMA,arg) { OtherInstr "STP" }
 | INST_STR separated_list(COMMA,arg) { OtherInstr "STR" }
 | INST_STRB separated_list(COMMA,arg) { OtherInstr "STRB" }
 | INST_STRH separated_list(COMMA,arg) { OtherInstr "STRH" }
 | INST_STUR separated_list(COMMA,arg) { OtherInstr "STUR" }
 | INST_SUB separated_list(COMMA,arg) { OtherInstr "SUB" }
 | INST_SUBS separated_list(COMMA,arg) { OtherInstr "SUBS" }
 | SXTW separated_list(COMMA,arg) { OtherInstr "SXTW" }
 | INST_TBNZ separated_list(COMMA,arg) { OtherInstr "TBNZ" }
 | INST_TBZ separated_list(COMMA,arg) { OtherInstr "TBZ" }
 | INST_UBFX separated_list(COMMA,arg) { OtherInstr "UBFX" }
 | INST_UDIV separated_list(COMMA,arg) { OtherInstr "UDIV" }

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

