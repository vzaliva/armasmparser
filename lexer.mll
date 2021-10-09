{
  open Parser
  exception Error of string

  let next_line lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
    { pos with
      Lexing.pos_bol  = pos.Lexing.pos_cnum;
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1
    }

  open Utils
}

let digit = ['0' - '9']

let identfirstchar = ['A'-'Z' 'a'-'z' '_' '.']
let identchar = ['A'-'Z' 'a'-'z' '_' '0'-'9' '.']

let hexchar = ['0'-'9' 'a'-'f']

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let comment = "//"

rule prog = parse
 | '"'
 { let buffer = Buffer.create 10 in
   dbg "STRING" ; STRING (stringl buffer lexbuf)
 }
    
 | ".addrsig" { dbg "ADDRSIG "; DIR_ADDRSIG }
 | ".addrsig_sym" { dbg "ADDRSIG_SYM"; DIR_ADDRSIG_SYM }
 | ".ascii" { dbg "ASCII"; DIR_ASCII }
 | ".asciz" { dbg "ASCIZ"; DIR_ASCIZ }
 | ".byte" { dbg "BYTE"; DIR_BYTE }
 | ".cfi_def_cfa" { dbg "CFI_DEF_CFA"; DIR_CFI_DEF_CFA }
 | ".cfi_def_cfa_offset" { dbg "CFI_DEF_CFA_OFFSET"; DIR_CFI_DEF_CFA_OFFSET }
 | ".cfi_endproc" { dbg "CFI_ENDPROC"; DIR_CFI_ENDPROC }
 | ".cfi_offset" { dbg "CFI_OFFSET"; DIR_CFI_OFFSET }
 | ".cfi_startproc" { dbg "CFI_STARTPROC"; DIR_CFI_STARTPROC }
 | ".cfi_sections" { dbg "CFI_SECTIONS"; DIR_CFI_SECTIONS }
 | ".chericap" { dbg "CHERICAP"; DIR_CHERICAP }
 | ".comm" { dbg "COMM"; DIR_COMM }
 | ".file" { dbg "FILE"; DIR_FILE }
 | ".globl" { dbg "GLOBL"; DIR_GLOBL }
 | ".hword" { dbg "HWORD"; DIR_HWORD }
 | ".ident" { dbg "IDENT"; DIR_IDENT }
 | ".loc" { dbg "LOC"; DIR_LOC }
 | ".local" { dbg "LOCAL"; DIR_LOCAL }
 | ".p2align" { dbg "P2ALIGN"; DIR_P2ALIGN }
 | ".section" { dbg "SECTION"; DIR_SECTION }
 | ".size" { dbg "SIZE"; DIR_SIZE }
 | ".text" { dbg "TEXT"; DIR_TEXT }
 | ".data" { dbg "DATA"; DIR_DATA }
 | ".type" { dbg "TYPE"; DIR_TYPE }
 | ".word" { dbg "WORD"; DIR_WORD }
 | ".xword" { dbg "XWORD"; DIR_XWORD }

(* shifting  *)
 | "lsl" { dbg "LSL"; LSL }
 | "lsr" { dbg "LSR"; LSR }
 | "asr" { dbg "ASR"; ASR }
 | "ror" { dbg "ROR"; ROR }
(* exending *)
 | "uxtb" { dbg "UXTB";  UXTB}
 | "sxtb" { dbg "SXTB";  SXTB}
 | "uxth" { dbg "UXTH";  UXTH}
 | "sxth" { dbg "SXTH";  SXTH}
 | "uxtw" { dbg "UXTW";  UXTW}
 | "sxtw" { dbg "SXTW";  SXTW}

 | "add" { dbg "ADD"; INST_ADD }
 | "adds" { dbg "ADDS"; INST_ADDS }
 | "adr" { dbg "ADR"; INST_ADR }
 | "adrp" { dbg "ADRP"; INST_ADRP }
 | "and" { dbg "AND"; INST_AND }
 | "ands" { dbg "ANDS"; INST_ANDS }
 | "b" { dbg "B"; INST_B_EQ }
 | "b.eq" { dbg "B.EQ"; INST_B_EQ }
 | "b.ne" { dbg "B.NE"; INST_B_NE }
 | "b.gt" { dbg "B.GT"; INST_B_GT }
 | "b.ge" { dbg "B.GE"; INST_B_GE }
 | "b.lt" { dbg "B.LT"; INST_B_LT }
 | "b.le" { dbg "B.LE"; INST_B_LE }
 | "b.hi" { dbg "B.HI"; INST_B_HI }
 | "bfi" { dbg "BFI"; INST_BFI }
 | "bic" { dbg "BIC"; INST_BIC }
 | "bl" { dbg "BL"; INST_BL }
 | "blr" { dbg "BLR"; INST_BLR }
 | "br" { dbg "BR"; INST_BR }
 | "cbnz" { dbg "CBNZ"; INST_CBNZ }
 | "cbz" { dbg "CBZ"; INST_CBZ }
 | "cinc" { dbg "CINC"; INST_CINC }
 | "clrperm" { dbg "CLRPERM"; INST_CLRPERM }
 | "clrtag" { dbg "CLRTAG"; INST_CLRTAG }
 | "cneg" { dbg "CNEG"; INST_CNEG }
 | "csel" { dbg "CSEL"; INST_CSEL }
 | "cset" { dbg "CSET"; INST_CSET }
 | "csinc" { dbg "CSINC"; INST_CSINC }
 | "eor" { dbg "EOR"; INST_EOR }
 | "fadd" { dbg "FADD"; INST_FADD }
 | "fcvt" { dbg "FCVT"; INST_FCVT }
 | "fdiv" { dbg "FDIV"; INST_FDIV }
 | "fmov" { dbg "FMOV"; INST_FMOV }
 | "fmul" { dbg "FMUL"; INST_FMUL }
 | "fsub" { dbg "FSUB"; INST_FSUB }
 | "gcvalue" { dbg "GCVALUE"; INST_GCVALUE }
 | "ldp" { dbg "LDP"; INST_LDP }
 | "ldr" { dbg "LDR"; INST_LDR }
 | "ldrb" { dbg "LDRB"; INST_LDRB }
 | "ldrh" { dbg "LDRH"; INST_LDRH }
 | "ldrsw" { dbg "LDRSW"; INST_LDRSW }
 | "ldur" { dbg "LDUR"; INST_LDUR }
 | "ldursw" { dbg "LDURSW"; INST_LDURSW }
 | "mov" { dbg "MOV"; INST_MOV }
 | "movi" { dbg "MOVI"; INST_MOVI }
 | "movk" { dbg "MOVK"; INST_MOVK }
 | "mrs" { dbg "MRS"; INST_MRS }
 | "mul" { dbg "MUL"; INST_MUL }
 | "mvn" { dbg "MVN"; INST_MVN }
 | "orn" { dbg "ORN"; INST_ORN }
 | "orr" { dbg "ORR"; INST_ORR }
 | "ret" { dbg "RET"; INST_RET }
 | "sbfiz" { dbg "SBFIZ"; INST_SBFIZ }
 | "scbnds" { dbg "SCBNDS"; INST_SCBNDS }
 | "scbndse" { dbg "SCBNDSE"; INST_SCBNDSE }
 | "scvtf" { dbg "SCVTF"; INST_SCVTF }
 | "sdiv" { dbg "SDIV"; INST_SDIV }
 | "seal" { dbg "SEAL"; INST_SEAL }
 | "smaddl" { dbg "SMADDL"; INST_SMADDL }
 | "stp" { dbg "STP"; INST_STP }
 | "str" { dbg "STR"; INST_STR }
 | "strb" { dbg "STRB"; INST_STRB }
 | "strh" { dbg "STRH"; INST_STRH }
 | "stur" { dbg "STUR"; INST_STUR }
 | "sub" { dbg "SUB"; INST_SUB }
 | "subs" { dbg "SUBS"; INST_SUBS }
 | "tbnz" { dbg "TBNZ"; INST_TBNZ }
 | "tbz" { dbg "TBZ"; INST_TBZ }
 | "ubfx" { dbg "UBFX"; INST_UBFX }
 | "udiv" { dbg "UDIV"; INST_UDIV }

 | ':' (identchar+ as s) ':' { dbg "RPREFIX"; RPREFIX s}

 | ((identfirstchar identchar+) as l) { dbg "IDENT" ; IDENT l }

 | comment ([^ '\r' '\n']+)  { dbg "<comment>" ; prog lexbuf }

 | '@' ['a'-'z']+ { dbg "@SECTION"; SECTION }

 | newline { dbg "EOL\n" ; next_line lexbuf; EOL }
 | white { prog lexbuf } (* skip *)
 | digit+ as i { dbg "UINT" ;  UINT (int_of_string i) }
 | "0x" (hexchar+ as s) { dbg "HEX" ;  HEX s }
 | eof { dbg "EOF" ; EOF }

 | '!'  { dbg "EXCL"     ; EXCL     }
 | ','  { dbg "COMMA"    ; COMMA    }
 | '-'  { dbg "MINUS"    ; MINUS    }
 | '+'  { dbg "PLUS"     ; PLUS     }
 | '*'  { dbg "ASTERISK" ; ASTERISK }
 | '/'  { dbg "SLASH"    ; SLASH    }
 | '%'  { dbg "PERCENT"  ; PERCENT  }
 | "."  { dbg "DOT"      ; DOT      }
 | ":"  { dbg "COLON"    ; COLON    }
 | '('  { dbg "LPAREN"   ; LPAREN   }
 | ')'  { dbg "RPAREN"   ; RPAREN   }
 | '['  { dbg "LBRACKET" ; LBRACKET }
 | ']'  { dbg "RBRACKET" ; RBRACKET }
 | "#"  { dbg "HASH"     ; HASH     }
 | _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

and stringl buffer = parse
| '"'
    { Buffer.contents buffer }
| _ as c
    { Buffer.add_char buffer c;
      stringl buffer lexbuf }
