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
 | ".type" { dbg "TYPE"; DIR_TYPE }
 | ".word" { dbg "WORD"; DIR_WORD }
 | ".xword" { dbg "XWORD"; DIR_XWORD }

 | "add" { dbg "ADD"; INST_ADD }
 | "adrp" { dbg "ADRP"; INST_ADRP }
 | "b" { dbg "B"; INST_B }
 | "bl" { dbg "BL"; INST_BL }
 | "b.ne" { dbg "B.NE"; INST_B_NE }
 | "clrperm" { dbg "CLRPERM"; INST_CLRPERM }
 | "clrtag" { dbg "CLRTAG"; INST_CLRTAG }
 | "ldp" { dbg "LDP"; INST_LDP }
 | "ldr" { dbg "LDR"; INST_LDR }
 | "ldrb" { dbg "LDRB"; INST_LDRB }
 | "ldur" { dbg "LDUR"; INST_LDUR }
 | "lsl" { dbg "LSL"; INST_LSL }
 | "mov" { dbg "MOV"; INST_MOV }
 | "ret" { dbg "RET"; INST_RET }
 | "scbnds" { dbg "SCBNDS"; INST_SCBNDS }
 | "scbndse" { dbg "SCBNDSE"; INST_SCBNDSE }
 | "stp" { dbg "STP"; INST_STP }
 | "str" { dbg "STR"; INST_STR }
 | "strb" { dbg "STRB"; INST_STRB }
 | "strh" { dbg "STRH"; INST_STRH }
 | "sub" { dbg "SUB"; INST_SUB }
 | "subs" { dbg "SUBS"; INST_SUBS }

 | ':' (identchar+ as s) ':' { dbg "RPREFIX"; RPREFIX s}

 | ((identfirstchar identchar+) as l) { dbg "IDENT" ; IDENT l }

 | comment ([^ '\r' '\n']+)  { dbg "<comment>" ; prog lexbuf }

 | '@' ['a'-'z']+ { dbg "@SECTION"; SECTION }

 | newline { dbg "EOL\n" ; next_line lexbuf; EOL }
 | white { prog lexbuf } (* skip *)
 | digit+ as i { dbg "UINT" ;  UINT (int_of_string i) }
 | "0x" (hexchar+ as s) { dbg "HEX" ;  HEX s }
 | eof { dbg "EOF" ; EOF }

 | ','  { dbg "COMMA   " ; COMMA    }
 | '-'  { dbg "MINUS   " ; MINUS    }
 | "."  { dbg "DOT     " ; DOT      }
 | ":"  { dbg "COLON   " ; COLON    }
 | '('  { dbg "LPAREN  " ; LPAREN   }
 | ')'  { dbg "RPAREN  " ; RPAREN   }
 | '['  { dbg "LBRACKET" ; LBRACKET }
 | ']'  { dbg "RBRACKET" ; RBRACKET }
 | "#"  { dbg "HASH    " ; HASH     }
 | _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

and stringl buffer = parse
| '"'
    { Buffer.contents buffer }
| _ as c
    { Buffer.add_char buffer c;
      stringl buffer lexbuf }
