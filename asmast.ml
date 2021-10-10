(* This is a very naive and simplified AST which represents
   only things we care about at the moment.
 *)

type cfi_directive = | Cfi_start_proc of string option
                     | Cfi_def_cfa of string*int
                     | Cfi_def_cfa_offset of int
                     | Cfi_offset of string*int
                     | Cfi_end_proc
                     | Cfi_sections  of string list
                                          [@@deriving show]

type directive = | File of int*string*string
                 | Location of int*int*int
                 | LocationExt of int*int*int*string
                 | LocationExt1 of int*int*int*string*int
                 | CFI of cfi_directive
                 | OtherDir of string
                                 [@@deriving show]

type instruction =
  | CheriInstr of string
  | OtherInstr of string
                    [@@deriving show]

type position = [%import: Lexing.position] [@@deriving show]

type statement =
  | Directive of (position * position) * directive
  | Instr of (position * position) * instruction
  | Label of (position * position) * string
               [@@deriving show]

type program = statement list
                 [@@deriving show]
