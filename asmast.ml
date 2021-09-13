(* This is a very naive and simplified AST which represents
   only things we care about at the moment.
 *)


type directive = | File of int*string*string
                 | Location of int*int*int
                 | LocationExt of int*int*int*string
                 | LocationExt1 of int*int*int*string*int
                 | OtherDir of string
                                 [@@deriving show]

type instruction =
  | CheriInstr of string
  | OtherInstr of string
                   [@@deriving show]

type statement = | Directive of directive
         | Instr of instruction
         | Label of string
                      [@@deriving show]

type program = statement list
                 [@@deriving show]
