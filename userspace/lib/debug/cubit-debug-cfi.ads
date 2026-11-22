------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Unwinding by call-frame information (.eh_frame), x86-64 (DBG-001).
--
--  @description
--  The table is built from the target's own .eh_frame bytes (from the
--  unstripped file, or read from the target through its grant when the debug
--  file only keeps a NOBITS copy). One Step turns a frame's registers into
--  its caller's, by the rules the compiler emitted for that PC.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Debug.Bytes;

package CuBit.Debug.Cfi is
   package B renames CuBit.Debug.Bytes;

   --  DWARF register numbers on x86-64 (System V psABI, 3.6.2).
   Register_Columns : constant := 17;
   type Register is range 0 .. Register_Columns - 1;
   RAX : constant Register := 0;
   RDX : constant Register := 1;
   RCX : constant Register := 2;
   RBX : constant Register := 3;
   RSI : constant Register := 4;
   RDI : constant Register := 5;
   RBP : constant Register := 6;
   RSP : constant Register := 7;
   R12 : constant Register := 12;
   R13 : constant Register := 13;
   R14 : constant Register := 14;
   R15 : constant Register := 15;
   Return_Address : constant Register := 16;   --  the RIP column

   type Register_Values is array (Register) of Unsigned_64;
   type Register_Known is array (Register) of Boolean;
   type Register_Set is record
      Value : Register_Values := [others => 0];
      Known : Register_Known := [others => False];
   end record;

   function Register_Name (R : Register) return String;

   FDE_Ceiling : constant := 65_536;
   type FDE_Entry is record
      Low, High : Unsigned_64 := 0;
      FDE_At, CIE_At : B.Offset := 0;     --  offsets in the section bytes
   end record;
   type FDE_Array is array (Natural range <>) of FDE_Entry;
   type FDE_Array_Access is access FDE_Array;

   type Table is record
      Data    : B.Buffer_Access;           --  the .eh_frame bytes (from 0)
      Address : Unsigned_64 := 0;          --  where the section is loaded
      FDEs    : FDE_Array_Access;
      Count   : Natural := 0;
   end record;
   type Table_Access is access Table;

   --  Data: the section's bytes, owned by the table afterwards.
   procedure Build (Data : B.Buffer_Access; Address : Unsigned_64; T : out Table_Access);
   procedure Free (T : in out Table_Access);

   --  Whether PC has unwind information.
   function Covers (T : Table; PC : Unsigned_64) return Boolean;

   --  Callee: the frame's registers (RIP in Return_Address, RSP known).
   --  Innermost: its RIP is where it is, not a return address (no -1).
   --  Caller: the calling frame's registers; CFA: this frame's canonical
   --  frame address. Ok False when there is no rule for this PC, or a
   --  needed register or memory word is not known.
   procedure Step
     (T : Table; Callee : Register_Set; Innermost : Boolean;
      Read : not null access procedure (Address : Unsigned_64; Value : out Unsigned_64; Ok : out Boolean);
      Caller : out Register_Set; CFA : out Unsigned_64; Ok : out Boolean);
end CuBit.Debug.Cfi;
