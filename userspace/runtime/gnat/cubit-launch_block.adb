------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  CuBit.Launch_Block - implementation
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Launch_Envelopes;
with CuBit.Startup_Tables;

package body CuBit.Launch_Block is

   package LE renames CuBit.Launch_Envelopes;
   package ST renames CuBit.Startup_Tables;

   --  Kept by the start code from the entry RDI (the whole block).
   Block_Length : Unsigned_64
   with Import, Convention => C, External_Name => "__cubit_launch_block_length";

   --  Where the kernel maps a launch block (kernel Process_Launch).
   Block_Address : constant := 16#0000_5A00_0000_0000#;

   Loaded  : CuBit.Startup_State.Load_Result := CuBit.Startup_State.Malformed;
   Present : Boolean := False;

   function Outcome return CuBit.Startup_State.Load_Result is (Loaded);
   function Table_Present return Boolean is (Present);

   procedure Load;
   procedure Load is
      Payload : LE.Block_Length;
      First   : Positive;
      Length  : LE.Table_Count;
   begin
      if Block_Length not in 1 .. LE.Maximum_Block_Bytes then
         return;
      end if;
      declare
         Mapped : constant LE.Block (1 .. Natural (Block_Length))
         with Import, Address => System'To_Address (Block_Address);
      begin
         LE.Split (Mapped, Payload, First, Length, Present);
         if not Present or else Length = 0 then
            Present := False;
            return;
         end if;
         --  One copy (single fetch): the table is decoded from it alone.
         declare
            Copy : constant ST.Bytes (1 .. Length) :=
              [for I in 1 .. Length => Mapped (First + I - 1)];
         begin
            CuBit.Startup_State.Load (Copy, Loaded);
         end;
      end;
   end Load;

begin
   Load;
end CuBit.Launch_Block;
