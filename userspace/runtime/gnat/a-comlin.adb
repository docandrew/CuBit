------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Launch_Arguments; use CuBit.Launch_Arguments;

package body Ada.Command_Line is

   --  Kept by the start code (userspace/c/crt0.S) from the entry RDI; the
   --  start code exits with Exit_Code once Exit_Code_Set is nonzero.
   Launch_Length : Unsigned_64
   with Import, Convention => C,
        External_Name => "__cubit_launch_arguments_length";
   Exit_Code : Unsigned_64
   with Import, Convention => C, External_Name => "__cubit_exit_status";
   Exit_Code_Set : Unsigned_64
   with Import, Convention => C, External_Name => "__cubit_exit_status_set";

   Checked : Boolean := False;
   Usable  : Boolean := False;
   Strings : String_Count := 0;    --  argument strings, the name included

   procedure Check;
   function Text (Index : Positive) return String;

   procedure Check is
   begin
      if Checked then
         return;
      end if;
      Checked := True;
      if Launch_Length in Header_Bytes .. Maximum_Block_Bytes then
         declare
            Item : constant Block (1 .. Natural (Launch_Length))
            with Import, Address => System'To_Address (Block_Address);
         begin
            if Validate (Item) = Valid then
               Usable := True;
               Strings := Arguments_Declared (Item);
            end if;
         end;
      end if;
   end Check;

   --  Argument string Index (1: the program name).
   function Text (Index : Positive) return String is
      Item : constant Block (1 .. Natural (Launch_Length))
      with Import, Address => System'To_Address (Block_Address);
      First : Positive;
      Last : Natural;
      Found : Boolean;
   begin
      Locate (Item, Index, First, Last, Found);
      if not Found then
         return "";
      end if;
      declare
         Result : String (1 .. Last - First + 1);
      begin
         for K in Result'Range loop
            Result (K) := Character'Val (Item (First + K - 1));
         end loop;
         return Result;
      end;
   end Text;

   function Argument_Count return Natural is
   begin
      Check;
      return (if Usable and then Strings > 0 then Strings - 1 else 0);
   end Argument_Count;

   function Argument (Number : Positive) return String is
   begin
      if Number > Argument_Count then
         raise Constraint_Error;
      end if;
      return Text (Number + 1);
   end Argument;

   function Command_Name return String is
   begin
      Check;
      return (if Usable and then Strings > 0 then Text (1) else "");
   end Command_Name;

   procedure Set_Exit_Status (Code : Exit_Status) is
   begin
      --  The kernel keeps the low 8 bits, as POSIX does.
      Exit_Code := Unsigned_64'Mod (Code);
      Exit_Code_Set := 1;
   end Set_Exit_Status;

end Ada.Command_Line;
