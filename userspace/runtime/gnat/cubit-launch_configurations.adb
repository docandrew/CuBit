------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;

package body CuBit.Launch_Configurations is

   --  Kept by the start code (userspace/runtime/start/cubit_start.S) from
   --  the entry RDI.
   Launch_Length : Unsigned_64
   with Import, Convention => C,
        External_Name => "__cubit_launch_arguments_length";

   --  The installed words, or an empty block (refused by every decoder).
   procedure Read (Words : out LC.Word_Array; Length : out LC.Word_Count)
   with Pre => Words'First = 1 and then Words'Last = LC.Maximum_Words;

   procedure Read (Words : out LC.Word_Array; Length : out LC.Word_Count) is
   begin
      Words := [others => 0];
      Length := 0;
      if Launch_Length mod LC.Word_Bytes = 0 and then
        Launch_Length in LC.Word_Bytes .. LC.Maximum_Words * LC.Word_Bytes
      then
         declare
            Count : constant LC.Word_Count :=
              LC.Word_Count (Launch_Length / LC.Word_Bytes);
            Mapped : constant LC.Word_Array (1 .. Count)
            with Import, Address => System'To_Address (LC.Block_Address);
         begin
            Words (1 .. Count) := Mapped;
            Length := Count;
         end;
      end if;
   end Read;

   procedure Get (Value : out LC.Device_Manager_Configuration; Valid : out Boolean) is
      Words : LC.Word_Array (1 .. LC.Maximum_Words);
      Length : LC.Word_Count;
   begin
      Read (Words, Length);
      LC.Decode (Words (1 .. Length), Value, Valid);
   end Get;

   procedure Get (Value : out LC.Process_Manager_Configuration; Valid : out Boolean) is
      Words : LC.Word_Array (1 .. LC.Maximum_Words);
      Length : LC.Word_Count;
   begin
      Read (Words, Length);
      LC.Decode (Words (1 .. Length), Value, Valid);
   end Get;

   procedure Get (Value : out LC.Boot_Image_Configuration; Valid : out Boolean) is
      Words : LC.Word_Array (1 .. LC.Maximum_Words);
      Length : LC.Word_Count;
   begin
      Read (Words, Length);
      LC.Decode (Words (1 .. Length), Value, Valid);
   end Get;

   procedure Get (Kind  : LC.Configuration_Kind;
                  Value : out LC.Controller_Configuration; Valid : out Boolean) is
      Words : LC.Word_Array (1 .. LC.Maximum_Words);
      Length : LC.Word_Count;
   begin
      Read (Words, Length);
      LC.Decode (Words (1 .. Length), Kind, Value, Valid);
   end Get;

   procedure Get (Kind  : LC.Configuration_Kind;
                  Value : out LC.Queue_Configuration; Valid : out Boolean) is
      Words : LC.Word_Array (1 .. LC.Maximum_Words);
      Length : LC.Word_Count;
   begin
      Read (Words, Length);
      LC.Decode (Words (1 .. Length), Kind, Value, Valid);
   end Get;

   procedure Get (Value : out LC.Virtio_GPU_Configuration; Valid : out Boolean) is
      Words : LC.Word_Array (1 .. LC.Maximum_Words);
      Length : LC.Word_Count;
   begin
      Read (Words, Length);
      LC.Decode (Words (1 .. Length), Value, Valid);
   end Get;

   procedure Get (Value : out LC.Generic_PCI_Configuration; Valid : out Boolean) is
      Words : LC.Word_Array (1 .. LC.Maximum_Words);
      Length : LC.Word_Count;
   begin
      Read (Words, Length);
      LC.Decode (Words (1 .. Length), Value, Valid);
   end Get;

end CuBit.Launch_Configurations;
