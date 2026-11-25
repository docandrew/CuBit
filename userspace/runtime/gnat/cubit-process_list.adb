------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with Process_List_ABI;

package body CuBit.Process_List is
   function Get (Buffer : System.Address; Index : Natural) return Process_Entry is
      Item : constant Process_Entry
        with Import, Address => Buffer + Storage_Offset (Index * Entry_Bytes);
   begin
      return Item;
   end Get;

   function Name_Of (Item : Process_Entry) return String is
      Last : Natural := 0;
   begin
      for I in Item.Name'Range loop
         exit when Item.Name (I) = ASCII.NUL;
         Last := I;
      end loop;
      return Item.Name (1 .. Last);
   end Name_Of;

   function Class_Name (Item : Process_Entry) return String is
      package ABI renames Process_List_ABI;
      function Padded (Word : String) return String is
        (Word & [1 .. Class_Name_Bytes - Word'Length => ' '])
        with Pre => Word'Length <= Class_Name_Bytes;
   begin
      for C in ABI.Scheduling_Class loop
         if Unsigned_8 (ABI.Scheduling_Class'Enum_Rep (C)) = Item.Class then
            return Padded (case C is
                             when ABI.Idle       => "idle",
                             when ABI.Background => "bground",
                             when ABI.Normal     => "normal",
                             when ABI.Realtime   => "realtime");
         end if;
      end loop;
      return Padded ("?");
   end Class_Name;
end CuBit.Process_List;
