------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;

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
end CuBit.Process_List;
