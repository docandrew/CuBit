with Ada.Text_IO;
with Interfaces;
with Retained_Record_Blocks;
with System;
with System.Storage_Elements;

procedure Main is
   use System.Storage_Elements;
   use type Interfaces.Unsigned_64;
   type Item is record
      Generation, Payload : Interfaces.Unsigned_64;
   end record;
   type Page is array (Storage_Offset range 0 .. 4095) of aliased Storage_Element;
   for Page'Alignment use 4096;
   type Pages is array (Natural range 0 .. 15) of aliased Page;
   Pool : Pages := [others => [others => 16#A5#]];
   Calls, Used : Natural := 0;
   type Mode_Type is (Normal, Fail, Misaligned, Wrapping);
   Mode : Mode_Type := Normal;

   function Allocate (Bytes, Alignment : Storage_Count) return System.Address is
   begin
      Calls := Calls + 1;
      pragma Assert (Bytes = 1024 and Alignment = Item'Alignment);
      case Mode is
         when Fail => return System.Null_Address;
         when Misaligned => return Pool (Used)'Address + 1;
         when Wrapping =>
            return To_Address (Integer_Address'Last -
              Integer_Address (Item'Alignment) + 1);
         when Normal =>
            pragma Assert (Used < Pool'Length);
            Used := Used + 1;
            return Pool (Used - 1)'Address;
      end case;
   end Allocate;
   package Records is new Retained_Record_Blocks (Item, 1000, 64, Allocate);
   use type Records.Element_Access;
   use type Records.Allocation_Result;
   S : Records.Store;
   Value, Held : Records.Element_Access;
   Result : Records.Allocation_Result;
   Index : Records.Slot;
   Found : Boolean;
   Before : Natural;
begin
   pragma Assert (Records.Find (S, 0) = null and Calls = 0);
   Records.Next_Present (S, 0, Index, Found);
   pragma Assert (not Found);
   Records.Ensure (S, 0, (1, 2), 0, Value, Result);
   pragma Assert (Result = Records.At_Quota and Calls = 0 and Value = null);
   for M in Fail .. Wrapping loop
      Mode := M;
      Records.Ensure (S, 0, (1, 2), 16, Value, Result);
      pragma Assert (Value = null and Records.Allocated_Blocks (S) = 0);
      pragma Assert (Result = (if M = Fail then Records.Out_Of_Memory
                              else Records.Invalid_Backing));
      pragma Assert (Pool (0) = (Page'[others => 16#A5#]));
   end loop;
   Mode := Normal;
   Records.Ensure (S, 128, (1, 2), 16, Value, Result);
   pragma Assert (Result = Records.Added);
   Held := Value;
   Held.all := (77, 88);
   Before := Calls;
   Records.Ensure (S, 128, (9, 9), 0, Value, Result);
   pragma Assert (Result = Records.Existing and Value = Held and Calls = Before);
   pragma Assert (Held.all = (77, 88));
   Records.Next_Present (S, 0, Index, Found);
   pragma Assert (Found and Index = 128);
   Records.Next_Present (S, 150, Index, Found);
   pragma Assert (Found and Index = 150);
   Records.Next_Present (S, 192, Index, Found);
   pragma Assert (not Found);
   for B in 0 .. 15 loop
      if B /= 2 then
         Before := Records.Allocated_Blocks (S);
         Mode := Fail;
         Records.Ensure (S, B * 64, (1, 2), 16, Value, Result);
         pragma Assert (Result = Records.Out_Of_Memory and Value = null);
         pragma Assert (Records.Allocated_Blocks (S) = Before);
         pragma Assert (Records.Find (S, 128) = Held and Held.all = (77, 88));
         Mode := Normal;
         Records.Ensure (S, B * 64, (1, 2), Before, Value, Result);
         pragma Assert (Result = Records.At_Quota);
         Records.Ensure (S, B * 64, (1, 2), 16, Value, Result);
         pragma Assert (Result = Records.Added);
      end if;
      pragma Assert (Records.Find (S, 128) = Held and Held.all = (77, 88));
   end loop;
   for I in Records.Slot loop
      Records.Next_Present (S, I, Index, Found);
      pragma Assert (Found and Index = I);
      Value := Records.Find (S, I);
      pragma Assert (Value /= null);
      pragma Assert (Value.all = (if I = 128 then Item'(77, 88) else Item'(1, 2)));
   end loop;
   pragma Assert (Records.Allocated_Blocks (S) = 16 and Used = 16);
   Ada.Text_IO.Put_Line
     ("PASS retained blocks: sparse lookup, quota/OOM, invalid backing, stable pointers, 1001 records");
end Main;
