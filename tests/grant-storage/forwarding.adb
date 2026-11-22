with Ada.Text_IO;
with Memory_Grants.Loans;
with Retained_Record_Blocks;
with System;
with System.Storage_Elements;

procedure Forwarding is
   use System.Storage_Elements;
   package Loans is new Memory_Grants.Loans;
   use type Loans.Parent_Phase;
   Per_Block : constant Positive :=
     4096 / (Loans.State'Object_Size / System.Storage_Unit);
   type Page is array (Storage_Offset range 0 .. 4095) of Storage_Element;
   for Page'Alignment use 4096;
   type Pages is array (Natural range 0 .. 9) of aliased Page;
   Pool : Pages := [others => [others => 16#A5#]];
   Used : Natural := 0;
   Fail : Boolean := False;
   function Allocate (Bytes, Alignment : Storage_Count) return System.Address is
   begin
      pragma Assert (Bytes <= 4096 and Alignment <= 4096);
      if Fail then return System.Null_Address; end if;
      pragma Assert (Used < Pool'Length);
      Used := Used + 1;
      return Pool (Used - 1)'Address;
   end Allocate;
   package Records is new Retained_Record_Blocks
     (Loans.State, Per_Block * 10 - 1, Per_Block, Allocate);
   use type Records.Element_Access;
   use type Records.Allocation_Result;
   S : Records.Store;
   Empty : Loans.State;
   Held, Value : Records.Element_Access;
   Result : Records.Allocation_Result;
   Applied : Boolean;
begin
   Records.Ensure (S, 0, Empty, 10, Held, Result);
   pragma Assert (Result = Records.Added);
   Loans.Configure
     (Held.all, (0, Memory_Grants.Initial_Generation), 1,
      Memory_Grants.Borrowed_Read_Only, Loans.Forward_Once, Applied);
   pragma Assert (Applied and Loans.Holds_Parent (Held.all));
   for B in 1 .. 9 loop
      Fail := True;
      Records.Ensure (S, B * Per_Block, Empty, 10, Value, Result);
      pragma Assert (Result = Records.Out_Of_Memory and Value = null);
      pragma Assert (Records.Allocated_Blocks (S) = B);
      pragma Assert (Loans.Holds_Parent (Held.all));
      Fail := False;
      Records.Ensure (S, B * Per_Block, Empty, 10, Value, Result);
      pragma Assert (Result = Records.Added);
      pragma Assert (Loans.Phase (Value.all) = Loans.Unconfigured);
      pragma Assert (Records.Find (S, 0) = Held);
      pragma Assert (Loans.Holds_Parent (Held.all));
   end loop;
   for I in 1 .. Records.Slot'Last loop
      Value := Records.Find (S, I);
      pragma Assert (Value /= null and then Loans.Phase (Value.all) = Loans.Unconfigured);
   end loop;
   Ada.Text_IO.Put_Line
     ("PASS actual forwarding state: ten stable blocks, OOM preserves live parent");
end Forwarding;
