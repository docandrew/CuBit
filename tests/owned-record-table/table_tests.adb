with Owned_Record_Tables;
with Interfaces; use Interfaces;
with Interfaces.C;
with System;
with Ada.Text_IO; use Ada.Text_IO;
procedure Table_Tests is
   use type System.Address;
   function Malloc (Bytes : Interfaces.C.size_t) return System.Address
     with Import, Convention => C, External_Name => "malloc";
   procedure C_Free (Address : System.Address)
     with Import, Convention => C, External_Name => "free";
   Outstanding, Calls : Natural := 0;
   Fail_At : Natural := Natural'Last;
   procedure Allocate_Memory (Bytes : Positive; Address : out System.Address) is
   begin
      Calls := Calls + 1;
      Address := (if Calls = Fail_At then System.Null_Address
                  else Malloc (Interfaces.C.size_t (Bytes)));
      if Address /= System.Null_Address then Outstanding := Outstanding + 1; end if;
   end Allocate_Memory;
   procedure Free_Memory (Bytes : Positive; Address : System.Address) is
   begin
      pragma Assert (Bytes > 0 and Address /= System.Null_Address and Outstanding > 0);
      C_Free (Address); Outstanding := Outstanding - 1;
   end Free_Memory;
   type Payload is record
      Key, Tag : Unsigned_64 := 0;
   end record;
   package Registry is new Owned_Record_Tables
     (Payload, 8193, 24, Allocate_Memory, Free_Memory);
   use Registry;
   A, B : aliased Table;
   I, J, Saved_Next : Id;
   Handles : array (1 .. 8193) of Id;
   Addresses : array (1 .. 8193) of System.Address;
   Present_Keys : array (0 .. 8192) of Boolean := (others => False);
   procedure Check_Contents is
      N : Natural := 0;
      Cursor : Id := First (A);
   begin
      for Key in Present_Keys'Range loop
         if Present_Keys(Key) then
            pragma Assert (Cursor /= 0);
            pragma Assert (Get (A, Cursor).Key = Unsigned_64(Key));
            pragma Assert (Get (A, Cursor).Tag = Unsigned_64(Key) * 17 + 3);
            N := N + 1;
            Cursor := Next (A, Cursor);
         end if;
      end loop;
      pragma Assert (Cursor = 0 and Registry.Count(A) = N);
   end Check_Contents;
begin
   -- Directory failure and first-block failure leave no retained backing.
   for Failure in 1 .. 2 loop
      Fail_At := Calls + Failure;
      Insert (A, 0, (0, 3), I);
      pragma Assert (I = 0 and Registry.Count(A) = 0 and Outstanding = 0);
   end loop;
   Fail_At := Natural'Last;
   -- More than twice the old GLOBAL limit, and a partial final block.
   for Key in Present_Keys'Range loop
      Insert (A, Unsigned_64(Key), (Unsigned_64(Key), Unsigned_64(Key)*17+3), I);
      pragma Assert (I /= 0);
      Handles(Key+1) := I;
      Addresses(Key+1) := Get(A,I).Value.all'Address;
      Present_Keys(Key) := True;
   end loop;
   Check_Contents;
   Insert (A, 99999, (99999, 0), I);
   pragma Assert (I = 0 and Registry.Count(A) = 8193);
   -- One process/table at its ceiling must not consume another's record IDs.
   Insert (B, 8, (8, 139), J);
   pragma Assert (J /= 0 and Registry.Count(B) = 1);
   for Key in Present_Keys'Range loop
      pragma Assert (Addresses(Key+1) = Get(A,Handles(Key+1)).Value.all'Address);
      if Key mod 3 = 1 then
         Release (A, Handles(Key+1)); Present_Keys(Key) := False;
      end if;
   end loop;
   Check_Contents;
   -- Reinsert out of order: exercise head/middle/tail links and hole reuse.
   for Key in reverse Present_Keys'Range loop
      if not Present_Keys(Key) then
         Insert (A, Unsigned_64(Key), (Unsigned_64(Key), Unsigned_64(Key)*17+3), I);
         pragma Assert (I /= 0); Present_Keys(Key) := True;
      end if;
   end loop;
   Check_Contents;
   I := First(A);
   while I /= 0 loop
      Saved_Next := Next(A,I); Release(A,I); I := Saved_Next;
   end loop;
   pragma Assert (Registry.Count(A)=0 and First(A)=0 and Get(B,J).Tag=139);
   Release(B,J);
   pragma Assert (Outstanding=0);
   -- Failed expansion preserves all existing payloads and list links.
   for Key in 0 .. 23 loop
      Insert(A,Unsigned_64(Key),(Unsigned_64(Key),Unsigned_64(Key)*17+3),I);
   end loop;
   Fail_At := Calls+1;
   Insert(A,24,(24,411),I);
   pragma Assert(I=0 and Registry.Count(A)=24);
   Present_Keys := (others => False);
   for Key in 0 .. 23 loop Present_Keys(Key):=True; end loop;
   Check_Contents;
   Fail_At:=Natural'Last;
   I:=First(A);
   while I/=0 loop Saved_Next:=Next(A,I);Release(A,I);I:=Saved_Next;end loop;
   -- Equal keys are valid (reservation plus committed chunk starts).
   for K in 1 .. 100 loop Insert(A,7,(7,Unsigned_64(K)),I);end loop;
   for Item of Iterate(A'Access) loop
      pragma Assert(Get(A,Item).Key=7);
      Release(A,Item);
   end loop;
   pragma Assert(First(A)=0 and Outstanding=0);
   Put_Line("PASS dynamic mapping records: 8193 records, isolated ceiling, ordered holes, stable references, equal keys, allocation rollback, full reclamation");
end Table_Tests;
