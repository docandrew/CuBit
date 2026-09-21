with Ada.Text_IO;
with Heap_Growth;
procedure Check_Heap_Growth is
   type Bits is array (Natural range 0 .. 31) of Boolean;
   Mapped, Owned : Bits := [others => False];
   Fail_At, Added, Removed, Released, Syncs : Natural := 0;
   OK : Boolean;
   procedure Add (Index : Natural; Success : out Boolean) is
   begin
      pragma Assert (Index = Added and then not Mapped (Index) and then not Owned (Index));
      Success := Index /= Fail_At;
      if Success then
         Mapped (Index) := True;
         Owned (Index) := True;
         Added := Added + 1;
      end if;
   end Add;
   procedure Unmap (Index : Natural) is
   begin
      pragma Assert (Syncs = 0 and then Released = 0);
      pragma Assert (Index = Added - Removed - 1 and then Mapped (Index) and then Owned (Index));
      Mapped (Index) := False;
      Removed := Removed + 1;
   end Unmap;
   procedure Synchronize is
   begin
      pragma Assert (Added > 0 and then Removed = Added and then Released = 0 and then Syncs = 0);
      pragma Assert (Mapped = Bits'[others => False]);
      Syncs := 1;
   end Synchronize;
   procedure Release_Latest is
      Index : constant Natural := Added - Released - 1;
   begin
      pragma Assert (Syncs = 1 and then Removed = Added and then Owned (Index));
      Owned (Index) := False;
      Released := Released + 1;
   end Release_Latest;
   procedure Grow is new Heap_Growth.Apply (Add, Unmap, Synchronize, Release_Latest);
begin
   for Count in 0 .. 32 loop
      for Failure in 0 .. Count loop
         Fail_At := Failure;
         Mapped := [others => False]; Owned := [others => False];
         Added := 0; Removed := 0; Released := 0; Syncs := 0;
         Grow (Count, OK);
         pragma Assert (OK = (Failure = Count));
         if OK then
            pragma Assert (Added = Count and then Removed = 0 and then Released = 0 and then Syncs = 0);
         else
            pragma Assert (Added = Failure and then Removed = Added and then Released = Added);
            pragma Assert (Syncs = (if Added = 0 then 0 else 1));
            pragma Assert (Mapped = Bits'[others => False] and then Owned = Bits'[others => False]);
         end if;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("PASS heap growth: all partial prefixes, unmap-all / TLB-sync / release ordering, zero growth");
end Check_Heap_Growth;
