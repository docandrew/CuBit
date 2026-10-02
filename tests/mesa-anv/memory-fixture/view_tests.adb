with Ada.Text_IO;
with Intel_GPU_Buffer_Reply;
with Interfaces; use Interfaces;
with CuBit.Memory_Grants;
with CuBit.Messages;
with Intel_GPU_Buffer_Views;
with Intel_GPU_Buffer_Handles;
with Intel_GPU_Buffer_Backing;
procedure View_Tests is
   package G renames CuBit.Memory_Grants;
   package V renames Intel_GPU_Buffer_Views;
   package H renames Intel_GPU_Buffer_Handles;
   use type V.View_State;
   use type H.Handle;
   Buffers : H.Registry;
   ID : H.Handle;
   Base : constant Unsigned_64 := Intel_GPU_Buffer_Backing.CPU_Base;
   Shared : V.View;
   Before : Natural;
   Identity : constant Unsigned_64 := 7 * 2 ** 32 + 42;
   Source_Mode, Source_Calls : Natural := 0;
   function Completed_Source return Intel_GPU_Buffer_Reply.Backing is
   begin
      Source_Calls := Source_Calls + 1;
      if Source_Mode = 1 or else
        (Source_Calls = 2 and Source_Mode in 2 | 6)
      then
         if Source_Mode = 6 then G.Succeed := False; end if;
         return (Ready => False);
      end if;
      if Source_Calls = 2 and Source_Mode = 3 then
         return Intel_GPU_Buffer_Reply.From_Linear
           (16#10002000#, Base + 8192, 16384, 16#10000000#);
      end if;
      return Intel_GPU_Buffer_Reply.From_Linear
        (16#10001000#, Base + 4096, 16384, 16#10000000#);
   end Completed_Source;
   procedure Share_Completed is new V.Share_Completed (Completed_Source);
begin
   H.Register (Buffers, 42,
     Intel_GPU_Buffer_Reply.From_Linear (16#1000_0000#, Base, 8192, 16#1000_0000#), ID);
   pragma Assert (ID /= 0);
   G.Expected_Slot := 7;
   G.Expected_Offset := Base + 4096;
   G.Expected_Bytes := 4096;
   G.Expected_Reference := (slot => 8, generation => 9);
   G.Expected_Access := G.Write_Access;
   V.Share (Shared, Buffers, 42, ID, 7, Identity, 4096, 4096, True);
   pragma Assert (V.State (Shared) = V.Shared and V.Wire_Reference (Shared) /= 0);
   Before := G.Creates;
   V.Share (Shared, Buffers, 42, ID, 7, Identity, 4096, 4096, True);
   pragma Assert (G.Creates = Before);
   V.Retire (Shared);
   pragma Assert (V.State (Shared) = V.Retiring and V.Wire_Reference (Shared) = 0);
   pragma Assert (H.Is_Open (Buffers, 42, ID));
   V.Poll_Retirement (Shared);
   pragma Assert (V.State (Shared) = V.Retiring);
   G.Gone := True;
   V.Poll_Retirement (Shared);
   pragma Assert (V.State (Shared) = V.Retired);
   Before := G.Creates;
   V.Share (Shared, Buffers, 42, ID, 7, Identity, 4096, 4096, True);
   pragma Assert (G.Creates = Before);
   for Bad in 0 .. 5 loop
      declare
         Item : V.View;
         Offset : constant Unsigned_64 :=
           (case Bad is when 1 => 1, when 2 => Unsigned_64'Last, when others => 4096);
         Bytes : constant Unsigned_64 :=
           (case Bad is when 3 => 0, when 4 => 1, when 5 => 8192, when others => 4096);
      begin
         Before := G.Creates;
         V.Share (Item, Buffers, (if Bad = 0 then 43 else 42), ID, 7, Identity, Offset, Bytes, True);
         pragma Assert (V.State (Item) = V.Failed and G.Creates = Before);
      end;
   end loop;
   for Bad in 0 .. 4 loop
      declare
         Item : V.View;
      begin
         CuBit.Messages.Inspection := [1, 1, 0, 42, 0, 7];
         case Bad is
            when 0 => CuBit.Messages.Inspection (0) := 6;
            when 1 => CuBit.Messages.Inspection (1) := 0;
            when 2 => CuBit.Messages.Inspection (3) := 43;
            when 3 => CuBit.Messages.Inspection (5) := 8;
            when others => null;
         end case;
         Before := G.Creates;
         V.Share (Item, Buffers, 42, ID, 7,
           (if Bad = 4 then 0 else Identity), 4096, 4096, True);
         pragma Assert (V.State (Item) = V.Failed and G.Creates = Before);
      end;
   end loop;
   CuBit.Messages.Inspection := [1, 1, 0, 42, 0, 7];
   for Fail_Create in Boolean loop
      declare
         Item : V.View;
      begin
         G.Succeed := not Fail_Create;
         V.Share (Item, Buffers, 42, ID, 7, Identity, 4096, 4096, True);
         if not Fail_Create then
            G.Succeed := False;
            V.Retire (Item);
         end if;
         pragma Assert (V.State (Item) = V.Failed and V.Wire_Reference (Item) = 0);
         Before := G.Creates;
         V.Share (Item, Buffers, 42, ID, 7, Identity, 4096, 4096, True);
         pragma Assert (G.Creates = Before);
      end;
   end loop;
   for Mode in 0 .. 6 loop
      declare
         Item : V.View;
         Before_Forward : constant Natural := G.Forwardable_Creates;
         Before_Revoke : constant Natural := G.Revokes;
      begin
         Source_Mode := Mode; Source_Calls := 0;
         G.Succeed := Mode /= 5; G.Gone := False;
         G.Expected_Bytes := 16384; G.Expected_Access := G.Read_Access;
         Before := G.Creates;
         Share_Completed (Item, 7, (if Mode = 4 then 0 else Identity));
         if Mode = 0 then
            pragma Assert (V.State (Item) = V.Shared and V.Wire_Reference (Item) /= 0);
            pragma Assert (Source_Calls = 2 and G.Revokes = Before_Revoke);
         elsif Mode in 2 | 3 then
            pragma Assert (V.State (Item) = V.Retiring and V.Wire_Reference (Item) = 0);
            pragma Assert (G.Revokes = Before_Revoke + 1);
            G.Gone := True;
            V.Poll_Retirement (Item);
            pragma Assert (V.State (Item) = V.Retired);
         else
            pragma Assert (V.State (Item) = V.Failed and V.Wire_Reference (Item) = 0);
         end if;
         pragma Assert (G.Creates = Before + (if Mode in 1 | 4 then 0 else 1));
         pragma Assert (G.Forwardable_Creates = Before_Forward +
           (if Mode in 1 | 4 then 0 else 1));
         Before := G.Creates;
         Share_Completed (Item, 7, Identity);
         pragma Assert (G.Creates = Before);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Buffer views PASS: bounded/read-only completed sharing, deferred retirement, failure retention");
end View_Tests;
