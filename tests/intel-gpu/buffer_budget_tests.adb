with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Backing;
procedure Buffer_Budget_Tests is
   package B renames Intel_GPU_Buffer_Backing;
   use type B.Budget_Words;
   Data : B.Budget_Words;
   Empty : constant B.Budget_Words := [others => 0];
   Snapshot : B.Budget_Snapshot;
   Expected : Boolean;
begin
   for Pages in 0 .. Natural (B.Capacity / 4096) loop
      for Slots in 0 .. 16 loop
         Data := B.Budget_Reply (True, B.Capacity, Unsigned_64 (Pages) * 4096, Slots);
         Expected := Pages >= 16 - Slots and then Pages <= (16 - Slots) * 4096;
         Snapshot := B.Decode_Budget (B.Budget_Request_Label, 4, 0, 0, Data);
         pragma Assert (Snapshot.Known = Expected);
         if Expected then
            pragma Assert (Data = [1, B.Capacity, Unsigned_64 (Pages) * 4096, Unsigned_64 (Slots)]);
            pragma Assert (Snapshot.Free_Bytes = B.Capacity - Unsigned_64 (Pages) * 4096);
            pragma Assert (Snapshot.Maximum_Allocation =
              (if Slots = 0 then 0 else Unsigned_64'Min (Snapshot.Free_Bytes, 16 * 1024 * 1024)));
         else
            pragma Assert (Data = Empty and Snapshot.Maximum_Allocation = 0);
         end if;
      end loop;
   end loop;
   pragma Assert (B.Budget_Reply (False, B.Capacity, 0, 16) = Empty);
   pragma Assert (B.Budget_Reply (True, 0, 0, 16) = Empty);
   pragma Assert (B.Budget_Reply (True, B.Capacity + 4096, 0, 16) = Empty);
   pragma Assert (B.Budget_Reply (True, B.Capacity, 1, 16) = Empty);
   pragma Assert (B.Budget_Reply (True, B.Capacity, B.Capacity + 4096, 16) = Empty);
   pragma Assert (B.Budget_Reply (True, B.Capacity, Unsigned_64'Last, 16) = Empty);
   pragma Assert (B.Budget_Reply (True, B.Capacity, 0, 17) = Empty);
   pragma Assert (B.Budget_Reply (True, B.Capacity, 0, Natural'Last) = Empty);
   Data := B.Budget_Reply (True, B.Capacity, 4096, 15);
   for Bit in 0 .. 31 loop
      Snapshot := B.Decode_Budget
        (B.Budget_Request_Label xor Shift_Left (Unsigned_32'(1), Bit), 4, 0, 0, Data);
      pragma Assert (not Snapshot.Known and Snapshot.Free_Bytes = 0);
   end loop;
   for N in Unsigned_8 loop
      Snapshot := B.Decode_Budget (B.Budget_Request_Label, N, 0, 0, Data);
      pragma Assert (Snapshot.Known = (N = 4));
      Snapshot := B.Decode_Budget (B.Budget_Request_Label, 4, N, 0, Data);
      pragma Assert (Snapshot.Known = (N = 0));
   end loop;
   for Bit in 0 .. 15 loop
      Snapshot := B.Decode_Budget
        (B.Budget_Request_Label, 4, 0, Shift_Left (Unsigned_16'(1), Bit), Data);
      pragma Assert (not Snapshot.Known);
   end loop;
   Data (0) := 0;
   pragma Assert (not B.Decode_Budget (B.Budget_Request_Label, 4, 0, 0, Data).Known);
   Ada.Text_IO.Put_Line ("Buffer budget PASS: all page/slot counts and invalid snapshots; codec only");
end Buffer_Budget_Tests;
