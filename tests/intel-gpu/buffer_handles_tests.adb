with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Handles; use Intel_GPU_Buffer_Handles;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Metadata_Initialize;
with System.Storage_Elements; use System.Storage_Elements;
procedure Buffer_Handles_Tests is
   package Layout renames Intel_GPU_Buffer_Backing;
   package Replies renames Intel_GPU_Buffer_Reply;
   Object : Registry;
   ID : Handle;
   OK : Boolean;
   B : Replies.Backing;
   function Backing (Offset : Unsigned_64) return Replies.Backing is
     (Intel_GPU_Buffer_Reply.From_Linear (16#2000000# + Offset, Layout.CPU_Base + Offset, 4096, 16#2000000#));
begin
   declare
      type Storage is array (Natural range 0 .. 8191) of Unsigned_64;
      Memory : Storage := [others => 0] with Alignment => 4096;
      Pool : Registry;
      Names : array (1 .. 65) of Handle;
      Pinned : Retained_Reference;
      Cursor : Natural := 0;
      Done : Boolean;
   begin
      Extend_Storage (Pool, Unsigned_64 (To_Integer (Memory'Address)), 65536, OK);
      pragma Assert (OK and Record_Capacity (Pool) >= 65);
      for I in Names'Range loop
         Register (Pool, (if I mod 2 = 0 then 43 else 42),
           Backing (Unsigned_64 (I - 1) * 4096), Names (I));
         pragma Assert (Names (I) /= 0);
      end loop;
      Retain_Backing (Pool, 42, Names (65), Pinned, OK); pragma Assert (OK);
      Close_Session_Step (Pool, 42, 66, Cursor, Done);
      pragma Assert (not Done and Cursor = 0 and Is_Open (Pool, 42, Names (1)));
      for Turn in 1 .. 3 loop
         Close_Session_Step (Pool, 42, 65, Cursor, Done);
         pragma Assert (Cursor = Natural'Min (32 * Turn, 65) and Done = (Turn = 3));
         for I in Names'Range loop
            pragma Assert (Is_Open (Pool, (if I mod 2 = 0 then 43 else 42), Names (I)) =
              (I mod 2 = 0 or I > Cursor));
         end loop;
         pragma Assert (Referenced_Backing (Pool, Pinned).Ready);
      end loop;
      pragma Assert (Session_Closed (Pool, 42) and Count (Pool) = 65);
      pragma Assert (not Can_Release_Backing (Pool, 42, Names (65)));
      Return_Reference (Pool, Pinned, True, OK); pragma Assert (OK);
      pragma Assert (Can_Release_Backing (Pool, 42, Names (65)));
      Close_Session_Step (Pool, 42, 65, Cursor, Done);
      pragma Assert (Done and Cursor = 65);
   end;
   Ada.Text_IO.Put_Line ("Bounded handle close PASS:65 records in32/32/1 visits, other owner and retained pin preserved");
   declare
      Pool : Registry;
      Name, Next_Name : Handle;
   begin
      pragma Assert (Check_Close (Pool, 0, 1) = Session_Unavailable);
      pragma Assert (Check_Close (Pool, 42, 0) = Invalid_Handle);
      pragma Assert (Check_Close (Pool, 42, 133) = Unknown_Handle);
      Register (Pool, 42, Backing (0), Name);
      for Cycle in 1 .. 1024 loop
         pragma Assert (Check_Close (Pool, 42, Name) = Close_Ready);
         pragma Assert (Check_Close (Pool, 43, Name) = Foreign_Session);
         Close (Pool, 43, Name, OK); pragma Assert (not OK);
         pragma Assert (Is_Open (Pool, 42, Name));
         Close (Pool, 42, Name, OK); pragma Assert (OK);
         pragma Assert (Check_Close (Pool, 42, Name) = Already_Closed);
         Close (Pool, 42, Name, OK); pragma Assert (not OK);
         Release_Retired_Backing (Pool, 42, Name, True, OK);
         pragma Assert (OK);
         Replace_Retired (Pool, 42, 42, Name, Backing (0), True, Next_Name);
         pragma Assert (Next_Name > Name);
         pragma Assert (Check_Close (Pool, 42, Name) = Unknown_Handle);
         pragma Assert (Check_Close (Pool, 42, Next_Name) = Close_Ready);
         Name := Next_Name;
      end loop;
      Quarantine (Pool);
      pragma Assert (Check_Close (Pool, 42, Name) = Registry_Quarantined);
      Close (Pool, 42, Name, OK); pragma Assert (not OK);
   end;
   Ada.Text_IO.Put_Line ("Close diagnostics PASS: 1024 identity replacements, foreign/duplicate/stale/session/quarantine rejection");
   declare
      Pool, Foreign : Registry;
      Export_Pin, Import_Pin, Work_Pin, Empty : Retained_Reference;
      Name, Replacement : Handle;
   begin
      Register (Pool, 42, Backing (0), Name);
      Retain_Referenced_Backing (Pool, Empty, Import_Pin, OK);
      pragma Assert (not OK);
      Retain_Backing (Pool, 42, Name, Export_Pin, OK); pragma Assert (OK);
      Close_Session (Pool, 42);
      -- Admission is closed; only an existing internal retained lifetime can
      -- be split, not an application naming the closed allocation.
      Retain_Backing (Pool, 42, Name, Import_Pin, OK); pragma Assert (not OK);
      Retain_Referenced_Backing (Foreign, Export_Pin, Import_Pin, OK);
      pragma Assert (not OK);
      Retain_Referenced_Backing (Pool, Export_Pin, Import_Pin, OK);
      pragma Assert (OK);
      Retain_Referenced_Backing (Pool, Export_Pin, Import_Pin, OK);
      pragma Assert (not OK);
      Return_Reference (Pool, Export_Pin, True, OK); pragma Assert (OK);
      Retain_Referenced_Backing (Pool, Export_Pin, Work_Pin, OK);
      pragma Assert (not OK);
      Retain_Referenced_Backing (Pool, Import_Pin, Work_Pin, OK);
      pragma Assert (OK);
      pragma Assert (Referenced_Backing (Pool, Work_Pin).Ready);
      Return_Reference (Pool, Import_Pin, True, OK); pragma Assert (OK);
      pragma Assert (not Can_Release_Backing (Pool, 42, Name));
      Replace_Retired (Pool, 42, 42, Name, Backing (0), True, Replacement);
      pragma Assert (Replacement = No_Handle);
      Return_Reference (Pool, Work_Pin, False, OK); pragma Assert (not OK);
      Return_Reference (Pool, Work_Pin, True, OK); pragma Assert (OK);
      Return_Reference (Pool, Work_Pin, True, OK); pragma Assert (not OK);
      pragma Assert (Can_Release_Backing (Pool, 42, Name));
      Release_Retired_Backing (Pool, 42, Name, True, OK); pragma Assert (OK);
      Replace_Retired (Pool, 42, 43, Name, Backing (0), True, Replacement);
      pragma Assert (Replacement /= No_Handle);
      Retain_Referenced_Backing (Pool, Work_Pin, Import_Pin, OK);
      pragma Assert (not OK);
      Retain_Backing (Pool, 43, Replacement, Export_Pin, OK); pragma Assert (OK);
      Quarantine (Pool);
      Retain_Referenced_Backing (Pool, Export_Pin, Import_Pin, OK);
      pragma Assert (not OK and not Referenced_Backing (Pool, Import_Pin).Ready);
   end;
   Ada.Text_IO.Put_Line ("Retained lifetime split PASS: closed owner, independent users, stale/foreign/active rejection, retirement and quarantine");
   declare
      Pool, Other : Registry;
      First, Second : Retained_Reference;
      Name, Other_Name, Replacement : Handle;
   begin
      Register (Pool, 42, Backing (0), Name);
      Register (Other, 42, Backing (0), Other_Name);
      pragma Assert (Name = Other_Name);
      pragma Assert (not Can_Release_Backing (Pool, 42, Name)); -- still open
      Retain_Backing (Pool, 43, Name, First, OK); pragma Assert (not OK);
      Retain_Backing (Pool, 42, Name, First, OK); pragma Assert (OK);
      Retain_Backing (Pool, 42, Name, First, OK); pragma Assert (not OK);
      Retain_Backing (Pool, 42, Name, Second, OK); pragma Assert (OK);
      pragma Assert (not Referenced_Backing (Other, First).Ready);
      Return_Reference (Other, First, True, OK); pragma Assert (not OK);
      Close_Session (Pool, 42);
      pragma Assert (not Resolve (Pool, 42, Name).Ready and
        Referenced_Backing (Pool, First).Ready and Referenced_Backing (Pool, Second).Ready);
      Release_Retired_Backing (Pool, 42, Name, True, OK); pragma Assert (not OK);
      pragma Assert (not Can_Release_Backing (Pool, 42, Name));
      Replace_Retired (Pool, 42, 42, Name, Backing (0), True, Replacement);
      pragma Assert (Replacement = No_Handle);
      Return_Reference (Pool, First, False, OK); pragma Assert (not OK);
      Return_Reference (Pool, First, True, OK); pragma Assert (OK);
      pragma Assert (not Can_Release_Backing (Pool, 42, Name)); -- second reader
      Return_Reference (Pool, First, True, OK); pragma Assert (not OK);
      Release_Retired_Backing (Pool, 42, Name, True, OK); pragma Assert (not OK);
      Return_Reference (Pool, Second, True, OK); pragma Assert (OK);
      pragma Assert (Can_Release_Backing (Pool, 42, Name));
      pragma Assert (not Can_Release_Backing (Pool, 43, Name));
      Release_Retired_Backing (Pool, 42, Name, True, OK); pragma Assert (OK);
      Replace_Retired (Pool, 42, 43, Name, Backing (0), True, Replacement);
      pragma Assert (Replacement > Name and Resolve (Pool, 43, Replacement).Ready);
      pragma Assert (not Referenced_Backing (Pool, Second).Ready);
      Retain_Backing (Pool, 43, Replacement, First, OK); pragma Assert (OK);
      Quarantine (Pool);
      Return_Reference (Pool, First, True, OK); pragma Assert (not OK);
   end;
   Ada.Text_IO.Put_Line ("Backing references PASS: close retains pins, wrong registry, double return, replacement gate, quarantine");
   Register (Object, 0, Backing (0), ID); pragma Assert (ID = 0 and Count (Object) = 0);
   Register (Object, 1, (Ready => False), ID); pragma Assert (ID = 0);
   for I in 1 .. Initial_Capacity loop
      Register (Object, Unsigned_64 (I), Backing (Unsigned_64 (I) * 4096), ID);
      pragma Assert (ID = Handle (I));
      -- Same storage-slot bits are not the same issued name. Reject forged
      -- future generations even while the original object remains live.
      for Generation in Handle range 1 .. 32 loop
         declare Other : constant Handle := ID + Generation * Initial_Capacity; begin
            pragma Assert (not Resolve (Object, Unsigned_64 (I), Other).Ready);
            pragma Assert (not Closed_Backing (Object, Unsigned_64 (I), Other).Ready);
            Close (Object, Unsigned_64 (I), Other, OK);
            pragma Assert (not OK and Is_Open (Object, Unsigned_64 (I), ID));
         end;
      end loop;
      for Session in 0 .. Initial_Capacity + 1 loop
         B := Resolve (Object, Unsigned_64 (Session), ID);
         pragma Assert (B.Ready = (Session = I));
         if B.Ready then pragma Assert (Intel_GPU_Buffer_Reply.Page_Address (B, 0) = 16#2000000# + Unsigned_64 (I) * 4096); end if;
      end loop;
      Close (Object, Unsigned_64 (I + 100), ID, OK);
      pragma Assert (not OK and Is_Open (Object, Unsigned_64 (I), ID));
   end loop;
   Register (Object, 1, Backing (0), ID); pragma Assert (ID = 0 and Count (Object) = Initial_Capacity);
   Close (Object, 1, 1, OK); pragma Assert (OK and not Is_Open (Object, 1, 1));
   Register (Object, 1, Backing (0), ID); pragma Assert (ID = 0); -- no reuse
   Close_Session (Object, 2);
   pragma Assert (not Is_Open (Object, 2, 2) and Is_Open (Object, 3, 3));
   pragma Assert (not Resolve (Object, 1, Unsigned_32'Last).Ready);
   Quarantine (Object);
   for I in 1 .. Initial_Capacity loop pragma Assert (not Resolve (Object, Unsigned_64 (I), Handle (I)).Ready); end loop;
   declare Fresh : Registry; begin
      Register (Fresh, 42, Backing (0), ID);
      Register (Fresh, 42, Backing (4096), ID);
      Register (Fresh, 43, Backing (8192), ID);
      Close_Session (Fresh, 42);
      pragma Assert (not Resolve (Fresh, 42, 1).Ready and
                     not Resolve (Fresh, 42, 2).Ready and Resolve (Fresh, 43, 3).Ready);
      Close (Fresh, 43, 0, OK); pragma Assert (not OK);
      Quarantine (Fresh);
      Register (Fresh, 43, Backing (12288), ID);
      pragma Assert (ID = 0 and Count (Fresh) = 3);
   end;
   for Fault in 1 .. 4 loop
      declare Fresh : Registry; begin
         Register (Fresh, 1, Backing (4096), ID); pragma Assert (ID = 1);
         Close (Fresh, 1, ID, OK); pragma Assert (OK);
         B := Backing (4096);
         case Fault is
            when 1 => null; -- overlaps retired backing
            when 2 => B := Replies.From_Linear
              (16#3002000#, Layout.CPU_Base + 8192, 4096, 16#3000000#);
            when 3 => B.Bytes := Unsigned_64'Last;
            when 4 => B.CPU_Address := Unsigned_64'Last;
            when others => null;
         end case;
         Register (Fresh, 2, B, ID); pragma Assert (ID = 0 and Count (Fresh) = 1);
         Register (Fresh, 2, Backing (8192), ID); pragma Assert (ID = 2);
         pragma Assert (not Resolve (Fresh, 1, 2).Ready and Resolve (Fresh, 2, 2).Ready);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Buffer handles PASS: session isolation, close, retained ranges, exhaustion, no reuse, quarantine");
   declare
      Pool : Registry;
      Current, Next_ID, Neighbor : Handle;
   begin
      Register (Pool, 42, Backing (0), Current);
      Register (Pool, 43, Backing (4096), Neighbor);
      pragma Assert (not Session_Closed (Pool, 42) and Session_Closed (Pool, 44));
      for Cycle in 1 .. 128 loop
         Replace_Retired (Pool, 42, 42, Current, Backing (0), True, Next_ID);
         pragma Assert (Next_ID = 0 and Is_Open (Pool, 42, Current));
         Close (Pool, 42, Current, OK); pragma Assert (OK);
         pragma Assert (Session_Closed (Pool, 42));
         Replace_Retired (Pool, 42, 42, Current, Backing (0), False, Next_ID);
         pragma Assert (Next_ID = 0 and Closed_Backing (Pool, 42, Current).Ready);
         Replace_Retired (Pool, 43, 43, Current, Backing (0), True, Next_ID);
         pragma Assert (Next_ID = 0);
         Replace_Retired (Pool, 42, 42, Current, Backing (4096), True, Next_ID);
         pragma Assert (Next_ID = 0); -- neighboring allocation still reserved
         Replace_Retired (Pool, 42, 42, Current, Backing (0), True, Next_ID);
         pragma Assert (Next_ID = Handle (Cycle + 2) and Count (Pool) = 2);
         pragma Assert (not Resolve (Pool, 42, Current).Ready);
         pragma Assert (not Closed_Backing (Pool, 42, Current).Ready);
         Close (Pool, 42, Current, OK); pragma Assert (not OK);
         pragma Assert (Is_Open (Pool, 42, Next_ID) and Is_Open (Pool, 43, Neighbor));
         Current := Next_ID;
         pragma Assert (not Session_Closed (Pool, 42));
      end loop;
      Close_Session (Pool, 42);
      pragma Assert (Session_Closed (Pool, 42) and not Session_Closed (Pool, 43));
      pragma Assert (not Is_Open (Pool, 42, Current) and Is_Open (Pool, 43, Neighbor));
      Quarantine (Pool);
      Replace_Retired (Pool, 42, 42, Current, Backing (0), True, Next_ID);
      pragma Assert (Next_ID = 0);
   end;
   Ada.Text_IO.Put_Line ("Buffer replacement PASS:128 fresh identities, stale names, retained neighbors, trusted gate");
   declare
      Pool : Registry;
      Current, Replacement, Neighbor, Third : Handle;
   begin
      Register (Pool, 42, Backing (0), Current);
      for Cycle in 1 .. 128 loop
         Close (Pool, 42, Current, OK); pragma Assert (OK);
         Replace_Retired (Pool, 42, 42, Current, Backing (0), True, Replacement);
         pragma Assert (Replacement = Current + 1);
         Current := Replacement;
      end loop;
      -- Storage grows after name issuance has advanced far beyond its size.
      -- Neither Used nor handle modulo capacity may select a record.
      Register (Pool, 43, Backing (4096), Neighbor);
      pragma Assert (Neighbor = Current + 1 and Count (Pool) = 2);
      Close (Pool, 42, Current, OK); pragma Assert (OK);
      Replace_Retired (Pool, 42, 42, Current, Backing (0), True, Replacement);
      pragma Assert (Replacement = Neighbor + 1 and Count (Pool) = 2);
      Register (Pool, 44, Backing (8192), Third);
      pragma Assert (Third = Replacement + 1 and Count (Pool) = 3);
      pragma Assert (Resolve (Pool, 42, Replacement).Ready and
                     Resolve (Pool, 43, Neighbor).Ready and
                     Resolve (Pool, 44, Third).Ready);
      for Stale in Handle range 1 .. Current loop
         Close (Pool, 42, Stale, OK); pragma Assert (not OK);
      end loop;
      Close_Session (Pool, 42);
      pragma Assert (not Resolve (Pool, 42, Replacement).Ready and
                     Resolve (Pool, 43, Neighbor).Ready and
                     Resolve (Pool, 44, Third).Ready);
   end;
   Ada.Text_IO.Put_Line ("Capacity-independent handles PASS: mixed fresh/reused slots and retained neighbors");
   declare
      Pool : Registry;
      Old_ID, Neighbor, Replacement : Handle;
   begin
      Register (Pool, 42, Backing (0), Old_ID);
      Release_Retired_Backing (Pool, 42, Old_ID, True, OK);
      pragma Assert (not OK); -- live names cannot release their reservation
      Close (Pool, 42, Old_ID, OK); pragma Assert (OK);
      Release_Retired_Backing (Pool, 42, Old_ID, False, OK); pragma Assert (not OK);
      Release_Retired_Backing (Pool, 43, Old_ID, True, OK); pragma Assert (not OK);
      Release_Retired_Backing (Pool, 42, Old_ID, True, OK); pragma Assert (OK);
      pragma Assert (not Closed_Backing (Pool, 42, Old_ID).Ready);
      Release_Retired_Backing (Pool, 42, Old_ID, True, OK); pragma Assert (not OK);
      Register (Pool, 43, Backing (0), Neighbor);
      pragma Assert (Neighbor /= 0 and Resolve (Pool, 43, Neighbor).Ready);
      Replace_Retired (Pool, 42, 42, Old_ID, Backing (0), True, Replacement);
      pragma Assert (Replacement = 0); -- range now belongs to neighbor
      Replace_Retired (Pool, 42, 42, Old_ID, Backing (4096), True, Replacement);
      pragma Assert (Replacement = Neighbor + 1);
      pragma Assert (not Resolve (Pool, 42, Old_ID).Ready);
      pragma Assert (Resolve (Pool, 43, Neighbor).Ready and Resolve (Pool, 42, Replacement).Ready);
   end;
   Ada.Text_IO.Put_Line ("Retired reservation PASS: cross-session range reuse preserves identity tombstones");
   declare
      Pool : Registry;
      Current, Replacement : Handle;
      Owner : Session_ID := 100;
   begin
      Register (Pool, Owner, Backing (0), Current);
      for Cycle in 1 .. 128 loop
         Close (Pool, Owner, Current, OK); pragma Assert (OK);
         Replace_Retired (Pool, Owner, Owner + 1, Current, Backing (0), True, Replacement);
         pragma Assert (Replacement = 0); -- name closure is insufficient
         Release_Retired_Backing (Pool, Owner, Current, True, OK); pragma Assert (OK);
         Replace_Retired (Pool, Owner + 1, Owner + 1, Current, Backing (0), True, Replacement);
         pragma Assert (Replacement = 0); -- wrong previous identity
         Replace_Retired (Pool, Owner, 0, Current, Backing (0), True, Replacement);
         pragma Assert (Replacement = 0);
         Replace_Retired (Pool, Owner, Owner + 1, Current, Backing (0), False, Replacement);
         pragma Assert (Replacement = 0);
         Replace_Retired (Pool, Owner, Owner + 1, Current, Backing (0), True, Replacement);
         pragma Assert (Replacement = Current + 1 and Count (Pool) = 1);
         pragma Assert (not Resolve (Pool, Owner, Current).Ready);
         pragma Assert (not Resolve (Pool, Owner, Replacement).Ready);
         pragma Assert (not Resolve (Pool, Owner + 1, Current).Ready);
         Close_Session (Pool, Owner);
         pragma Assert (Resolve (Pool, Owner + 1, Replacement).Ready);
         Current := Replacement;
         Owner := Owner + 1;
      end loop;
   end;
   Ada.Text_IO.Put_Line ("Cross-session handles PASS:128 released generations, previous-owner checks, old-session teardown isolation");
   declare
      type Storage is array (Natural range 0 .. 8191) of Unsigned_64;
      Memory : Storage := [others => 16#ABCD#] with Alignment => 4096;
      Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Memory'Address));
      Committed : Unsigned_64 := 0;
      function Reserve (Bytes : Unsigned_64) return Unsigned_64 is
        (if Bytes = 65536 then Base else 0);
      function Commit (Address, Offset, Bytes : Unsigned_64) return Boolean is
      begin
         pragma Assert (Address = Base and Offset = Committed and Offset + Bytes <= 65536);
         Committed := Committed + Bytes; return True;
      end Commit;
      package M is new Intel_GPU_Metadata_Arena
        (Reserve, Commit, Intel_GPU_Metadata_Initialize.Clear);
      A : M.Arena;
      Pool : Registry;
      Previous_Capacity : Natural;
      Name, Replacement : Handle;
      Pinned : Retained_Reference;
   begin
      M.Open (A, 65536, OK); pragma Assert (OK);
      for Growth in Unsigned_64 range 1 .. 8 loop
         Previous_Capacity := Record_Capacity (Pool);
         M.Request (A, Growth * 4096, OK); pragma Assert (OK);
         M.Step (A);
         Extend_Storage (Pool, Base, M.Snapshot (A).Published, OK);
         pragma Assert (OK and Record_Capacity (Pool) > Previous_Capacity);
         while Count (Pool) < Record_Capacity (Pool) loop
            Register (Pool, 42, Backing (Unsigned_64 (Count (Pool)) * 4096), Name);
            pragma Assert (Name /= 0 and Name = Handle (Count (Pool)));
         end loop;
         if Growth = 1 then
            Retain_Backing (Pool, 42, 20, Pinned, OK); pragma Assert (OK);
         end if;
         pragma Assert (Referenced_Backing (Pool, Pinned).Ready and then
           Referenced_Backing (Pool, Pinned).CPU_Address = Layout.CPU_Base + 19 * 4096);
         for I in 1 .. Count (Pool) loop
            pragma Assert (Check_Close (Pool, 42, Handle (I)) = Close_Ready);
            pragma Assert (Resolve (Pool, 42, Handle (I)).Ready and then
              Resolve (Pool, 42, Handle (I)).CPU_Address = Layout.CPU_Base + Unsigned_64 (I - 1) * 4096);
            pragma Assert (not Resolve (Pool, 43, Handle (I)).Ready);
         end loop;
         Previous_Capacity := Record_Capacity (Pool);
         Extend_Storage (Pool, Base + 4096, (Growth + 1) * 4096, OK);
         pragma Assert (not OK and Record_Capacity (Pool) = Previous_Capacity);
      end loop;
      pragma Assert (Count (Pool) > 128);
      pragma Assert (Check_Close (Pool, 42, 133) = Close_Ready);
      Close (Pool, 42, 133, OK); pragma Assert (OK);
      pragma Assert (Check_Close (Pool, 42, 133) = Already_Closed);
      Close (Pool, 42, 20, OK); pragma Assert (OK);
      Replace_Retired (Pool, 42, 42, 20, Backing (19 * 4096), True, Replacement);
      pragma Assert (Replacement = No_Handle and Referenced_Backing (Pool, Pinned).Ready);
      Return_Reference (Pool, Pinned, True, OK); pragma Assert (OK);
      Replace_Retired (Pool, 42, 42, 20, Backing (19 * 4096), True, Replacement);
      pragma Assert (Replacement > Handle (Count (Pool)) and Resolve (Pool, 42, Replacement).Ready);
      pragma Assert (not Resolve (Pool, 42, 20).Ready and Resolve (Pool, 42, 21).Ready);
      Close_Session (Pool, 42); pragma Assert (Session_Closed (Pool, 42));
      for I in 4096 .. Memory'Last loop pragma Assert (Memory (I) = 16#ABCD#); end loop;
   end;
   Ada.Text_IO.Put_Line ("Dynamic handle storage PASS: eight arena growth boundaries, >128 live records, stable names, replacement/session retirement, untouched uncommitted tail");
   declare
      type Storage is array (0 .. 8191) of Unsigned_64;
      Memory : Storage := [others => 0] with Alignment => 4096;
      Pool : Registry;
      Names : array (1 .. 129) of Handle;
      Owners : array (1 .. 129) of Session_ID := [others => 42];
      Pin : Retained_Reference;
      New_Name, Old_Name : Handle;
   begin
      Extend_Storage (Pool, Unsigned_64 (To_Integer (Memory'Address)), 65536, OK);
      pragma Assert (OK);
      for I in Names'Range loop
         Register (Pool, Owners (I), Backing (Unsigned_64 (I - 1) * 4096), Names (I));
         pragma Assert (Names (I) /= No_Handle);
      end loop;
      Retain_Backing (Pool, 42, Names (129), Pin, OK); pragma Assert (OK);
      for Cycle in 1 .. 1024 loop
         declare
            I : constant Positive := (Cycle * 37) mod 128 + 1;
            Next_Owner : constant Session_ID := (if Owners (I) = 42 then 43 else 42);
         begin
            Old_Name := Names (I);
            Close (Pool, Owners (I), Old_Name, OK); pragma Assert (OK);
            Release_Retired_Backing (Pool, Owners (I), Old_Name, True, OK);
            pragma Assert (OK);
            Replace_Retired (Pool, Owners (I), Next_Owner, Old_Name,
              Backing (Unsigned_64 (I - 1) * 4096), True, New_Name);
            pragma Assert (New_Name > Old_Name);
            pragma Assert (not Resolve (Pool, Owners (I), New_Name).Ready);
            pragma Assert (not Resolve (Pool, Next_Owner, Old_Name).Ready);
            Names (I) := New_Name; Owners (I) := Next_Owner;
         end;
         for I in Names'Range loop
            pragma Assert (Resolve (Pool, Owners (I), Names (I)).Ready and then
              Resolve (Pool, Owners (I), Names (I)).CPU_Address =
                Layout.CPU_Base + Unsigned_64 (I - 1) * 4096);
         end loop;
         pragma Assert (Count (Pool) = 129 and Referenced_Backing (Pool, Pin).Ready);
      end loop;
      Return_Reference (Pool, Pin, True, OK); pragma Assert (OK);
   end;
   Ada.Text_IO.Put_Line ("Indexed handle relocation PASS:129 stable buffers,1024 cross-owner replacements, all neighbors and pinned record preserved");
end Buffer_Handles_Tests;
