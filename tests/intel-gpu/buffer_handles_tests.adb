with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Handles; use Intel_GPU_Buffer_Handles;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Reply;
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
   Register (Object, 0, Backing (0), ID); pragma Assert (ID = 0 and Count (Object) = 0);
   Register (Object, 1, (Ready => False), ID); pragma Assert (ID = 0);
   for I in 1 .. Capacity loop
      Register (Object, Unsigned_64 (I), Backing (Unsigned_64 (I) * 4096), ID);
      pragma Assert (ID = Handle (I));
      -- Same storage-slot bits are not the same issued name. Reject forged
      -- future generations even while the original object remains live.
      for Generation in Handle range 1 .. 32 loop
         declare Other : constant Handle := ID + Generation * Capacity; begin
            pragma Assert (not Resolve (Object, Unsigned_64 (I), Other).Ready);
            pragma Assert (not Closed_Backing (Object, Unsigned_64 (I), Other).Ready);
            Close (Object, Unsigned_64 (I), Other, OK);
            pragma Assert (not OK and Is_Open (Object, Unsigned_64 (I), ID));
         end;
      end loop;
      for Session in 0 .. Capacity + 1 loop
         B := Resolve (Object, Unsigned_64 (Session), ID);
         pragma Assert (B.Ready = (Session = I));
         if B.Ready then pragma Assert (Intel_GPU_Buffer_Reply.Page_Address (B, 0) = 16#2000000# + Unsigned_64 (I) * 4096); end if;
      end loop;
      Close (Object, Unsigned_64 (I + 100), ID, OK);
      pragma Assert (not OK and Is_Open (Object, Unsigned_64 (I), ID));
   end loop;
   Register (Object, 1, Backing (0), ID); pragma Assert (ID = 0 and Count (Object) = Capacity);
   Close (Object, 1, 1, OK); pragma Assert (OK and not Is_Open (Object, 1, 1));
   Register (Object, 1, Backing (0), ID); pragma Assert (ID = 0); -- no reuse
   Close_Session (Object, 2);
   pragma Assert (not Is_Open (Object, 2, 2) and Is_Open (Object, 3, 3));
   pragma Assert (not Resolve (Object, 1, Unsigned_32'Last).Ready);
   Quarantine (Object);
   for I in 1 .. Capacity loop pragma Assert (not Resolve (Object, Unsigned_64 (I), Handle (I)).Ready); end loop;
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
end Buffer_Handles_Tests;
