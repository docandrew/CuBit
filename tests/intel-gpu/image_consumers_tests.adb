with Ada.Text_IO;
with Intel_GPU_Image_Consumers;
with Intel_GPU_Image_Lease;
procedure Image_Consumers_Tests is
   package C renames Intel_GPU_Image_Consumers;
   Key : constant Intel_GPU_Image_Lease.Identity := (1, 2, 3, 4, 5, 6, 0, 7);
   Object, Other : C.Ledger;
   type Tokens is array (Positive range <>) of C.Obligation;
   Items : Tokens (1 .. 1024);
   Extra : C.Obligation;
   OK : Boolean;
begin
   pragma Assert (not C.Drained (Object, Key));
   C.Stop (Object, Key, OK); pragma Assert (not OK);
   C.Open (Object, Key, OK); pragma Assert (OK);
   C.Open (Other, Key, OK); pragma Assert (OK);
   pragma Assert (not C.Drained (Object, Key)); -- Empty but still admitting.
   for I in Items'Range loop
      C.Reserve (Object, Key, C.Domain'Val (I mod 3), Items (I), OK);
      pragma Assert (OK);
   end loop;
   C.Reserve (Object, Key, C.GPU, Items (1), OK); pragma Assert (not OK);
   C.Stop (Object, Key, OK); pragma Assert (OK);
   C.Reserve (Object, Key, C.CPU, Extra, OK); pragma Assert (not OK);
   for I in reverse Items'Range loop
      C.Complete (Other, Key, Items (I), True, OK); pragma Assert (not OK);
      C.Complete (Object, (Key with delta Consumer_Instance => 8), Items (I), True, OK);
      pragma Assert (not OK);
      C.Complete (Object, Key, Items (I), False, OK); pragma Assert (not OK);
      pragma Assert (not C.Drained (Object, Key));
      C.Complete (Object, Key, Items (I), True, OK); pragma Assert (OK);
      C.Complete (Object, Key, Items (I), True, OK); pragma Assert (not OK);
   end loop;
   pragma Assert (C.Drained (Object, Key));
   pragma Assert (not C.Drained (Object, (Key with delta Output_Number => 1)));
   C.Open (Object, Key, OK); pragma Assert (not OK);
   C.Reserve (Other, Key, C.Display, Extra, OK); pragma Assert (OK);
   C.Stop (Other, Key, OK); pragma Assert (OK);
   C.Quarantine (Other);
   C.Complete (Other, Key, Extra, True, OK); pragma Assert (not OK);
   pragma Assert (not C.Drained (Other, Key));
   Ada.Text_IO.Put_Line ("Image consumers PASS: 1024 caller-owned obligations, all domains, stop, reverse completion, duplicate/foreign/stale/unknown, quarantine");
end Image_Consumers_Tests;
