with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Network_Authority; use CuBit.Network_Authority;
with Network_Grants;
with CuBit.Launch_Policy;
with UDP_Tests;

procedure Main is
   Narrow : constant Scope := (Connect_TCP, 16#0A00_0200#, 24, 80, 443, False, 4);
   Listener : constant Scope := (Listen_TCP, 16#0A00_020F#, 32, 8080, 8080, False, 4);
   Decoded : Scope;
   Success : Boolean;
   Grants : Network_Grants.Table;
   Tag, Other_Tag, New_Tag : Unsigned_64;
   Capacity : constant Network_Grants.Reservation :=
     Network_Grants.Reservation'Last;
begin
   declare
      package Policy renames CuBit.Launch_Policy;
      use type Policy.Network_Approval;
   begin
      --  NetSurf is retired: its old name no longer carries browser
      --  approval.
      pragma Assert (Policy.Desktop_Approval ("netsurf.app", 42, 42) =
        Policy.No_Network);
      pragma Assert (Policy.Desktop_Approval ("cubitshell.app", 0, 0) =
        Policy.No_Network);
      pragma Assert (Policy.Desktop_Approval
        ("cubitshell.app", Unsigned_64'Last, Unsigned_64'Last) = Policy.No_Network);
      pragma Assert (Policy.Desktop_Approval ("other.app", 42, 42) =
        Policy.No_Network);
      pragma Assert (Policy.Desktop_Approval ("/cubitshell.app", 42, 42) =
        Policy.No_Network);
      pragma Assert (Policy.Desktop_Approval ("cubitshell.app", 42, 42) =
        Policy.Browser_Outbound);
      pragma Assert (Policy.Desktop_Approval ("cubitshell.app", 41, 42) =
        Policy.No_Network);
      pragma Assert (Policy.Desktop_Approval ("cubitshell.apps", 42, 42) =
        Policy.No_Network);
      pragma Assert (Policy.Allows (Policy.Browser_Outbound, Broad_Outbound_TCP));
      pragma Assert (Policy.Allows (Policy.Browser_Outbound, Narrow));
      pragma Assert (not Policy.Allows (Policy.Browser_Outbound, Listener));
      pragma Assert (not Policy.Allows (Policy.Browser_Outbound, Denied_Scope));
      pragma Assert (not Policy.Allows (Policy.No_Network, Broad_Outbound_TCP));
      pragma Assert (Policy.Allows (Policy.Declared_Network, Listener));
   end;
   pragma Assert (not Valid (Denied_Scope));
   pragma Assert (Valid (Broad_Outbound_TCP));
   pragma Assert (Valid (Narrow) and Valid (Listener));
   pragma Assert (Allows (Narrow, Connect_TCP, 16#0A00_0201#, 80));
   pragma Assert (Allows (Narrow, Connect_TCP, 16#0A00_02FF#, 443));
   pragma Assert (not Allows (Narrow, Connect_TCP, 16#0A00_0301#, 80));
   pragma Assert (not Allows (Narrow, Connect_TCP, 16#0A00_0201#, 79));
   pragma Assert (not Allows (Narrow, Connect_TCP, 16#0A00_0201#, 444));
   pragma Assert (not Allows (Narrow, Listen_TCP, 16#0A00_0201#, 80));
   pragma Assert (not Allows (Broad_Outbound_TCP, Connect_TCP, 16#0101_0101#, 0));
   pragma Assert (Allows (Listener, Listen_TCP, 16#0A00_020F#, 8080));
   pragma Assert (not Allows (Listener, Listen_TCP, 16#0A00_0210#, 8080));
   pragma Assert (not Allows (Listener, Listen_TCP, 16#0A00_020F#, 8081));
   pragma Assert (Includes (Broad_Outbound_TCP, Narrow));
   pragma Assert (not Includes (Narrow, Broad_Outbound_TCP));
   pragma Assert (not Includes (Broad_Outbound_TCP, Listener));
   for Prefix in Prefix_Length loop
      declare
         S : constant Scope := (Connect_TCP, 0, Prefix, 1, 65535, False, 4);
      begin
         Decode (0, Descriptor (S), Decoded, Success);
         pragma Assert (Success and Decoded = S);
      end;
   end loop;
   Decode (Unsigned_64 (Narrow.Network), Descriptor (Narrow), Decoded, Success);
   pragma Assert (Success and Decoded = Narrow);
   Decode (16#0A00_0201#, Descriptor (Narrow), Decoded, Success);
   pragma Assert (not Success); -- host bits in network prefix
   Decode (0, Shift_Left (Unsigned_64'(1), 63), Decoded, Success);
   pragma Assert (not Success); -- no action
   Decode (0, Descriptor (Narrow) and not Shift_Left (Unsigned_64'(2 ** Connection_Bits - 1), 49),
           Decoded, Success);
   pragma Assert (not Success); -- no connections declared
   Decode (Unsigned_64 (Narrow.Network),
           Descriptor ((Narrow with delta Connections => Connection_Count'Last)),
           Decoded, Success);
   pragma Assert (Success and Decoded.Connections = Connection_Count'Last);
   pragma Assert (Includes (Narrow, (Narrow with delta Connections => 1)));
   pragma Assert (not Includes (Narrow, (Narrow with delta Connections => 5)));
   Decode (Unsigned_64'Last, Descriptor (Narrow), Decoded, Success);
   pragma Assert (not Success); -- address truncation forbidden
   for Invalid_Prefix in 33 .. 255 loop
      Decode (0, Descriptor (Broad_Outbound_TCP) or
              Shift_Left (Unsigned_64 (Invalid_Prefix), 32), Decoded, Success);
      pragma Assert (not Success);
   end loop;
   pragma Assert (not Network_Grants.Owned (Grants, 42, 0));
   Network_Grants.Install (Grants, 0, Narrow, Capacity, Tag, Success);
   pragma Assert (not Success and Tag = 0);
   Network_Grants.Install (Grants, 42, Denied_Scope, Capacity, Tag, Success);
   pragma Assert (not Success and Tag = 0);
   Network_Grants.Install (Grants, 42, Narrow, Capacity, Tag, Success);
   pragma Assert (Success and Tag >= First_Grant_Tag);
   pragma Assert (Network_Grants.Allows (Grants, 42, Tag, Connect_TCP, 16#0A00_0202#, 443));
   pragma Assert (not Network_Grants.Allows (Grants, 43, Tag, Connect_TCP, 16#0A00_0202#, 443));
   pragma Assert (not Network_Grants.May_Resolve (Grants, 42, Tag));
   Network_Grants.Install (Grants, 43, Broad_Outbound_TCP, Capacity, Other_Tag, Success);
   pragma Assert (Success and Other_Tag /= Tag);
   pragma Assert (Network_Grants.May_Resolve (Grants, 43, Other_Tag));
   pragma Assert (not Network_Grants.May_Resolve (Grants, 42, Other_Tag));
   Network_Grants.Release (Grants, 43, Tag);
   pragma Assert (Network_Grants.Owned (Grants, 42, Tag));
   Network_Grants.Release (Grants, 42, Tag);
   pragma Assert (not Network_Grants.Owned (Grants, 42, Tag));
   Network_Grants.Install (Grants, 42, Listener, Capacity, New_Tag, Success);
   pragma Assert (Success and New_Tag /= Tag);
   pragma Assert (not Network_Grants.Owned (Grants, 42, Tag));
   for I in 3 .. Network_Grants.Maximum_Grants loop
      Network_Grants.Install (Grants, 42, Narrow, Capacity, New_Tag, Success);
      pragma Assert (Success);
   end loop;
   Network_Grants.Install (Grants, 42, Narrow, Capacity, New_Tag, Success);
   pragma Assert (not Success and New_Tag = 0);
   --  Reservations: declared channels are reserved at install, never
   --  overcommitted, and charged one per open channel.
   declare
      Small : Network_Grants.Table;
      First_Tag, Second_Tag, Third_Tag : Unsigned_64;
      Ten : constant Network_Grants.Reservation := 10;
   begin
      Network_Grants.Install (Small, 42, Narrow, Ten, First_Tag, Success);
      pragma Assert (Success and Network_Grants.Reserved (Small) = 4);
      Network_Grants.Install (Small, 43, Narrow, Ten, Second_Tag, Success);
      pragma Assert (Success and Network_Grants.Reserved (Small) = 8);
      Network_Grants.Install (Small, 44, Narrow, Ten, Third_Tag, Success);
      pragma Assert (not Success and Third_Tag = 0); -- 12 > 10
      Network_Grants.Install (Small, 44, (Narrow with delta Connections => 2), Ten, Third_Tag, Success);
      pragma Assert (Success and Network_Grants.Reserved (Small) = 10);
      for I in 1 .. 4 loop
         Network_Grants.Charge (Small, 42, First_Tag, Success);
         pragma Assert (Success);
      end loop;
      Network_Grants.Charge (Small, 42, First_Tag, Success);
      pragma Assert (not Success); -- all four declared channels open
      Network_Grants.Charge (Small, 43, First_Tag, Success);
      pragma Assert (not Success); -- another owner's grant
      pragma Assert (Network_Grants.In_Use (Small, Second_Tag) = 0);
      Network_Grants.Refund (Small, First_Tag);
      pragma Assert (Network_Grants.In_Use (Small, First_Tag) = 3);
      Network_Grants.Charge (Small, 42, First_Tag, Success);
      pragma Assert (Success and Network_Grants.In_Use (Small, First_Tag) = 4);
      Network_Grants.Release (Small, 43, Second_Tag);
      pragma Assert (Network_Grants.Reserved (Small) = 6);
      Network_Grants.Install (Small, 45, Narrow, Ten, Second_Tag, Success);
      pragma Assert (Success and Network_Grants.Reserved (Small) = 10);
      Network_Grants.Charge (Small, 42, 0, Success);
      pragma Assert (not Success);
      --  Owner 42 exits: its scope and reservation go, others stay.
      declare
         Released : Network_Grants.Tag_List;
      begin
         Network_Grants.Release_Owner (Small, 42, Released);
         pragma Assert (Released (1) = First_Tag);
         pragma Assert (for all I in 2 .. Released'Last => Released (I) = 0);
         pragma Assert (Network_Grants.Reserved (Small) = 6);
         pragma Assert (not Network_Grants.Owned (Small, 42, First_Tag));
         pragma Assert (Network_Grants.Owned (Small, 44, Third_Tag));
         pragma Assert (Network_Grants.In_Use (Small, First_Tag) = 0);
         Network_Grants.Charge (Small, 42, First_Tag, Success);
         pragma Assert (not Success); -- stale tag after exit
         Network_Grants.Release_Owner (Small, 0, Released);
         pragma Assert (for all T of Released => T = 0);
         pragma Assert (Network_Grants.Reserved (Small) = 6);
      end;
   end;
   UDP_Tests.Run;
   Ada.Text_IO.Put_Line ("Network authority: scope decoding, CIDR/ports, direction, DNS, owner, stale tags, bounded table, reservations, owner exit PASS");
end Main;
