with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Network_Authority; use CuBit.Network_Authority;
with Network_Grants;
with CuBit.Launch_Policy;

procedure Main is
   Narrow : constant Scope := (Connect_TCP, 16#0A00_0200#, 24, 80, 443, False);
   Listener : constant Scope := (Listen_TCP, 16#0A00_020F#, 32, 8080, 8080, False);
   Decoded : Scope;
   Success : Boolean;
   Grants : Network_Grants.Table;
   Tag, Other_Tag, New_Tag : Unsigned_64;
begin
   declare
      package Policy renames CuBit.Launch_Policy;
      use type Policy.Network_Approval;
   begin
      pragma Assert (Policy.Desktop_Approval ("netsurf.app", 42, 42) =
        Policy.Browser_Outbound);
      pragma Assert (Policy.Desktop_Approval ("netsurf.app", 41, 42) =
        Policy.No_Network);
      pragma Assert (Policy.Desktop_Approval ("netsurf.app", 0, 0) =
        Policy.No_Network);
      pragma Assert (Policy.Desktop_Approval
        ("netsurf.app", Unsigned_64'Last, Unsigned_64'Last) = Policy.No_Network);
      pragma Assert (Policy.Desktop_Approval ("other.app", 42, 42) =
        Policy.No_Network);
      pragma Assert (Policy.Desktop_Approval ("netsurf.appx", 42, 42) =
        Policy.No_Network);
      pragma Assert (Policy.Desktop_Approval ("/netsurf.app", 42, 42) =
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
         S : constant Scope := (Connect_TCP, 0, Prefix, 1, 65535, False);
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
   pragma Assert (not Success); -- reserved metadata bits
   Decode (Unsigned_64'Last, Descriptor (Narrow), Decoded, Success);
   pragma Assert (not Success); -- address truncation forbidden
   for Invalid_Prefix in 33 .. 255 loop
      Decode (0, Descriptor (Broad_Outbound_TCP) or
              Shift_Left (Unsigned_64 (Invalid_Prefix), 32), Decoded, Success);
      pragma Assert (not Success);
   end loop;
   pragma Assert (not Network_Grants.Owned (Grants, 42, 0));
   Network_Grants.Install (Grants, 0, Narrow, Tag, Success);
   pragma Assert (not Success and Tag = 0);
   Network_Grants.Install (Grants, 42, Denied_Scope, Tag, Success);
   pragma Assert (not Success and Tag = 0);
   Network_Grants.Install (Grants, 42, Narrow, Tag, Success);
   pragma Assert (Success and Tag >= First_Grant_Tag);
   pragma Assert (Network_Grants.Allows (Grants, 42, Tag, Connect_TCP, 16#0A00_0202#, 443));
   pragma Assert (not Network_Grants.Allows (Grants, 43, Tag, Connect_TCP, 16#0A00_0202#, 443));
   pragma Assert (not Network_Grants.May_Resolve (Grants, 42, Tag));
   Network_Grants.Install (Grants, 43, Broad_Outbound_TCP, Other_Tag, Success);
   pragma Assert (Success and Other_Tag /= Tag);
   pragma Assert (Network_Grants.May_Resolve (Grants, 43, Other_Tag));
   pragma Assert (not Network_Grants.May_Resolve (Grants, 42, Other_Tag));
   Network_Grants.Release (Grants, 43, Tag);
   pragma Assert (Network_Grants.Owned (Grants, 42, Tag));
   Network_Grants.Release (Grants, 42, Tag);
   pragma Assert (not Network_Grants.Owned (Grants, 42, Tag));
   Network_Grants.Install (Grants, 42, Listener, New_Tag, Success);
   pragma Assert (Success and New_Tag /= Tag);
   pragma Assert (not Network_Grants.Owned (Grants, 42, Tag));
   for I in 3 .. Network_Grants.Maximum_Grants loop
      Network_Grants.Install (Grants, 42, Narrow, New_Tag, Success);
      pragma Assert (Success);
   end loop;
   Network_Grants.Install (Grants, 42, Narrow, New_Tag, Success);
   pragma Assert (not Success and New_Tag = 0);
   Ada.Text_IO.Put_Line ("Network authority: scope decoding, CIDR/ports, direction, DNS, owner, stale tags, bounded table PASS");
end Main;
