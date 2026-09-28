pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Network_Authority; use CuBit.Network_Authority;
with CuBit.Launch_Policy;
with Network_Grants;
with UDP_Channels; use UDP_Channels;

package body UDP_Tests is
   procedure Authority is
      NTP : constant Scope := (Connect_UDP, 16#0A00_0202#, 32, 123, 123, False, 4);
      Named : constant Scope := (Connect_UDP, 0, 0, 123, 123, True, 4);
      Decoded : Scope;
      Success : Boolean;
      Grants : Network_Grants.Table;
      Tag, Named_Tag : Unsigned_64;
      Capacity : constant Network_Grants.Reservation := 8;
      package Policy renames CuBit.Launch_Policy;
   begin
      pragma Assert (Valid (NTP) and Valid (Named));
      pragma Assert (Allows (NTP, Connect_UDP, 16#0A00_0202#, 123));
      pragma Assert (not Allows (NTP, Connect_TCP, 16#0A00_0202#, 123));
      pragma Assert (not Allows (NTP, Listen_TCP, 16#0A00_0202#, 123));
      pragma Assert (not Allows (NTP, Connect_UDP, 16#0A00_0203#, 123));
      pragma Assert (not Allows (NTP, Connect_UDP, 16#0A00_0202#, 124));
      pragma Assert (not Allows (Broad_Outbound_TCP, Connect_UDP, 16#0A00_0202#, 123));
      --  A TCP ceiling never contains UDP authority, and the reverse.
      pragma Assert (not Includes (Broad_Outbound_TCP, NTP));
      pragma Assert (not Includes (NTP, (Connect_TCP, 16#0A00_0202#, 32, 123, 123, False, 4)));
      pragma Assert (Includes (Named, (Connect_UDP, 16#0A00_0202#, 32, 123, 123, False, 4)));
      pragma Assert (not Includes (NTP, Named));
      --  Browser approval is TCP only; UDP needs declared approval.
      pragma Assert (not Policy.Allows (Policy.Browser_Outbound, NTP));
      pragma Assert (Policy.Allows (Policy.Declared_Network, NTP));
      Decode (Unsigned_64 (NTP.Network), Descriptor (NTP), Decoded, Success);
      pragma Assert (Success and Decoded = NTP);
      pragma Assert ((Shift_Right (Descriptor (NTP), 40) and 255) = 3);
      pragma Assert (Shift_Right (Descriptor (NTP), 49) = 4);
      Decode (0, Descriptor (Named), Decoded, Success);
      pragma Assert (Success and Decoded = Named);
      for Unknown in Unsigned_64'(4) .. 255 loop
         Decode (0, (Descriptor (Named) and not Shift_Left (Unsigned_64'(255), 40)) or
                 Shift_Left (Unknown, 40), Decoded, Success);
         pragma Assert (not Success and Decoded = Denied_Scope);
      end loop;
      Decode (0, Descriptor (Named) and not Shift_Left (Unsigned_64'(255), 40),
              Decoded, Success);
      pragma Assert (not Success); -- operation zero
      Network_Grants.Install (Grants, 42, NTP, Capacity, Tag, Success);
      pragma Assert (Success);
      pragma Assert (Network_Grants.Allows (Grants, 42, Tag, Connect_UDP, 16#0A00_0202#, 123));
      pragma Assert (not Network_Grants.Allows (Grants, 42, Tag, Connect_TCP, 16#0A00_0202#, 123));
      pragma Assert (not Network_Grants.May_Resolve (Grants, 42, Tag));
      Network_Grants.Install (Grants, 42, Named, Capacity, Named_Tag, Success);
      pragma Assert (Success and Network_Grants.May_Resolve (Grants, 42, Named_Tag));
      pragma Assert (not Network_Grants.May_Resolve (Grants, 43, Named_Tag));
   end Authority;

   procedure Channels is
      Item : Table;
      Success : Boolean;
      Index : Channel_Index;
      Result : Delivery;
      Peer : constant Unsigned_32 := 16#0A00_0202#;
      Port : Unsigned_16;
      Previous : Unsigned_16;
   begin
      Open (Item, 0, 0, 123, Success);
      pragma Assert (not Success and not Active (Item, 0));
      Open (Item, 0, Peer, 0, Success);
      pragma Assert (not Success and not Active (Item, 0));
      for I in Channel_Index loop
         Open (Item, I, Peer, 18446, Success);
         pragma Assert (Success and Local_Port (Item, I) >= First_Ephemeral);
         for J in Channel_Index'First .. I - 1 loop
            pragma Assert (Local_Port (Item, J) /= Local_Port (Item, I));
         end loop;
      end loop;
      Open (Item, 3, Peer, 18446, Success);
      pragma Assert (not Success); -- an active slot is not reopened
      Port := Local_Port (Item, 3);

      --  Only the exact endpoint reaches the channel (its datagrams go to
      --  the channel's receive ring, CuBit.Datagram_Rings).
      Deliver (Item, Port, Peer + 1, 18446, 2, Index, Result);
      pragma Assert (Result = No_Channel);
      Deliver (Item, Port, Peer, 18447, 2, Index, Result);
      pragma Assert (Result = No_Channel);
      Deliver (Item, Port + 1000, Peer, 18446, 2, Index, Result);
      pragma Assert (Result = No_Channel);
      Deliver (Item, Port, Peer, 18446, Maximum_Payload + 1, Index, Result);
      pragma Assert (Result = Oversized);
      Deliver (Item, Port, Peer, 18446, Maximum_Payload, Index, Result);
      pragma Assert (Result = Matched and Index = 3);

      --  Closing stops the old port matching.
      Close (Item, 3);
      pragma Assert (not Active (Item, 3));
      Deliver (Item, Port, Peer, 18446, 1, Index, Result);
      pragma Assert (Result = No_Channel);
      Open (Item, 3, Peer, 18446, Success);
      pragma Assert (Success and Local_Port (Item, 3) /= Port);

      --  Wrap the ephemeral cursor repeatedly: ports stay in range and never
      --  collide with the seven channels that remain open.
      Previous := Local_Port (Item, 3);
      for Round in 1 .. 40_000 loop
         Close (Item, 3);
         Open (Item, 3, Peer, 18446, Success);
         pragma Assert (Success);
         pragma Assert (Local_Port (Item, 3) /= Previous);
         for J in Channel_Index loop
            pragma Assert
              (J = 3 or else Local_Port (Item, J) /= Local_Port (Item, 3));
         end loop;
         Previous := Local_Port (Item, 3);
      end loop;
   end Channels;

   procedure Run is
   begin
      Authority;
      Channels;
      Ada.Text_IO.Put_Line
        ("UDP: scope direction/decoding, launch policy, endpoint filtering, " &
         "exact matching, size bound, port uniqueness and wrap PASS");
   end Run;
end UDP_Tests;
