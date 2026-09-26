pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Network_Authority; use CuBit.Network_Authority;
with CuBit.Launch_Policy;
with Network_Grants;
with UDP_Channels; use UDP_Channels;

package body UDP_Tests is
   procedure Authority is
      NTP : constant Scope := (Connect_UDP, 16#0A00_0202#, 32, 123, 123, False);
      Named : constant Scope := (Connect_UDP, 0, 0, 123, 123, True);
      Decoded : Scope;
      Success : Boolean;
      Grants : Network_Grants.Table;
      Tag, Named_Tag : Unsigned_64;
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
      pragma Assert (not Includes (NTP, (Connect_TCP, 16#0A00_0202#, 32, 123, 123, False)));
      pragma Assert (Includes (Named, (Connect_UDP, 16#0A00_0202#, 32, 123, 123, False)));
      pragma Assert (not Includes (NTP, Named));
      --  Browser approval is TCP only; UDP needs declared approval.
      pragma Assert (not Policy.Allows (Policy.Browser_Outbound, NTP));
      pragma Assert (Policy.Allows (Policy.Declared_Network, NTP));
      Decode (Unsigned_64 (NTP.Network), Descriptor (NTP), Decoded, Success);
      pragma Assert (Success and Decoded = NTP);
      pragma Assert (Shift_Right (Descriptor (NTP), 40) = 3);
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
      Network_Grants.Install (Grants, 42, NTP, Tag, Success);
      pragma Assert (Success);
      pragma Assert (Network_Grants.Allows (Grants, 42, Tag, Connect_UDP, 16#0A00_0202#, 123));
      pragma Assert (not Network_Grants.Allows (Grants, 42, Tag, Connect_TCP, 16#0A00_0202#, 123));
      pragma Assert (not Network_Grants.May_Resolve (Grants, 42, Tag));
      Network_Grants.Install (Grants, 42, Named, Named_Tag, Success);
      pragma Assert (Success and Network_Grants.May_Resolve (Grants, 42, Named_Tag));
      pragma Assert (not Network_Grants.May_Resolve (Grants, 43, Named_Tag));
   end Authority;

   procedure Channels is
      Item : Table;
      Success, Truncated, Found : Boolean;
      Index : Channel_Index;
      Result : Delivery;
      Length : Natural;
      Small : Byte_Array (1 .. 2);
      Large : Byte_Array (1 .. Maximum_Payload);
      Too_Large : constant Byte_Array (1 .. Maximum_Payload + 1) := [others => 7];
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

      --  Only the exact endpoint reaches the channel.
      Deliver (Item, Port, Peer + 1, 18446, [1, 2], Index, Result);
      pragma Assert (Result = No_Channel);
      Deliver (Item, Port, Peer, 18447, [1, 2], Index, Result);
      pragma Assert (Result = No_Channel);
      Deliver (Item, Port + 1000, Peer, 18446, [1, 2], Index, Result);
      pragma Assert (Result = No_Channel);
      Deliver (Item, Port, Peer, 18446, Too_Large, Index, Result);
      pragma Assert (Result = Oversized and Queued_Count (Item, 3) = 0);
      for N in 1 .. Queue_Depth loop
         Deliver (Item, Port, Peer, 18446, [Unsigned_8 (N), 0, Unsigned_8 (N)], Index, Result);
         pragma Assert (Result = Queued and Index = 3);
      end loop;
      Deliver (Item, Port, Peer, 18446, [9], Index, Result);
      pragma Assert (Result = Queue_Full and Index = 3 and Queued_Count (Item, 3) = Queue_Depth);

      --  FIFO order; a short buffer truncates and still consumes the datagram.
      Take (Item, 3, Small, Length, Truncated, Found);
      pragma Assert (Found and Truncated and Length = 2 and Small = [1, 0]);
      for N in 2 .. Queue_Depth loop
         Take (Item, 3, Large, Length, Truncated, Found);
         pragma Assert (Found and not Truncated and Length = 3 and
                        Large (1) = Unsigned_8 (N) and Large (3) = Unsigned_8 (N));
      end loop;
      Take (Item, 3, Large, Length, Truncated, Found);
      pragma Assert (not Found and Length = 0);
      Take (Item, 5, Large, Length, Truncated, Found);
      pragma Assert (not Found); -- other channels are untouched

      --  Maximum payload round trip.
      Large := [others => 16#A5#];
      Deliver (Item, Port, Peer, 18446, Large, Index, Result);
      pragma Assert (Result = Queued);
      Large := [others => 0];
      Take (Item, 3, Large, Length, Truncated, Found);
      pragma Assert (Found and Length = Maximum_Payload and not Truncated and
                     (for all B of Large => B = 16#A5#));

      --  Closing discards queued datagrams; the old port stops matching.
      Deliver (Item, Port, Peer, 18446, [1], Index, Result);
      Close (Item, 3);
      pragma Assert (not Active (Item, 3) and Queued_Count (Item, 3) = 0);
      Deliver (Item, Port, Peer, 18446, [1], Index, Result);
      pragma Assert (Result = No_Channel);
      Open (Item, 3, Peer, 18446, Success);
      pragma Assert (Success and Local_Port (Item, 3) /= Port and Queued_Count (Item, 3) = 0);

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
         "FIFO/truncation, queue bounds, port uniqueness and wrap PASS");
   end Run;
end UDP_Tests;
