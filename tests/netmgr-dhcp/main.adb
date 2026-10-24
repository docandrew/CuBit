------------------------------------------------------------------------------
--  Hosted tests: netmgr's DHCP client (DHCP_Client) against a simulated
--  server: DORA, INIT-REBOOT acknowledged and refused, renewal, rebinding
--  and expiry, retransmission and giving up, and malformed or foreign
--  replies (wrong transaction, wrong hardware address, truncated options,
--  random bytes).
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Interfaces; use Interfaces;
with CuBit.Net_Link;
with CuBit.Net_Management;
with DHCP_Client;

procedure Main is
   package NL renames CuBit.Net_Link;
   package NM renames CuBit.Net_Management;
   package D renames DHCP_Client;
   use type D.Phase, D.Lease, D.Message_Type;

   package R8 is new Ada.Numerics.Discrete_Random (Unsigned_8);
   Gen : R8.Generator;

   Failures : Natural := 0;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   MAC : constant NL.Hardware_Address := [16#52#, 16#54#, 0, 16#12#, 16#34#, 16#56#];
   function IP (A, B, C, E : Unsigned_8) return NM.IPv4_Word is
     (Unsigned_32 (A) or Shift_Left (Unsigned_32 (B), 8) or
      Shift_Left (Unsigned_32 (C), 16) or Shift_Left (Unsigned_32 (E), 24));
   Offered : constant NM.IPv4_Word := IP (10, 0, 3, 15);
   Server  : constant NM.IPv4_Word := IP (10, 0, 3, 2);
   Mask24  : constant NM.IPv4_Word := IP (255, 255, 255, 0);
   DNS     : constant NM.IPv4_Word := IP (10, 0, 3, 3);

   Out_B, In_B : D.Bytes := [others => 0];
   C : D.Client := D.Create (MAC);
   A : D.Action;
   Now : D.Time := 1_000;

   --  The sent message's XID.
   function Sent_XID return Unsigned_32 is
     (Shift_Left (Unsigned_32 (Out_B (D.XID_At)), 24) or Shift_Left (Unsigned_32 (Out_B (D.XID_At + 1)), 16) or
      Shift_Left (Unsigned_32 (Out_B (D.XID_At + 2)), 8) or Unsigned_32 (Out_B (D.XID_At + 3)));

   --  The sent message's option Code as (found, first 4 value bytes as a word).
   procedure Option (Code : Unsigned_8; Found : out Boolean; Value : out Unsigned_32) is
      P : Natural := D.Options_At;
   begin
      Found := False;
      Value := 0;
      while P < D.Sent_Bytes and then Out_B (P) /= D.Option_End loop
         if Out_B (P) = D.Option_Pad then
            P := P + 1;
         else
            if Out_B (P) = Code then
               Found := True;
               for I in 0 .. Natural'Min (3, Natural (Out_B (P + 1)) - 1) loop
                  Value := Value or Shift_Left (Unsigned_32 (Out_B (P + 2 + I)), 8 * I);
               end loop;
               return;
            end if;
            P := P + 2 + Natural (Out_B (P + 1));
         end if;
      end loop;
   end Option;

   function Has (Code : Unsigned_8) return Boolean is
      F : Boolean; V : Unsigned_32;
   begin
      Option (Code, F, V);
      return F;
   end Has;

   function Value_Of (Code : Unsigned_8) return Unsigned_32 is
      F : Boolean; V : Unsigned_32;
   begin
      Option (Code, F, V);
      return V;
   end Value_Of;

   function Ciaddr return NM.IPv4_Word is
     (Unsigned_32 (Out_B (D.Client_Addr_At)) or Shift_Left (Unsigned_32 (Out_B (D.Client_Addr_At + 1)), 8) or
      Shift_Left (Unsigned_32 (Out_B (D.Client_Addr_At + 2)), 16) or
      Shift_Left (Unsigned_32 (Out_B (D.Client_Addr_At + 3)), 24));

   --  A server reply into In_B; returns its length.
   function Reply (Kind : D.Message_Type; XID : Unsigned_32; Address : NM.IPv4_Word;
                   Lease_Seconds : Unsigned_32 := 3_600; Mask : NM.IPv4_Word := Mask24;
                   Hardware : NL.Hardware_Address := MAC; T1 : Unsigned_32 := 0) return Natural
   is
      P : Natural := D.Options_At;
      procedure Put (V : Unsigned_8) is
      begin
         In_B (P) := V;
         P := P + 1;
      end Put;
      procedure Put_IP (W : Unsigned_32) is
      begin
         for I in 0 .. 3 loop Put (Unsigned_8 (Shift_Right (W, 8 * I) and 16#FF#)); end loop;
      end Put_IP;
      procedure Put_Count (W : Unsigned_32) is
      begin
         for I in reverse 0 .. 3 loop Put (Unsigned_8 (Shift_Right (W, 8 * I) and 16#FF#)); end loop;
      end Put_Count;
   begin
      In_B := [others => 0];
      In_B (D.Op_At) := D.Op_Reply;
      In_B (D.Hardware_Type_At) := D.Hardware_Ethernet;
      In_B (D.Hardware_Len_At) := D.Hardware_Length;
      for I in 0 .. 3 loop
         In_B (D.XID_At + I) := Unsigned_8 (Shift_Right (XID, 8 * (3 - I)) and 16#FF#);
         In_B (D.Your_Addr_At + I) := Unsigned_8 (Shift_Right (Address, 8 * I) and 16#FF#);
      end loop;
      for I in NL.Octet_Index loop In_B (D.Hardware_At + I) := Hardware (I); end loop;
      for I in D.Cookie'Range loop In_B (D.Cookie_At + I) := D.Cookie (I); end loop;
      Put (D.Option_Message_Type); Put (1); Put (Unsigned_8 (D.Message_Type'Enum_Rep (Kind)));
      Put (D.Option_Server_ID); Put (4); Put_IP (Server);
      if Kind /= D.Nak then
         Put (D.Option_Subnet_Mask); Put (4); Put_IP (Mask);
         Put (D.Option_Router); Put (4); Put_IP (Server);
         Put (D.Option_DNS); Put (4); Put_IP (DNS);
         Put (D.Option_Lease_Time); Put (4); Put_Count (Lease_Seconds);
         if T1 /= 0 then Put (D.Option_Renewal); Put (4); Put_Count (T1); end if;
      end if;
      Put (D.Option_End);
      return Natural'Max (P, 300);
   end Reply;

   procedure Deliver (Length : Natural) is
   begin
      D.Receive (C, Now, In_B, Length, Out_B, A);
   end Deliver;

   procedure Run_To (T : D.Time) is
   begin
      Now := T;
      D.Tick (C, Now, 16#BEEF_0000# + Unsigned_32 (T mod 1000), Out_B, A);
   end Run_To;

   Held : D.Lease;
   XID : Unsigned_32;
   Ignore : Natural;
begin
   R8.Reset (Gen, 2131);

   --  DORA.
   D.Link_Up (C, Now, 16#1234_5678#, Out_B, A);
   Check (A.Send and then A.Length = D.Sent_Bytes and then C.Current = D.Selecting, "discover sent");
   Check (Out_B (D.Op_At) = D.Op_Request and then Out_B (D.Flags_At) = D.Broadcast_Flag and then
          Sent_XID = 16#1234_5678#, "discover header");
   Check (Value_Of (D.Option_Message_Type) = 1 and then not Has (D.Option_Requested) and then
          not Has (D.Option_Server_ID) and then Has (D.Option_Client_ID), "discover options");
   Deliver (Reply (D.Offer, 16#1234_5679#, Offered));
   Check (not A.Send and then C.Current = D.Selecting, "offer for another transaction ignored");
   Deliver (Reply (D.Offer, 16#1234_5678#, Offered, Hardware => [2, 0, 0, 0, 0, 9]));
   Check (not A.Send and then C.Current = D.Selecting, "offer for another client ignored");
   Deliver (Reply (D.Offer, 16#1234_5678#, Offered, Mask => IP (255, 0, 255, 0)));
   Check (not A.Send, "offer with a broken netmask ignored");
   Deliver (Reply (D.Offer, 16#1234_5678#, Offered));
   Check (A.Send and then C.Current = D.Requesting and then Value_Of (D.Option_Message_Type) = 3 and then
          Value_Of (D.Option_Requested) = Offered and then Value_Of (D.Option_Server_ID) = Server and then
          Ciaddr = 0, "request for the offer");
   Deliver (Reply (D.Ack, 16#1234_5678#, Offered));
   Check (A.Configure and then not A.Deconfigure and then C.Current = D.Bound and then
          A.Configured.Address = Offered and then A.Configured.Prefix = 24 and then
          A.Configured.Router = Server and then A.Configured.DNS_1 = DNS and then
          A.Configured.Seconds = 3_600 and then A.Configured.T1 = 1_800 and then
          A.Configured.T2 = 3_150, "acknowledged lease");
   Held := A.Configured;
   Deliver (Reply (D.Ack, 16#1234_5678#, Offered));
   Check (not A.Configure, "a second acknowledgement while bound changes nothing");

   --  Renewal at T1, rebinding at T2, expiry at the lease's end.
   Check (D.Next_Deadline (C) = Now + 1_800_000, "T1 deadline");
   Run_To (Now + 1_799_999);
   Check (not A.Send and then C.Current = D.Bound, "nothing before T1");
   Run_To (Now + 1);
   XID := Sent_XID;
   Check (A.Send and then C.Current = D.Renewing and then Value_Of (D.Option_Message_Type) = 3 and then
          Ciaddr = Offered and then not Has (D.Option_Requested) and then not Has (D.Option_Server_ID),
          "renewal request");
   Deliver (Reply (D.Ack, XID, Offered, T1 => 600));
   Check (A.Configure and then C.Current = D.Bound and then A.Configured.T1 = 600, "renewed");
   declare
      Start : constant D.Time := Now;
   begin
      Run_To (Start + 600_000);
      Check (C.Current = D.Renewing and then A.Send, "second renewal");
      Run_To (Start + 3_150_000);
      Check (C.Current = D.Rebinding and then A.Send, "rebinding at T2");
      Run_To (Start + 3_600_000);
      Check (A.Deconfigure and then A.Send and then C.Current = D.Selecting and then not C.Has_Lease,
             "lease expired: address removed, discover again");
   end;

   --  Get a lease again, then the link goes down and comes back.
   XID := Sent_XID;
   Deliver (Reply (D.Offer, XID, Offered));
   Deliver (Reply (D.Ack, XID, Offered));
   Check (C.Current = D.Bound, "bound again");
   D.Link_Down (C);
   Check (C.Current = D.Idle and then C.Has_Lease and then D.Next_Deadline (C) = D.Time'Last,
          "link down keeps the lease, sends nothing");
   Run_To (Now + 10_000_000);
   Check (not A.Send and then not A.Deconfigure, "idle client is quiet");
   D.Link_Up (C, Now, 16#CAFE_0001#, Out_B, A);
   Check (A.Send and then C.Current = D.Rebooting and then Value_Of (D.Option_Message_Type) = 3 and then
          Value_Of (D.Option_Requested) = Offered and then not Has (D.Option_Server_ID) and then
          Ciaddr = 0 and then Out_B (D.Flags_At) = D.Broadcast_Flag, "INIT-REBOOT request");
   Deliver (Reply (D.Ack, 16#CAFE_0001#, Offered));
   Check (A.Configure and then not A.Deconfigure and then C.Current = D.Bound and then
          A.Configured.Address = Offered, "INIT-REBOOT acknowledged");

   --  INIT-REBOOT on another network: NAK.
   D.Link_Down (C);
   D.Link_Up (C, Now, 16#CAFE_0002#, Out_B, A);
   Deliver (Reply (D.Nak, 16#CAFE_0002#, 0));
   Check (A.Deconfigure and then A.Send and then C.Current = D.Selecting and then not C.Has_Lease and then
          Value_Of (D.Option_Message_Type) = 1, "INIT-REBOOT refused: discover");

   --  INIT-REBOOT with no answer gives up after the attempts allowed.
   XID := Sent_XID;
   Ignore := Reply (D.Offer, XID, Offered);
   Deliver (Ignore);
   Deliver (Reply (D.Ack, XID, Offered));
   D.Link_Down (C);
   D.Link_Up (C, Now, 16#CAFE_0003#, Out_B, A);
   declare
      Sends : Natural := 1;
   begin
      for Round in 1 .. 20 loop
         exit when C.Current /= D.Rebooting;
         Run_To (D.Next_Deadline (C));
         if A.Send then Sends := Sends + 1; end if;
      end loop;
      Check (C.Current = D.Selecting and then Sends = D.Maximum_Attempts + 1 and then C.Has_Lease,
             "unanswered INIT-REBOOT falls back to discover");
   end;

   --  Retransmission doubles to 64 s.
   declare
      Last : D.Time := Now;
      Gaps_OK : Boolean := True;
      Expected : D.Time := D.Retry_First_MS * 2;
   begin
      Last := Now;
      for Round in 1 .. 6 loop
         Run_To (D.Next_Deadline (C));
         Gaps_OK := Gaps_OK and then A.Send;
         if Round > 1 then
            Gaps_OK := Gaps_OK and then Now - Last = Expected;
            Expected := D.Time'Min (Expected * 2, D.Retry_Last_MS);
         end if;
         Last := Now;
      end loop;
      Check (Gaps_OK, "retransmission backs off to 64 s");
   end;

   --  Malformed replies: truncated options, a bad cookie, too short, ours
   --  (a request), random bytes.
   XID := Sent_XID;
   declare
      L : constant Natural := Reply (D.Offer, XID, Offered);
   begin
      In_B (D.Options_At + 1) := 250;   --  message type's length runs past the end
      Deliver (L);
      Check (not A.Send and then C.Current = D.Selecting, "truncated option refused");
      Ignore := Reply (D.Offer, XID, Offered);
      In_B (D.Cookie_At) := 0;
      Deliver (L);
      Check (not A.Send, "bad cookie refused");
      Ignore := Reply (D.Offer, XID, Offered);
      Deliver (NM.Minimum_Message - 1);
      Check (not A.Send, "short message refused");
      Ignore := Reply (D.Offer, XID, Offered);
      In_B (D.Op_At) := D.Op_Request;
      Deliver (L);
      Check (not A.Send, "a request is no reply");
   end;
   for Round in 1 .. 100_000 loop
      for I in In_B'Range loop
         In_B (I) := R8.Random (Gen);
      end loop;
      if Round mod 2 = 0 then
         --  Plausible header, random options.
         In_B (D.Op_At) := D.Op_Reply;
         In_B (D.Hardware_Type_At) := D.Hardware_Ethernet;
         In_B (D.Hardware_Len_At) := D.Hardware_Length;
         for I in 0 .. 3 loop
            In_B (D.XID_At + I) := Unsigned_8 (Shift_Right (XID, 8 * (3 - I)) and 16#FF#);
         end loop;
         for I in NL.Octet_Index loop In_B (D.Hardware_At + I) := MAC (I); end loop;
         for I in D.Cookie'Range loop In_B (D.Cookie_At + I) := D.Cookie (I); end loop;
      end if;
      declare
         Before : constant D.Client := C;
         L : constant Natural := Natural (R8.Random (Gen)) * 8;
      begin
         Deliver (L);
         if C.Current /= D.Selecting then
            --  It took a random offer: fine, as long as what it holds is
            --  sane; go back to selecting for the next round.
            Check (C.Current = D.Requesting and then C.Offer.Address /= 0, "random offer is sane");
            C := Before;
         end if;
      end;
   end loop;

   --  Prefixes.
   declare
      P : NM.IPv4_Prefix;
      OK : Boolean;
   begin
      D.Prefix_Of (IP (255, 255, 255, 0), P, OK);
      Check (OK and then P = 24, "mask /24");
      D.Prefix_Of (0, P, OK);
      Check (OK and then P = 0, "mask /0");
      D.Prefix_Of (IP (255, 255, 255, 255), P, OK);
      Check (OK and then P = 32, "mask /32");
      D.Prefix_Of (IP (255, 255, 0, 255), P, OK);
      Check (not OK, "non-contiguous mask refused");
   end;

   if Failures = 0 then
      Put_Line ("netmgr-dhcp: PASS");
   else
      Put_Line ("netmgr-dhcp: FAIL" & Failures'Image);
      raise Program_Error;
   end if;
end Main;
