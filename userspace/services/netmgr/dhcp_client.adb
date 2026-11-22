------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body DHCP_Client with SPARK_Mode is

   Milliseconds : constant := 1_000;

   function Later (Now, Delay_MS : Time) return Time is
     (if Delay_MS > Time'Last - Now then Time'Last else Now + Delay_MS);

   function After_Seconds (From : Time; Seconds : Unsigned_32) return Time is
     (Later (From, Time (Seconds) * Milliseconds));

   function Next_Retry (R : Time) return Time is
     (if R >= Retry_Last_MS / 2 then Retry_Last_MS else R * 2)
   with Pre  => R in Retry_First_MS .. Retry_Last_MS,
        Post => Next_Retry'Result in Retry_First_MS .. Retry_Last_MS;

   function Create (Hardware : NL.Hardware_Address) return Client is
     ((Hardware => Hardware, others => <>));

   ---------------------------------------------------------------------------
   --  Wire
   ---------------------------------------------------------------------------
   procedure Put_Word (B : in out Bytes; At_Byte : Byte_Index; W : NM.IPv4_Word)
   with Pre => At_Byte <= Byte_Index'Last - 3
   is
   begin
      B (At_Byte)     := Unsigned_8 (W and 16#FF#);
      B (At_Byte + 1) := Unsigned_8 (Shift_Right (W, 8) and 16#FF#);
      B (At_Byte + 2) := Unsigned_8 (Shift_Right (W, 16) and 16#FF#);
      B (At_Byte + 3) := Unsigned_8 (Shift_Right (W, 24) and 16#FF#);
   end Put_Word;

   function Get_Word (B : Bytes; At_Byte : Byte_Index) return NM.IPv4_Word is
     (Unsigned_32 (B (At_Byte)) or Shift_Left (Unsigned_32 (B (At_Byte + 1)), 8) or
      Shift_Left (Unsigned_32 (B (At_Byte + 2)), 16) or Shift_Left (Unsigned_32 (B (At_Byte + 3)), 24))
   with Pre => At_Byte <= Byte_Index'Last - 3;

   --  Network order (big-endian) 32-bit value.
   function Get_Count (B : Bytes; At_Byte : Byte_Index) return Unsigned_32 is
     (Shift_Left (Unsigned_32 (B (At_Byte)), 24) or Shift_Left (Unsigned_32 (B (At_Byte + 1)), 16) or
      Shift_Left (Unsigned_32 (B (At_Byte + 2)), 8) or Unsigned_32 (B (At_Byte + 3)))
   with Pre => At_Byte <= Byte_Index'Last - 3;

   procedure Build (B : in out Bytes; Kind : Message_Type; XID : Unsigned_32;
                    Hardware : NL.Hardware_Address;
                    Requested, Server, Client_Address : NM.IPv4_Word)
   is
      P : Byte_Index := Options_At;
      procedure Put (Value : Unsigned_8) with
        Pre  => P < Sent_Bytes - 1,
        Post => P = P'Old + 1
      is
      begin
         B (P) := Value;
         P := P + 1;
      end Put;
   begin
      B (0 .. Sent_Bytes - 1) := [others => 0];
      B (Op_At) := Op_Request;
      B (Hardware_Type_At) := Hardware_Ethernet;
      B (Hardware_Len_At) := Hardware_Length;
      --  The transaction identifier, in network order.
      B (XID_At)     := Unsigned_8 (Shift_Right (XID, 24) and 16#FF#);
      B (XID_At + 1) := Unsigned_8 (Shift_Right (XID, 16) and 16#FF#);
      B (XID_At + 2) := Unsigned_8 (Shift_Right (XID, 8) and 16#FF#);
      B (XID_At + 3) := Unsigned_8 (XID and 16#FF#);
      B (Flags_At) := Broadcast_Flag;
      Put_Word (B, Client_Addr_At, Client_Address);
      for I in NL.Octet_Index loop
         B (Hardware_At + I) := Hardware (I);
      end loop;
      for I in Cookie'Range loop
         B (Cookie_At + I) := Cookie (I);
      end loop;
      Put (Option_Message_Type); Put (1); Put (Unsigned_8 (Message_Type'Enum_Rep (Kind)));
      --  Client identifier: hardware type, then the address (RFC 2132 9.14).
      Put (Option_Client_ID); Put (1 + Hardware_Length); Put (Hardware_Ethernet);
      for I in NL.Octet_Index loop
         Put (Hardware (I));
      end loop;
      if Requested /= NM.No_IPv4 then
         Put (Option_Requested); Put (4);
         Put_Word (B, P, Requested);
         P := P + 4;
      end if;
      if Server /= NM.No_IPv4 then
         Put (Option_Server_ID); Put (4);
         Put_Word (B, P, Server);
         P := P + 4;
      end if;
      Put (Option_Parameters); Put (6);
      Put (Option_Subnet_Mask); Put (Option_Router); Put (Option_DNS);
      Put (Option_Lease_Time); Put (Option_Renewal); Put (Option_Rebinding);
      B (P) := Option_End;
   end Build;

   procedure Prefix_Of (Mask : NM.IPv4_Word; Prefix : out NM.IPv4_Prefix; OK : out Boolean) is
      --  The mask in network order as an ordinary number: a.b.c.d -> a << 24 ...
      M : constant Unsigned_32 :=
        Shift_Left (Mask and 16#FF#, 24) or Shift_Left (Shift_Right (Mask, 8) and 16#FF#, 16) or
        Shift_Left (Shift_Right (Mask, 16) and 16#FF#, 8) or (Shift_Right (Mask, 24) and 16#FF#);
   begin
      Prefix := 0;
      OK := False;
      for N in NM.IPv4_Prefix loop
         if M = (if N = 0 then 0 else Shift_Left (Unsigned_32'Last, 32 - N)) then
            Prefix := N;
            OK := True;
            return;
         end if;
      end loop;
   end Prefix_Of;

   procedure Parse (B : Bytes; Length : Natural; XID : Unsigned_32;
                    Hardware : NL.Hardware_Address; Item : out Reply; OK : out Boolean)
   is
      P : Natural := Options_At;
      Kind_Value : Unsigned_8 := 0;
      Mask : NM.IPv4_Word := NM.No_IPv4;
      Have_Mask : Boolean := False;
      Prefix_OK : Boolean;
      Have_T1, Have_T2 : Boolean := False;
      Got_XID : constant Unsigned_32 :=
        Shift_Left (Unsigned_32 (B (XID_At)), 24) or Shift_Left (Unsigned_32 (B (XID_At + 1)), 16) or
        Shift_Left (Unsigned_32 (B (XID_At + 2)), 8) or Unsigned_32 (B (XID_At + 3));
   begin
      Item := (others => <>);
      OK := False;
      if Length not in NM.Message_Length or else B (Op_At) /= Op_Reply or else
        B (Hardware_Type_At) /= Hardware_Ethernet or else B (Hardware_Len_At) /= Hardware_Length or else
        Got_XID /= XID or else
        (for some I in NL.Octet_Index => B (Hardware_At + I) /= Hardware (I)) or else
        (for some I in Cookie'Range => B (Cookie_At + I) /= Cookie (I))
      then
         return;
      end if;
      Item.Offer.Address := Get_Word (B, Your_Addr_At);
      --  Options: code, length, value; Pad alone; End stops. An option
      --  running past Length makes the whole message malformed.
      while P < Length loop
         pragma Loop_Invariant (P in Options_At .. Length - 1 and then Length <= NM.Maximum_Message);
         declare
            Code : constant Unsigned_8 := B (P);
         begin
            exit when Code = Option_End;
            if Code = Option_Pad then
               P := P + 1;
            else
               if P + 1 >= Length then
                  return;
               end if;
               declare
                  Size  : constant Natural := Natural (B (P + 1));
                  Value : constant Natural := P + 2;
               begin
                  if Value + Size > Length then
                     return;
                  end if;
                  case Code is
                     when Option_Message_Type =>
                        if Size = 1 then Kind_Value := B (Value); end if;
                     when Option_Subnet_Mask =>
                        if Size = 4 then Mask := Get_Word (B, Value); Have_Mask := True; end if;
                     when Option_Router =>
                        if Size >= 4 then Item.Offer.Router := Get_Word (B, Value); end if;
                     when Option_DNS =>
                        if Size >= 4 then Item.Offer.DNS_1 := Get_Word (B, Value); end if;
                        if Size >= 8 then Item.Offer.DNS_2 := Get_Word (B, Value + 4); end if;
                     when Option_Server_ID =>
                        if Size = 4 then Item.Offer.Server := Get_Word (B, Value); end if;
                     when Option_Lease_Time =>
                        if Size = 4 then Item.Offer.Seconds := Get_Count (B, Value); end if;
                     when Option_Renewal =>
                        if Size = 4 then Item.Offer.T1 := Get_Count (B, Value); Have_T1 := True; end if;
                     when Option_Rebinding =>
                        if Size = 4 then Item.Offer.T2 := Get_Count (B, Value); Have_T2 := True; end if;
                     when others =>
                        null;
                  end case;
                  P := Value + Size;
               end;
            end if;
         end;
      end loop;
      if Kind_Value = Unsigned_8 (Message_Type'Enum_Rep (Nak)) then
         Item.Kind := Nak;
         OK := True;
         return;
      elsif Kind_Value = Unsigned_8 (Message_Type'Enum_Rep (Offer)) then
         Item.Kind := Offer;
      elsif Kind_Value = Unsigned_8 (Message_Type'Enum_Rep (Ack)) then
         Item.Kind := Ack;
      else
         return;
      end if;
      --  An offer or acknowledgement must give an address and a usable mask.
      if Item.Offer.Address = NM.No_IPv4 then
         return;
      end if;
      if Have_Mask then
         Prefix_Of (Mask, Item.Offer.Prefix, Prefix_OK);
         if not Prefix_OK then
            return;
         end if;
      else
         Item.Offer.Prefix := NM.IPv4_Prefix'Last;
      end if;
      --  RFC 2131 4.4.5's defaults: T1 half the lease, T2 seven eighths.
      if not Have_T1 then
         Item.Offer.T1 := Item.Offer.Seconds / 2;
      end if;
      if not Have_T2 then
         Item.Offer.T2 := Item.Offer.Seconds / 8 * 7;
      end if;
      OK := True;
   end Parse;

   ---------------------------------------------------------------------------
   --  The machine
   ---------------------------------------------------------------------------
   procedure Send_Now (C : in out Client; Now : Time; Out_Bytes : in out Bytes; Result : in out Action)
   with Pre  => Valid (C) and then C.Current in Selecting | Requesting | Renewing | Rebinding | Rebooting,
        Post => Valid (C) and then Result.Send and then Result.Length = Sent_Bytes
                and then C.Current = C'Old.Current and then C.Has_Lease = C'Old.Has_Lease
                and then C.Held = C'Old.Held
                and then Result.Configure = Result'Old.Configure
                and then Result.Configured = Result'Old.Configured
                and then Result.Deconfigure = Result'Old.Deconfigure
   is
   begin
      case C.Current is
         when Selecting =>
            Build (Out_Bytes, Discover, C.XID, C.Hardware, NM.No_IPv4, NM.No_IPv4, NM.No_IPv4);
         when Requesting =>
            Build (Out_Bytes, Request, C.XID, C.Hardware, C.Offer.Address, C.Offer.Server, NM.No_IPv4);
         when Rebooting =>
            Build (Out_Bytes, Request, C.XID, C.Hardware, C.Held.Address, NM.No_IPv4, NM.No_IPv4);
         when Renewing | Rebinding =>
            Build (Out_Bytes, Request, C.XID, C.Hardware, NM.No_IPv4, NM.No_IPv4,
                   (if C.Has_Lease then C.Held.Address else NM.No_IPv4));
         when Idle | Bound =>
            null;
      end case;
      Result.Send := True;
      Result.Length := Sent_Bytes;
      C.Next_Send := Later (Now, C.Retry);
      C.Retry := Next_Retry (C.Retry);
   end Send_Now;

   --  Start over with DISCOVER.
   procedure Discover_Now (C : in out Client; Now : Time; XID : Unsigned_32;
                           Out_Bytes : in out Bytes; Result : in out Action)
   with Pre  => Valid (C),
        Post => Valid (C) and then C.Current = Selecting and then Result.Send
                and then Result.Length = Sent_Bytes and then C.Has_Lease = C'Old.Has_Lease
                and then C.Held = C'Old.Held
                and then Result.Configure = Result'Old.Configure
                and then Result.Configured = Result'Old.Configured
                and then Result.Deconfigure = Result'Old.Deconfigure
   is
   begin
      C.XID := XID;
      C.Current := Selecting;
      C.Retry := Retry_First_MS;
      C.Attempts := 1;
      Send_Now (C, Now, Out_Bytes, Result);
   end Discover_Now;

   procedure Link_Up (C : in out Client; Now : Time; XID : Unsigned_32;
                      Out_Bytes : in out Bytes; Result : out Action) is
   begin
      Result := (others => <>);
      if C.Has_Lease then
         C.XID := XID;
         C.Current := Rebooting;
         C.Retry := Retry_First_MS;
         C.Attempts := 1;
         Send_Now (C, Now, Out_Bytes, Result);
      else
         Discover_Now (C, Now, XID, Out_Bytes, Result);
      end if;
   end Link_Up;

   procedure Link_Down (C : in out Client) is
   begin
      C.Current := Idle;
      C.Next_Send := Time'Last;
      C.Retry := Retry_First_MS;
      C.Attempts := 0;
   end Link_Down;

   procedure Receive (C : in out Client; Now : Time; In_Bytes : Bytes; Length : Natural;
                      Out_Bytes : in out Bytes; Result : out Action)
   is
      Item : Reply;
      OK   : Boolean;
   begin
      Result := (others => <>);
      if C.Current in Idle | Bound then
         return;
      end if;
      Parse (In_Bytes, Length, C.XID, C.Hardware, Item, OK);
      if not OK then
         return;
      end if;
      case Item.Kind is
         when Offer =>
            if C.Current = Selecting then
               C.Offer := Item.Offer;
               C.Current := Requesting;
               C.Retry := Retry_First_MS;
               C.Attempts := 1;
               Send_Now (C, Now, Out_Bytes, Result);
            end if;
         when Ack =>
            --  Our address is what we asked for (Requesting: the offer's;
            --  otherwise the lease's), from the server we asked.
            if (C.Current = Requesting and then Item.Offer.Address = C.Offer.Address)
              or else (C.Current in Rebooting | Renewing | Rebinding and then C.Has_Lease
                       and then Item.Offer.Address = C.Held.Address)
            then
               if C.Has_Lease and then C.Held.Address /= Item.Offer.Address then
                  Result.Deconfigure := True;
               end if;
               C.Held := Item.Offer;
               C.Has_Lease := True;
               C.Leased_At := Now;
               C.Current := Bound;
               C.Next_Send := Time'Last;
               C.Retry := Retry_First_MS;
               C.Attempts := 0;
               Result.Configure := True;
               Result.Configured := C.Held;
            end if;
         when Nak =>
            --  The address is not ours (another network, or the lease is
            --  gone): give it up and start over.
            if C.Current in Requesting | Rebooting | Renewing | Rebinding then
               if C.Has_Lease then
                  Result.Deconfigure := True;
               end if;
               C.Has_Lease := False;
               C.Held := (others => <>);
               --  A fresh exchange; the server's identifier for it is
               --  derived from the old one (the caller gives new ones on
               --  Link_Up and Tick).
               Discover_Now (C, Now, C.XID + 1, Out_Bytes, Result);
            end if;
         when others =>
            null;
      end case;
   end Receive;

   procedure Tick (C : in out Client; Now : Time; XID : Unsigned_32;
                   Out_Bytes : in out Bytes; Result : out Action) is
   begin
      Result := (others => <>);
      case C.Current is
         when Idle =>
            null;
         when Bound =>
            if C.Has_Lease and then Now >= After_Seconds (C.Leased_At, C.Held.T1) then
               C.XID := XID;
               C.Current := Renewing;
               C.Retry := Retry_First_MS;
               C.Attempts := 1;
               Send_Now (C, Now, Out_Bytes, Result);
            end if;
         when Renewing | Rebinding =>
            if C.Has_Lease and then Now >= After_Seconds (C.Leased_At, C.Held.Seconds) then
               --  The lease ran out (RFC 2131 4.4.5): the address goes.
               Result.Deconfigure := True;
               C.Has_Lease := False;
               C.Held := (others => <>);
               Discover_Now (C, Now, XID, Out_Bytes, Result);
            elsif C.Current = Renewing and then C.Has_Lease
              and then Now >= After_Seconds (C.Leased_At, C.Held.T2)
            then
               C.Current := Rebinding;
               Send_Now (C, Now, Out_Bytes, Result);
            elsif Now >= C.Next_Send then
               Send_Now (C, Now, Out_Bytes, Result);
            end if;
         when Selecting =>
            if Now >= C.Next_Send then
               Send_Now (C, Now, Out_Bytes, Result);
            end if;
         when Requesting | Rebooting =>
            if Now >= C.Next_Send then
               if C.Attempts >= Maximum_Attempts then
                  --  No answer: start over (4.3.2: INIT-REBOOT without an
                  --  answer, and REQUESTING without one, go back to INIT).
                  Discover_Now (C, Now, XID, Out_Bytes, Result);
               else
                  C.Attempts := C.Attempts + 1;
                  Send_Now (C, Now, Out_Bytes, Result);
               end if;
            end if;
      end case;
   end Tick;

   function Next_Deadline (C : Client) return Time is
     (case C.Current is
         when Idle => Time'Last,
         when Bound => (if C.Has_Lease then After_Seconds (C.Leased_At, C.Held.T1) else Time'Last),
         when Renewing | Rebinding =>
           Time'Min (C.Next_Send,
                     (if C.Has_Lease
                      then Time'Min (After_Seconds (C.Leased_At, C.Held.Seconds),
                                     (if C.Current = Renewing
                                      then After_Seconds (C.Leased_At, C.Held.T2) else Time'Last))
                      else Time'Last)),
         when Selecting | Requesting | Rebooting => C.Next_Send);

end DHCP_Client;
