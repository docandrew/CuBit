------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Internet_Checksum;
with ICMPv4_Error;

package body IPv4_ICMP with
  SPARK_Mode,
  Refined_State => (State => (Tokens, Refill_At))
is
   use IPv4_Header;

   Echo_Reply     : constant := 0;
   Echo_Request   : constant := 8;
   ICMP_Protocol  : constant := 1;
   Default_TTL    : constant := 64;
   Header_Bytes   : constant := 8;          --  type, code, checksum, id, sequence
   Checksum_At    : constant := 2;
   Identifier_At  : constant := 4;
   Sequence_At    : constant := 6;
   Probe_Bytes    : constant := 32;
   Replies_Per_Ms : constant := 1;
   Reply_Burst    : constant := 50;

   subtype Token_Count is Natural range 0 .. Reply_Burst;
   Tokens    : Token_Count := Reply_Burst;
   Refill_At : Unsigned_64 := 0;

   subtype Message_Length is Natural range Header_Bytes .. Maximum_Message;

   function U16 (B : Bytes; I : Natural) return Unsigned_16 is
     (Shift_Left (Unsigned_16 (B (I)), 8) or Unsigned_16 (B (I + 1)))
   with Pre => I >= B'First and then I < B'Last;

   --  Frame and send Message (its checksum filled in here), if both
   --  addresses are unicast.
   procedure Send_ICMP
     (Our_MAC, To_MAC : Net.MACAddress; Source, Destination : Address; Message : Bytes)
   with Pre => Message'First = 0 and then Message'Length in Message_Length
   is
      Length : constant Natural := Minimum_Size + Message'Length;
      Packet : Bytes (0 .. Length - 1) := [others => 0];
      Frame  : Bytes (0 .. IPv4_Frame.Ethernet_Header + Length - 1) := [others => 0];
      Sum    : Unsigned_16;
   begin
      if not IPv4_Frame.Unicast (Source) or else not IPv4_Frame.Unicast (Destination) then
         return;
      end if;
      Packet (Minimum_Size .. Packet'Last) := Message;
      Packet (Minimum_Size + Checksum_At) := 0;
      Packet (Minimum_Size + Checksum_At + 1) := 0;
      Sum := Internet_Checksum.Of_Bytes
        (Internet_Checksum.Bytes (Packet (Minimum_Size .. Packet'Last)));
      Packet (Minimum_Size + Checksum_At) := Unsigned_8 (Shift_Right (Sum, 8));
      Packet (Minimum_Size + Checksum_At + 1) := Unsigned_8 (Sum and 16#FF#);
      Build ((Size => Minimum_Size, Total_Length => Length, Protocol => ICMP_Protocol,
              TTL => Default_TTL, Source => Source, Destination => Destination),
             DF => True, B => Packet);
      for K in 0 .. 5 loop
         Frame (K) := To_MAC (K);
         Frame (6 + K) := Our_MAC (K);
      end loop;
      Frame (12) := IPv4_Frame.Type_High;
      Frame (13) := IPv4_Frame.Type_Low;
      Frame (IPv4_Frame.Ethernet_Header .. Frame'Last) := Packet;
      pragma Assert (Frame (IPv4_Frame.Header_At) = Packet (0));
      pragma Assert (IPv4_Frame.Address_At (Frame, IPv4_Frame.Source_At) =
                     [Packet (12), Packet (13), Packet (14), Packet (15)]);
      pragma Assert (IPv4_Frame.Address_At (Frame, IPv4_Frame.Destination_At) =
                     [Packet (16), Packet (17), Packet (18), Packet (19)]);
      Send (Frame);
   end Send_ICMP;

   --  One token per echo reply, refilled with time.
   procedure Take_Token (Now : Unsigned_64; Granted : out Boolean) is
   begin
      if Now > Refill_At then
         Tokens := Natural'Min
           (Reply_Burst,
            Tokens + Natural (Unsigned_64'Min
              (Unsigned_64 (Reply_Burst), (Now - Refill_At) * Replies_Per_Ms)));
         Refill_At := Now;
      end if;
      Granted := Tokens > 0;
      if Granted then
         Tokens := Tokens - 1;
      end if;
   end Take_Token;

   procedure Handle
     (H : Header; Message : Bytes; Our_MAC, From_MAC : Net.MACAddress; Now : Unsigned_64)
   with Pre => Message'First = 0 and then Message'Length in Message_Length
   is
      Granted : Boolean;
   begin
      if Internet_Checksum.Of_Bytes (Internet_Checksum.Bytes (Message)) /= 0 then
         return;
      end if;
      case Message (0) is
         when Echo_Request =>
            --  Only between unicast addresses: never an amplifier.
            if IPv4_Frame.Unicast (H.Source) and then IPv4_Frame.Unicast (H.Destination) then
               Take_Token (Now, Granted);
               if Granted then
                  declare
                     Reply : Bytes := Message;
                  begin
                     Reply (0) := Echo_Reply;
                     Send_ICMP (Our_MAC, From_MAC, H.Destination, H.Source, Reply);
                  end;
               end if;
            end if;
         when ICMPv4_Error.Destination_Unreachable | ICMPv4_Error.Time_Exceeded =>
            Error_Arrived (Message);
         when Echo_Reply =>
            if U16 (Message, Identifier_At) = Echo_Identifier then
               Echo_Answered (H.Source, U16 (Message, Sequence_At));
            end if;
         when others =>
            null;
      end case;
   end Handle;

   procedure Receive
     (Packet : Bytes; Our_MAC, From_MAC : Net.MACAddress; Now : Unsigned_64)
   is
      H : Header;
      Buffer : Bytes (0 .. Maximum_Message - 1) := [others => 0];
      Length : Natural;
   begin
      Parse (Packet, H);
      Length := H.Total_Length - H.Size;
      if Length < Header_Bytes or else Length > Maximum_Message then
         return;
      end if;
      --  Copied to a fixed buffer: its length is the header's.
      Buffer (0 .. Length - 1) := Packet (H.Size .. H.Total_Length - 1);
      Handle (H, Buffer (0 .. Length - 1), Our_MAC, From_MAC, Now);
   end Receive;

   procedure Echo
     (Our_MAC, To_MAC : Net.MACAddress; Ours, Destination : Address;
      Sequence : Unsigned_16)
   is
      Message : Bytes (0 .. Header_Bytes + Probe_Bytes - 1) := [others => 0];
   begin
      Message (0) := Echo_Request;
      Message (Identifier_At) := Unsigned_8 (Echo_Identifier / 256);
      Message (Identifier_At + 1) := Unsigned_8 (Echo_Identifier mod 256);
      Message (Sequence_At) := Unsigned_8 (Shift_Right (Sequence, 8));
      Message (Sequence_At + 1) := Unsigned_8 (Sequence and 16#FF#);
      for K in 0 .. Probe_Bytes - 1 loop
         Message (Header_Bytes + K) := Unsigned_8 (K);
      end loop;
      Send_ICMP (Our_MAC, To_MAC, Ours, Destination, Message);
   end Echo;

end IPv4_ICMP;
