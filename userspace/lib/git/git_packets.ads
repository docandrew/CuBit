------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  git's pkt-line framing (SELF-003 step 2), read from untrusted bytes in
--  pieces of any size: one whole packet at a time.
--
--  @description
--  git's protocol documentation (gitprotocol-common, "pkt-line Format";
--  gitprotocol-v2): a packet is four hexadecimal digits giving its length,
--  the four included, then that many bytes less four of payload, at most
--  LARGE_PACKET_MAX (65520) in all. Three lengths are special: 0000 is a
--  flush packet, 0001 a delimiter, 0002 the end of a response; 0003 is
--  never valid. 0004 is an empty data packet.
--
--  Proved at level 2 (tests/git-codecs): Step takes input only from Next
--  on and moves Next forward; a packet's payload is exactly its declared
--  length; a refusal is final.
------------------------------------------------------------------------------
pragma Ada_2022;
with Inflate_Streams;

package Git_Packets with SPARK_Mode, Pure is
   subtype Byte is Inflate_Streams.Byte;
   subtype Byte_Array is Inflate_Streams.Byte_Array;

   LENGTH_DIGITS : constant := 4;
   --  gitprotocol-common: LARGE_PACKET_MAX.
   MAXIMUM_PACKET_BYTES : constant := 65_520;
   MAXIMUM_PAYLOAD_BYTES : constant := MAXIMUM_PACKET_BYTES - LENGTH_DIGITS;
   subtype Payload_Length is Natural range 0 .. MAXIMUM_PAYLOAD_BYTES;

   type Packet_Kind is
     (Data_Packet,          --  length 0004 and up: a payload
      Flush_Packet,         --  0000
      Delimiter_Packet,     --  0001: separates sections of a v2 message
      Response_End_Packet); --  0002: the end of a v2 response

   type Refusal is
     (No_Refusal,
      Bad_Length,       --  not four hexadecimal digits
      Reserved_Length,  --  0003
      Too_Long,         --  beyond MAXIMUM_PACKET_BYTES
      Truncated);       --  the input ended inside a packet

   function Name (Why : Refusal) return String is
     (case Why is
         when No_Refusal => "No_Refusal",
         when Bad_Length => "Bad_Length",
         when Reserved_Length => "Reserved_Length",
         when Too_Long => "Too_Long",
         when Truncated => "Truncated");

   type Event_Kind is
     (Need_More,   --  every byte given was taken; give more
      Packet,      --  a whole packet: Packet says which; a data packet's payload is Payload
      Refused);    --  Why says why; final

   type Event is record
      Kind : Event_Kind := Need_More;
      Why : Refusal := No_Refusal;
      Packet : Packet_Kind := Data_Packet;
   end record;

   type Reader is private;

   function Valid (R : Reader) return Boolean;
   function Failed (R : Reader) return Boolean;
   function Failure (R : Reader) return Refusal;
   --  Whether the last event was a packet, and its payload (empty unless a
   --  data packet).
   function Has_Packet (R : Reader) return Boolean;
   function Length (R : Reader) return Payload_Length;
   function Payload (R : Reader) return Byte_Array
   with Pre => Has_Packet (R), Post => Payload'Result'First = 1 and then Payload'Result'Length = Length (R);
   --  Whether the reader is between packets (the input may end here).
   function Between_Packets (R : Reader) return Boolean;

   procedure Start (R : out Reader)
   with Post => Valid (R) and then not Failed (R) and then not Has_Packet (R) and then Between_Packets (R);

   procedure Step (R : in out Reader; Input : Byte_Array; Next : in out Positive; E : out Event)
   with
     Pre => Valid (R) and then Input'Last < Positive'Last and then Next in Input'First .. Input'Last + 1,
     Post =>
       Valid (R)
       and then Next in Next'Old .. Input'Last + 1
       and then (if Failed (R'Old) then E.Kind = Refused and then Next = Next'Old)
       and then (E.Kind = Refused) = Failed (R)
       and then (if E.Kind = Refused then E.Why = Failure (R) and then E.Why /= No_Refusal)
       and then (if E.Kind = Need_More then Next = Input'Last + 1)
       and then Has_Packet (R) = (E.Kind = Packet)
       and then (if E.Kind = Packet then Between_Packets (R)
                   and then (if E.Packet /= Data_Packet then Length (R) = 0));

   --  The input ended: Truncated inside a packet.
   procedure Input_Ended (R : in out Reader; E : out Event)
   with Pre => Valid (R),
        Post => Valid (R) and then E.Kind in Need_More | Refused
          and then (E.Kind = Refused) = Failed (R)
          and then (if E.Kind = Need_More then Between_Packets (R));
private
   subtype Payload_Buffer is Byte_Array (1 .. MAXIMUM_PAYLOAD_BYTES);
   type Phase_Kind is (In_Length, In_Payload, Failed_Phase);

   type Reader is record
      Phase : Phase_Kind := In_Length;
      Why : Refusal := No_Refusal;
      Digits_Read : Natural range 0 .. LENGTH_DIGITS - 1 := 0;
      Declared : Natural range 0 .. 16 ** LENGTH_DIGITS - 1 := 0;
      Buffer : Payload_Buffer := [others => 0];
      Filled : Payload_Length := 0;
      Expected : Payload_Length := 0;
      Has_Packet : Boolean := False;
   end record;

   --  The digits read so far bound the value (a linear form of
   --  Declared < 16 ** Digits_Read).
   function Bounded (R : Reader) return Boolean is
     (case R.Digits_Read is
         when 0 => R.Declared = 0,
         when 1 => R.Declared < 16,
         when 2 => R.Declared < 16 ** 2,
         when others => R.Declared < 16 ** 3);

   function Valid (R : Reader) return Boolean is
     ((R.Phase = Failed_Phase) = (R.Why /= No_Refusal)
      and then (if R.Phase /= Failed_Phase then Bounded (R))
      and then R.Filled <= R.Expected
      and then (if R.Phase = In_Payload then R.Filled < R.Expected and then R.Digits_Read = 0)
      and then (if R.Has_Packet then R.Phase = In_Length and then R.Digits_Read = 0
                  and then R.Filled = R.Expected));

   function Failed (R : Reader) return Boolean is (R.Phase = Failed_Phase);
   function Failure (R : Reader) return Refusal is (R.Why);
   function Has_Packet (R : Reader) return Boolean is (R.Has_Packet);
   function Length (R : Reader) return Payload_Length is (R.Filled);
   function Payload (R : Reader) return Byte_Array is (R.Buffer (1 .. R.Filled));
   function Between_Packets (R : Reader) return Boolean is (R.Phase = In_Length and then R.Digits_Read = 0);
end Git_Packets;
