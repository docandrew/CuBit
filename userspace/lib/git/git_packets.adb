------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The pkt-line reader (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;

package body Git_Packets with SPARK_Mode is
   --  gitprotocol-v2: the special lengths.
   FLUSH_LENGTH : constant := 0;
   DELIMITER_LENGTH : constant := 1;
   RESPONSE_END_LENGTH : constant := 2;
   RESERVED_LENGTH_VALUE : constant := 3;
   HEX_BASE : constant := 16;

   --  The value of a hexadecimal digit, or HEX_BASE if it is not one.
   function Hex_Value (C : Byte) return Natural is
     (case C is
         when Character'Pos ('0') .. Character'Pos ('9') => Natural (C) - Character'Pos ('0'),
         when Character'Pos ('a') .. Character'Pos ('f') => Natural (C) - Character'Pos ('a') + 10,
         when Character'Pos ('A') .. Character'Pos ('F') => Natural (C) - Character'Pos ('A') + 10,
         when others => HEX_BASE)
   with Post => Hex_Value'Result <= HEX_BASE;

   procedure Start (R : out Reader) is
   begin
      R := (others => <>);
   end Start;

   procedure Step (R : in out Reader; Input : Byte_Array; Next : in out Positive; E : out Event) is
      procedure Refuse (Why : Refusal)
      with Pre => Why /= No_Refusal and then not R.Has_Packet and then R.Filled <= R.Expected
                    and then R.Phase /= In_Payload,
           Post => Failed (R) and then Valid (R) and then E.Kind = Refused and then E.Why = Why
                     and then Failure (R) = Why and then not R.Has_Packet,
           Global => (In_Out => R, Output => E)
      is
      begin
         R.Phase := Failed_Phase;
         R.Why := Why;
         E := (Kind => Refused, Why => Why, others => <>);
      end Refuse;

      procedure Emit (Kind : Packet_Kind)
      with Pre => R.Phase = In_Length and then R.Digits_Read = 0 and then R.Declared = 0
                    and then R.Filled = R.Expected
                    and then R.Why = No_Refusal
                    and then (if Kind /= Data_Packet then R.Filled = 0),
           Post => Valid (R) and then R.Has_Packet and then E.Kind = Packet and then E.Packet = Kind
                     and then R.Filled = R.Filled'Old,
           Global => (In_Out => R, Output => E)
      is
      begin
         R.Has_Packet := True;
         E := (Kind => Packet, Packet => Kind, others => <>);
      end Emit;
   begin
      if R.Phase = Failed_Phase then
         E := (Kind => Refused, Why => R.Why, others => <>);
         return;
      end if;
      R.Has_Packet := False;
      loop
         pragma Loop_Invariant (Valid (R) and then not Failed (R) and then not R.Has_Packet);
         pragma Loop_Invariant (Next in Next'Loop_Entry .. Input'Last + 1);
         if Next > Input'Last then
            E := (Kind => Need_More, others => <>);
            return;
         end if;
         if R.Phase = In_Length then
            declare
               Digit : constant Natural := Hex_Value (Input (Next));
            begin
               Next := Next + 1;
               if Digit = HEX_BASE then
                  Refuse (Bad_Length);
                  return;
               end if;
               if R.Digits_Read < LENGTH_DIGITS - 1 then
                  R.Declared := R.Declared * HEX_BASE + Digit;
                  R.Digits_Read := R.Digits_Read + 1;
               else
                  declare
                     Declared : constant Natural := R.Declared * HEX_BASE + Digit;
                  begin
                  R.Declared := 0;
                  R.Digits_Read := 0;
                  R.Filled := 0;
                  R.Expected := 0;
                  case Declared is
                     when FLUSH_LENGTH =>
                        Emit (Flush_Packet);
                        return;
                     when DELIMITER_LENGTH =>
                        Emit (Delimiter_Packet);
                        return;
                     when RESPONSE_END_LENGTH =>
                        Emit (Response_End_Packet);
                        return;
                     when RESERVED_LENGTH_VALUE =>
                        Refuse (Reserved_Length);
                        return;
                     when LENGTH_DIGITS =>
                        Emit (Data_Packet);
                        return;
                     when LENGTH_DIGITS + 1 .. MAXIMUM_PACKET_BYTES =>
                        R.Expected := Declared - LENGTH_DIGITS;
                        R.Phase := In_Payload;
                     when others =>
                        Refuse (Too_Long);
                        return;
                  end case;
                  end;
               end if;
            end;
         else
            --  In_Payload: as much of the payload as the input holds.
            while Next <= Input'Last and then R.Filled < R.Expected loop
               pragma Loop_Invariant (Next in Next'Loop_Entry .. Input'Last);
               pragma Loop_Invariant (R.Filled < R.Expected and then R.Phase = In_Payload);
               pragma Loop_Invariant (Valid (R) and then not R.Has_Packet and then R.Digits_Read = 0);
               R.Filled := R.Filled + 1;
               R.Buffer (R.Filled) := Input (Next);
               Next := Next + 1;
            end loop;
            if R.Filled = R.Expected then
               R.Phase := In_Length;
               Emit (Data_Packet);
               return;
            end if;
         end if;
      end loop;
   end Step;

   procedure Input_Ended (R : in out Reader; E : out Event) is
   begin
      R.Has_Packet := False;
      if R.Phase = Failed_Phase then
         E := (Kind => Refused, Why => R.Why, others => <>);
      elsif R.Phase = In_Length and then R.Digits_Read = 0 then
         E := (Kind => Need_More, others => <>);
      else
         R.Phase := Failed_Phase;
         R.Why := Truncated;
         R.Filled := 0;
         R.Expected := 0;
         R.Digits_Read := 0;
         R.Declared := 0;
         E := (Kind => Refused, Why => Truncated, others => <>);
      end if;
   end Input_Ended;
end Git_Packets;
