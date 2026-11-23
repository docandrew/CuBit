------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  net.fetch's HTTP/1.1 response reader (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;

package body Http_Responses with SPARK_Mode is

   CR : constant Character := ASCII.CR;
   LF : constant Character := ASCII.LF;
   HTAB : constant Character := ASCII.HT;

   --  RFC 9110 15.2.2, 15.3.5, 15.4.5: no body after these.
   SWITCHING_PROTOCOLS_STATUS : constant := 101;
   NO_CONTENT : constant := 204;
   NOT_MODIFIED : constant := 304;
   INFORMATIONAL_LAST : constant := 199;
   --  "HTTP/1.x NNN": eight bytes of version, a space, three digits
   --  (STATUS_MINIMUM bytes before the CR).
   STATUS_DIGITS_AT : constant := 10;
   STATUS_MINIMUM : constant := 12;
   HEX_DIGITS_MAXIMUM : constant := 15;

   --  RFC 9110 5.6.2: tchar.
   function Token_Character (C : Character) return Boolean is
     (C in 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '!' | '#' | '$' | '%' | '&' | ''' | '*' | '+' | '-'
         | '.' | '^' | '_' | '`' | '|' | '~');

   --  RFC 9110 5.5: field-vchar, SP, HTAB (obs-text included).
   function Value_Character (C : Character) return Boolean is
     (C = HTAB or else C in ' ' .. '~' or else Character'Pos (C) >= 16#80#);

   function Lower (C : Character) return Character is
     (if C in 'A' .. 'Z' then Character'Val (Character'Pos (C) + 32) else C);

   function Same_Ignoring_Case (A, B : String) return Boolean is
     (A'Length = B'Length
      and then (for all K in 0 .. A'Length - 1 => Lower (A (A'First + K)) = Lower (B (B'First + K))));

   function Hex_Value (C : Character) return Natural is
     (case C is
         when '0' .. '9' => Character'Pos (C) - Character'Pos ('0'),
         when 'a' .. 'f' => Character'Pos (C) - Character'Pos ('a') + 10,
         when 'A' .. 'F' => Character'Pos (C) - Character'Pos ('A') + 10,
         when others => 0)
   with Post => Hex_Value'Result <= 15;

   procedure Refuse (R : in out Reader; Why : Refusal; E : out Event)
   with Pre => Valid (R) and then Why /= No_Refusal,
        Post => Valid (R) and then R.Phase = Failed_Phase and then E.Kind = Refused and then E.Why = Why
          and then R.Delivered = R.Delivered'Old and then R.Ceiling = R.Ceiling'Old
          and then R.Head_Read = R.Head_Read'Old
   is
   begin
      R.Phase := Failed_Phase;
      R.Why := Why;
      E := (Kind => Refused, Why => Why, others => <>);
   end Refuse;

   procedure Start (R : out Reader; Ceiling : Body_Count) is
   begin
      R := (Ceiling => Ceiling, others => <>);
   end Start;

   --  The first CRLF in H (From .. H'Last): the CR's index, 0 when none.
   function Find_CRLF (H : String; From : Positive) return Natural
   with Pre => H'First = 1 and then H'Last in 0 .. Positive'Last - 1,
        Post => (if Find_CRLF'Result /= 0 then
                   Find_CRLF'Result >= From and then Find_CRLF'Result < H'Last
                   and then Find_CRLF'Result >= H'First)
   is
   begin
      if From < H'First then
         return 0;
      end if;
      for I in From .. H'Last - 1 loop
         if H (I) = CR and then H (I + 1) = LF then
            return I;
         end if;
      end loop;
      return 0;
   end Find_CRLF;

   --  One field line H (First .. Last), CRLF excluded.
   procedure Field
     (R : in out Reader; H : String; First, Last : Positive; Seen_Length, Seen_Chunked : in out Boolean;
      Why : out Refusal)
   with Pre => H'First = 1 and then First <= Last and then Last <= H'Last and then First >= H'First
                 and then H'Last < Positive'Last,
        Post => R.Phase = R.Phase'Old and then R.Why = R.Why'Old and then R.Ceiling = R.Ceiling'Old
          and then R.Delivered = R.Delivered'Old and then R.Head_Read = R.Head_Read'Old
   is
      Colon : Natural := 0;
      V_First, V_Last : Natural;
   begin
      Why := No_Refusal;
      if H (First) in ' ' | HTAB then
         Why := Folded_Field;
         return;
      end if;
      for I in First .. Last loop
         if H (I) = ':' then
            Colon := I;
            exit;
         end if;
      end loop;
      if Colon <= First or else not (for all I in First .. Colon - 1 => Token_Character (H (I))) then
         Why := Bad_Field;
         return;
      end if;
      V_First := Colon + 1;
      V_Last := Last;
      while V_First <= V_Last and then H (V_First) in ' ' | HTAB loop
         pragma Loop_Invariant (V_First in Colon + 1 .. V_Last and then V_Last = Last);
         pragma Loop_Variant (Increases => V_First);
         V_First := V_First + 1;
      end loop;
      while V_Last >= V_First and then H (V_Last) in ' ' | HTAB loop
         pragma Loop_Invariant (V_Last in V_First .. Last and then V_First >= Colon + 1);
         pragma Loop_Variant (Decreases => V_Last);
         V_Last := V_Last - 1;
      end loop;
      if not (for all I in V_First .. V_Last => Value_Character (H (I))) then
         Why := Bad_Field;
         return;
      end if;
      declare
         Name : constant String := H (First .. Colon - 1);
         Value : constant String := H (V_First .. V_Last);
      begin
         if Same_Ignoring_Case (Name, "content-length") then
            declare
               N : Body_Count := 0;
            begin
               if Value'Length = 0 then
                  Why := Bad_Content_Length;
                  return;
               end if;
               for C of Value loop
                  if C not in '0' .. '9' or else N > (Body_Count'Last - 9) / 10 then
                     Why := Bad_Content_Length;
                     return;
                  end if;
                  N := N * 10 + Body_Count (Character'Pos (C) - Character'Pos ('0'));
               end loop;
               if Seen_Length and then N /= R.Length then
                  Why := Conflicting_Lengths;
                  return;
               end if;
               Seen_Length := True;
               R.Length := N;
            end;
         elsif Same_Ignoring_Case (Name, "transfer-encoding") then
            if Seen_Chunked or else not Same_Ignoring_Case (Value, "chunked") then
               Why := Unsupported_Transfer_Coding;
               return;
            end if;
            Seen_Chunked := True;
         elsif Same_Ignoring_Case (Name, "content-encoding") then
            if not Same_Ignoring_Case (Value, "identity") then
               Why := Unsupported_Content_Coding;
               return;
            end if;
         elsif Same_Ignoring_Case (Name, "location") then
            if Value'Length > MAXIMUM_LOCATION_BYTES then
               Why := Location_Too_Long;
               return;
            end if;
            R.Location_Last := Value'Length;
            R.Location (1 .. R.Location_Last) := Value;
         end if;
      end;
   end Field;

   --  The head in R.Head (1 .. R.Head_Last), which ends in CRLF CRLF.
   procedure Parse_Head (R : in out Reader; Why : out Refusal)
   with Pre => R.Head_Last >= 4,
        Post => R.Phase = R.Phase'Old and then R.Why = R.Why'Old and then R.Ceiling = R.Ceiling'Old
          and then R.Delivered = R.Delivered'Old and then R.Head_Read = R.Head_Read'Old
   is
      H : constant String := R.Head (1 .. R.Head_Last);
      Line_End : Natural := Find_CRLF (H, 1);
      P : Positive;
      Seen_Length, Seen_Chunked : Boolean := False;
      Code : Natural := 0;
   begin
      Why := No_Refusal;
      R.Location_Last := 0;
      R.Length := 0;
      --  "HTTP/1." DIGIT SP 3DIGIT [SP reason-phrase]
      if Line_End <= STATUS_MINIMUM or else H (1 .. 7) /= "HTTP/1." or else H (8) not in '0' .. '9'
        or else H (9) /= ' '
        or else (for some I in STATUS_DIGITS_AT .. STATUS_DIGITS_AT + 2 => H (I) not in '0' .. '9')
        or else (Line_End > STATUS_MINIMUM + 1 and then H (STATUS_MINIMUM + 1) /= ' ')
        or else not (for all I in STATUS_MINIMUM + 1 .. Line_End - 1 => Value_Character (H (I)))
      then
         Why := Bad_Status_Line;
         return;
      end if;
      for I in STATUS_DIGITS_AT .. STATUS_DIGITS_AT + 2 loop
         pragma Loop_Invariant
           (Code < (case I - STATUS_DIGITS_AT is when 0 => 1, when 1 => 10, when others => 100));
         Code := Code * 10 + (Character'Pos (H (I)) - Character'Pos ('0'));
      end loop;
      if Code not in Status_Code then
         Why := Bad_Status_Line;
         return;
      end if;
      R.Status := Code;
      P := Line_End + 2;
      loop
         pragma Loop_Invariant (P in 3 .. H'Last + 1 and then Why = No_Refusal);
         pragma Loop_Invariant (R.Phase = R.Phase'Loop_Entry and then R.Why = R.Why'Loop_Entry
                                and then R.Ceiling = R.Ceiling'Loop_Entry
                                and then R.Delivered = R.Delivered'Loop_Entry
                                and then R.Head_Read = R.Head_Read'Loop_Entry);
         pragma Loop_Variant (Increases => P);
         Line_End := Find_CRLF (H, P);
         if Line_End = 0 then
            --  Not reached: the head ends in CRLF CRLF.
            Why := Bad_Field;
            return;
         end if;
         exit when Line_End = P;
         Field (R, H, P, Line_End - 1, Seen_Length, Seen_Chunked, Why);
         if Why /= No_Refusal then
            return;
         end if;
         P := Line_End + 2;
      end loop;
      if Seen_Length and then Seen_Chunked then
         Why := Length_And_Chunked;
      elsif Code <= INFORMATIONAL_LAST or else Code = NO_CONTENT or else Code = NOT_MODIFIED then
         R.Framing := Length_Delimited;
         R.Length := 0;
      elsif Seen_Chunked then
         R.Framing := Chunked;
      elsif Seen_Length then
         R.Framing := Length_Delimited;
      else
         R.Framing := Close_Delimited;
      end if;
   end Parse_Head;

   --  Bytes into the head until it ends; then the head is parsed.
   procedure Take_Head (R : in out Reader; Input : String; Next : in out Positive; E : out Event)
   with Pre => Valid (R) and then R.Phase = In_Head and then Input'Last < Positive'Last
                 and then Next in Input'First .. Input'Last + 1,
        Post => Valid (R) and then R.Ceiling = R.Ceiling'Old and then R.Delivered = R.Delivered'Old
          and then Next in Next'Old .. Input'Last + 1
          and then E.Kind in Need_More | Head_Ready | Refused
          and then (E.Kind = Refused) = (R.Phase = Failed_Phase)
          and then (if E.Kind = Refused then E.Why /= No_Refusal)
          and then (if E.Kind = Head_Ready then R.Head_Read and then R.Status not in 100 .. 199)
   is
      Why : Refusal;
   begin
      E := (others => <>);
      while Next <= Input'Last loop
         pragma Loop_Invariant (Valid (R) and then R.Phase = In_Head and then E.Kind = Need_More
                                and then R.Ceiling = R.Ceiling'Loop_Entry
                                and then R.Delivered = R.Delivered'Loop_Entry
                                and then Next in Next'Loop_Entry .. Input'Last);
         pragma Loop_Variant (Increases => Next);
         if R.Head_Last = MAXIMUM_HEAD_BYTES then
            Refuse (R, Head_Too_Large, E);
            return;
         end if;
         R.Head_Last := R.Head_Last + 1;
         R.Head (R.Head_Last) := Input (Next);
         Next := Next + 1;
         if R.Head_Last >= 4 and then R.Head (R.Head_Last - 3 .. R.Head_Last) = CR & LF & CR & LF then
            Parse_Head (R, Why);
            if Why /= No_Refusal then
               Refuse (R, Why, E);
               return;
            elsif R.Status = SWITCHING_PROTOCOLS_STATUS then
               Refuse (R, Switching_Protocols, E);
               return;
            elsif R.Status <= INFORMATIONAL_LAST then
               --  An interim response: the final one follows.
               R.Head_Last := 0;
            else
               R.Head_Read := True;
               R.Remaining := 0;
               R.Chunk_Digits := 0;
               case R.Framing is
                  when Length_Delimited =>
                     if R.Length = 0 then
                        R.Phase := Done_Phase;
                     else
                        R.Remaining := R.Length;
                        R.Phase := In_Length_Body;
                     end if;
                  when Chunked =>
                     R.Phase := In_Chunk_Size;
                  when Close_Delimited =>
                     R.Phase := In_Close_Body;
               end case;
               E := (Kind => Head_Ready, others => <>);
               return;
            end if;
         end if;
      end loop;
   end Take_Head;

   --  The chunked framing (RFC 9112 7.1), a byte at a time, and each
   --  chunk's data as one run.
   procedure Chunk_Step (R : in out Reader; Input : String; Next : in out Positive; E : out Event)
   with Pre => Valid (R) and then R.Phase in In_Chunk_Size .. In_Trailer_LF and then Input'Last < Positive'Last
                 and then Next in Input'First .. Input'Last + 1,
        Post => Valid (R) and then R.Ceiling = R.Ceiling'Old and then R.Head_Read
          and then Next in Next'Old .. Input'Last + 1
          and then E.Kind in Need_More | Body_Run | Complete | Refused
          and then (E.Kind = Refused) = (R.Phase = Failed_Phase)
          and then (if E.Kind = Refused then E.Why /= No_Refusal)
          and then (if E.Kind = Complete then R.Phase = Done_Phase and then R.Framing = Chunked)
          and then (if E.Kind = Body_Run then
                      E.First >= Next'Old and then E.First <= E.Last and then E.Last = Next - 1
                      and then E.Last <= Input'Last
                      and then R.Delivered = R.Delivered'Old + Body_Count (E.Last - E.First + 1)
                    else R.Delivered = R.Delivered'Old)
   is
      C : Character;
   begin
      E := (others => <>);
      while Next <= Input'Last loop
         pragma Loop_Invariant (Valid (R) and then R.Phase in In_Chunk_Size .. In_Trailer_LF
                                and then E.Kind = Need_More and then R.Head_Read
                                and then R.Ceiling = R.Ceiling'Loop_Entry
                                and then R.Delivered = R.Delivered'Loop_Entry
                                and then Next in Next'Loop_Entry .. Input'Last);
         pragma Loop_Variant (Increases => Next);
         if R.Phase = In_Chunk_Data then
            declare
               Available : constant Body_Count := Body_Count (Input'Last - Next + 1);
               N : constant Body_Count := Body_Count'Min (Available, R.Remaining);
            begin
               if N > R.Ceiling - R.Delivered then
                  Refuse (R, Body_Too_Large, E);
                  return;
               end if;
               E := (Kind => Body_Run, First => Next, Last => Next + Natural (N) - 1, Why => No_Refusal);
               Next := Next + Natural (N);
               R.Delivered := R.Delivered + N;
               R.Remaining := R.Remaining - N;
               if R.Remaining = 0 then
                  R.Phase := In_Chunk_Data_CR;
               end if;
               return;
            end;
         end if;
         C := Input (Next);
         Next := Next + 1;
         case R.Phase is
            when In_Chunk_Size =>
               if C in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' then
                  if R.Chunk_Digits >= HEX_DIGITS_MAXIMUM or else R.Remaining > MAXIMUM_CHUNK_BYTES / 16 then
                     Refuse (R, Bad_Chunk, E);
                     return;
                  end if;
                  R.Remaining := R.Remaining * 16 + Body_Count (Hex_Value (C));
                  R.Chunk_Digits := R.Chunk_Digits + 1;
               elsif R.Chunk_Digits = 0 then
                  Refuse (R, Bad_Chunk, E);
                  return;
               elsif C = CR then
                  R.Phase := In_Chunk_Size_LF;
               elsif C in ';' | ' ' | HTAB then
                  R.Line_Bytes := 0;
                  R.Phase := In_Chunk_Extension;
               else
                  Refuse (R, Bad_Chunk, E);
                  return;
               end if;
            when In_Chunk_Extension =>
               --  Extensions are read past (RFC 9112 7.1.1), within a ceiling.
               if C = CR then
                  R.Phase := In_Chunk_Size_LF;
               elsif not Value_Character (C) or else R.Line_Bytes = MAXIMUM_HEAD_BYTES then
                  Refuse (R, Bad_Chunk, E);
                  return;
               else
                  R.Line_Bytes := R.Line_Bytes + 1;
               end if;
            when In_Chunk_Size_LF =>
               if C /= LF then
                  Refuse (R, Bad_Chunk, E);
                  return;
               end if;
               R.Chunk_Digits := 0;
               if R.Remaining = 0 then
                  R.Trailer_Bytes := 0;
                  R.Line_Bytes := 0;
                  R.Phase := In_Trailer;
               else
                  R.Phase := In_Chunk_Data;
               end if;
            when In_Chunk_Data_CR =>
               if C /= CR then
                  Refuse (R, Bad_Chunk, E);
                  return;
               end if;
               R.Phase := In_Chunk_Data_LF;
            when In_Chunk_Data_LF =>
               if C /= LF then
                  Refuse (R, Bad_Chunk, E);
                  return;
               end if;
               R.Remaining := 0;
               R.Phase := In_Chunk_Size;
            when In_Trailer =>
               --  Trailer fields are read past, within the head's ceiling.
               if C = CR then
                  R.Phase := In_Trailer_LF;
               elsif not Value_Character (C) or else R.Trailer_Bytes = MAXIMUM_HEAD_BYTES then
                  Refuse (R, Bad_Chunk, E);
                  return;
               else
                  R.Trailer_Bytes := R.Trailer_Bytes + 1;
                  R.Line_Bytes := Natural'Min (R.Line_Bytes + 1, MAXIMUM_HEAD_BYTES);
               end if;
            when In_Trailer_LF =>
               if C /= LF then
                  Refuse (R, Bad_Chunk, E);
                  return;
               elsif R.Line_Bytes = 0 then
                  R.Phase := Done_Phase;
                  E := (Kind => Complete, others => <>);
                  return;
               end if;
               R.Line_Bytes := 0;
               R.Phase := In_Trailer;
            when others =>
               --  In_Chunk_Data is handled above.
               null;
         end case;
      end loop;
   end Chunk_Step;

   procedure Step (R : in out Reader; Input : String; Next : in out Positive; E : out Event) is
      Available : constant Natural := Input'Last - Next + 1;
   begin
      E := (others => <>);
      case R.Phase is
         when Failed_Phase =>
            E := (Kind => Refused, Why => R.Why, others => <>);
         when Done_Phase =>
            if Available > 0 then
               Refuse (R, Extra_Data, E);
            else
               E := (Kind => Complete, others => <>);
            end if;
         when In_Head =>
            Take_Head (R, Input, Next, E);
         when In_Length_Body | In_Close_Body =>
            if Available > 0 then
               declare
                  N : constant Body_Count :=
                    (if R.Phase = In_Length_Body then Body_Count'Min (Body_Count (Available), R.Remaining)
                     else Body_Count (Available));
               begin
                  if N > R.Ceiling - R.Delivered then
                     Refuse (R, Body_Too_Large, E);
                     return;
                  end if;
                  E := (Kind => Body_Run, First => Next, Last => Next + Natural (N) - 1, Why => No_Refusal);
                  Next := Next + Natural (N);
                  R.Delivered := R.Delivered + N;
                  if R.Phase = In_Length_Body then
                     R.Remaining := R.Remaining - N;
                     if R.Remaining = 0 then
                        R.Phase := Done_Phase;
                     end if;
                  end if;
               end;
            end if;
         when In_Chunk_Size .. In_Trailer_LF =>
            Chunk_Step (R, Input, Next, E);
      end case;
   end Step;

   procedure Input_Ended (R : in out Reader; E : out Event) is
   begin
      case R.Phase is
         when Failed_Phase =>
            E := (Kind => Refused, Why => R.Why, others => <>);
         when Done_Phase =>
            E := (Kind => Complete, others => <>);
         when In_Close_Body =>
            R.Phase := Done_Phase;
            E := (Kind => Complete, others => <>);
         when others =>
            Refuse (R, Truncated, E);
      end case;
   end Input_Ended;
end Http_Responses;
