pragma Ada_2022;
package body CuBit.Metric_Records with SPARK_Mode is
   Kind_Word : constant Slot_Word_Index := 0;
   Key_Word : constant Slot_Word_Index := 1;
   Declared_Word : constant Slot_Word_Index := 2;
   Unit_Word : constant Slot_Word_Index := 3;
   First_Name_Word : constant Slot_Word_Index := 4;
   Time_Word : constant Slot_Word_Index := 2;
   Value_Word : constant Slot_Word_Index := 3;
   Correlation_Word : constant Slot_Word_Index := 4;
   First_Reserved_Value_Word : constant Slot_Word_Index := 5;

   Magic_Word : constant Slot_Word_Index := 0;
   Count_Word : constant Slot_Word_Index := 1;
   Sequence_Word : constant Slot_Word_Index := 2;
   Dropped_Word : constant Slot_Word_Index := 3;
   Clock_Word : constant Slot_Word_Index := 4;
   First_Reserved_Header_Word : constant Slot_Word_Index := 5;

   Bits_Per_Byte : constant := 8;
   Byte_Mask : constant Unsigned_64 := 16#FF#;

   subtype Name_Word_Index is Slot_Word_Index
     range First_Name_Word .. First_Name_Word + Name_Words - 1;
   subtype Byte_In_Word is Natural range 0 .. Bytes_Per_Word - 1;

   function Name_Word_Of (I : Name_Index) return Name_Word_Index is
     (First_Name_Word + (I - 1) / Bytes_Per_Word);
   function Byte_Of (I : Name_Index) return Byte_In_Word is
     ((I - 1) mod Bytes_Per_Word);

   function To_Name (Text : String) return Metric_Name is
      Result : Metric_Name;
   begin
      if Text'Length = 0 or else Text'Length > Maximum_Name_Bytes then
         return (others => <>);
      end if;
      for I in Text'Range loop
         pragma Loop_Invariant (Result.Length = 0);
         if not Name_Byte_Allowed (Character'Pos (Text (I))) then
            return (others => <>);
         end if;
         Result.Bytes (I - Text'First + 1) := Character'Pos (Text (I));
      end loop;
      Result.Length := Text'Length;
      if Valid_Name (Result) then
         return Result;
      end if;
      return (others => <>);
   end To_Name;

   function Encode (Item : Metric_Record) return Slot_Words is
      Words : Slot_Words := [others => 0];
   begin
      Words (Kind_Word) := Record_Kind'Enum_Rep (Item.Kind);
      Words (Key_Word) := Unsigned_64 (Item.Key);
      case Item.Kind is
         when Describe =>
            Words (Declared_Word) := Record_Kind'Enum_Rep (Item.Declared);
            Words (Unit_Word) := Unit'Enum_Rep (Item.Measure);
            for I in Name_Index loop
               Words (Name_Word_Of (I)) := Words (Name_Word_Of (I)) or
                 Shift_Left (Unsigned_64 (Item.Name.Bytes (I)),
                             Bits_Per_Byte * Byte_Of (I));
            end loop;
         when Counter .. Latency =>
            Words (Time_Word) := Item.Time_Us;
            Words (Value_Word) := Item.Value;
            Words (Correlation_Word) := Item.Correlation;
         when Trace =>
            Words (2) := Item.Trace_ID;
            Words (3) := Unsigned_64 (Item.Part);
            for I in Trace_Part loop
               Words (4 + I) := Item.Data (I);
            end loop;
         when Span =>
            Words (Time_Word) := Item.Start_Us;
            Words (Value_Word) := Item.End_Us;
            Words (Correlation_Word) := Item.Span_Correlation;
      end case;
      return Words;
   end Encode;

   function Decode (Words : Slot_Words) return Decoded_Record is
      Found : Boolean := False;
      Kind : Record_Kind := Counter;
      Declared : Metric_Kind := Counter;
      Measure : Unit := Count;
      Name : Metric_Name;
   begin
      for Candidate in Record_Kind loop
         if Words (Kind_Word) = Record_Kind'Enum_Rep (Candidate) then
            Kind := Candidate;
            Found := True;
         end if;
      end loop;
      if not Found then
         return (Success => False, Reason => Unknown_Kind);
      end if;
      if Words (Key_Word) not in 1 .. Maximum_Keys then
         return (Success => False, Reason => Invalid_Key);
      end if;
      declare
         Key : constant Metric_Key := Metric_Key (Words (Key_Word));
      begin
         if Kind = Describe then
            Found := False;
            for Candidate in Metric_Kind loop
               if Words (Declared_Word) = Record_Kind'Enum_Rep (Candidate)
               then
                  Declared := Candidate;
                  Found := True;
               end if;
            end loop;
            if not Found then
               return (Success => False, Reason => Invalid_Declaration);
            end if;
            Found := False;
            for Candidate in Unit loop
               if Words (Unit_Word) = Unit'Enum_Rep (Candidate) then
                  Measure := Candidate;
                  Found := True;
               end if;
            end loop;
            if not Found then
               return (Success => False, Reason => Invalid_Declaration);
            end if;
            for I in Name_Index loop
               Name.Bytes (I) := Unsigned_8
                 (Shift_Right (Words (Name_Word_Of (I)),
                               Bits_Per_Byte * Byte_Of (I)) and Byte_Mask);
            end loop;
            for I in Name_Index loop
               exit when Name.Bytes (I) = 0;
               Name.Length := I;
            end loop;
            if not Valid_Name (Name) then
               return (Success => False, Reason => Invalid_Name);
            end if;
            return (Success => True,
                    Value => (Kind => Describe, Key => Key,
                              Declared => Declared, Measure => Measure,
                              Name => Name));
         end if;
         if Kind = Trace then
            if Words (2) = 0 or Words (3) > Unsigned_64 (Trace_Part'Last) then
               return (Success => False, Reason => Invalid_Trace);
            end if;
            return (Success => True,
                    Value => (Trace, Key, Words (2), Trace_Part (Words (3)),
                              [for I in Trace_Part => Words (4 + I)]));
         end if;
         for W in First_Reserved_Value_Word .. Slot_Word_Index'Last loop
            if Words (W) /= 0 then
               return (Success => False, Reason => Nonzero_Reserved);
            end if;
         end loop;
         case Kind is
            when Describe | Trace =>
               return (Success => False, Reason => Unknown_Kind);
            when Counter =>
               return (Success => True,
                       Value => (Kind => Counter, Key => Key,
                                 Time_Us => Words (Time_Word),
                                 Value => Words (Value_Word),
                                 Correlation => Words (Correlation_Word)));
            when Gauge =>
               return (Success => True,
                       Value => (Kind => Gauge, Key => Key,
                                 Time_Us => Words (Time_Word),
                                 Value => Words (Value_Word),
                                 Correlation => Words (Correlation_Word)));
            when Latency =>
               return (Success => True,
                       Value => (Kind => Latency, Key => Key,
                                 Time_Us => Words (Time_Word),
                                 Value => Words (Value_Word),
                                 Correlation => Words (Correlation_Word)));
            when Span =>
               if Words (Time_Word) > Words (Value_Word) then
                  return (Success => False, Reason => Reversed_Span);
               end if;
               return (Success => True,
                       Value => (Kind => Span, Key => Key,
                                 Start_Us => Words (Time_Word),
                                 End_Us => Words (Value_Word),
                                 Span_Correlation =>
                                   Words (Correlation_Word)));
         end case;
      end;
   end Decode;

   function Encode_Header (Item : Batch_Header) return Slot_Words is
     ([Magic_Word => Header_Magic,
       Count_Word => Unsigned_64 (Item.Records),
       Sequence_Word => Item.Sequence,
       Dropped_Word => Item.Producer_Dropped,
       Clock_Word => Clock_Domain'Enum_Rep (Item.Clock),
       others => 0]);

   function Decode_Header
     (Words : Slot_Words; Bytes : Unsigned_64) return Decoded_Header is
   begin
      if Words (Magic_Word) /= Header_Magic then
         return (Success => False, Reason => Bad_Magic);
      elsif Words (Count_Word) not in 1 .. Maximum_Records then
         return (Success => False, Reason => Bad_Count);
      elsif Words (Sequence_Word) not in Batch_Sequence then
         return (Success => False, Reason => Bad_Sequence);
      elsif Words (Clock_Word) /=
        Clock_Domain'Enum_Rep (Monotonic_Microseconds)
      then
         return (Success => False, Reason => Bad_Clock);
      end if;
      for W in First_Reserved_Header_Word .. Slot_Word_Index'Last loop
         if Words (W) /= 0 then
            return (Success => False, Reason => Header_Reserved);
         end if;
      end loop;
      if Bytes /= Batch_Bytes (Batch_Record_Count (Words (Count_Word))) then
         return (Success => False, Reason => Length_Mismatch);
      end if;
      return (Success => True,
              Value => (Records => Batch_Record_Count (Words (Count_Word)),
                        Sequence => Words (Sequence_Word),
                        Producer_Dropped => Words (Dropped_Word),
                        Clock => Monotonic_Microseconds));
   end Decode_Header;

   procedure Put_Slot
     (Page : in out Page_Words; Index : Record_Count; Words : Slot_Words) is
   begin
      for W in Slot_Word_Index loop
         Page (Index * Words_Per_Slot + W) := Words (W);
         pragma Loop_Invariant
           (for all V in Slot_Word_Index'First .. W =>
              Page (Index * Words_Per_Slot + V) = Words (V));
         pragma Loop_Invariant
           (for all I in Page_Word_Index =>
              (if I / Words_Per_Slot /= Index then
                 Page (I) = Page'Loop_Entry (I)));
      end loop;
   end Put_Slot;
end CuBit.Metric_Records;
