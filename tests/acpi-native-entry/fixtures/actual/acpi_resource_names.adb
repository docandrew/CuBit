with AML_Decode;
package body ACPI_Resource_Names with SPARK_Mode is
   use type Firmware_Tables.Byte;
   Prefix_Limit : constant Positive := 255;
   Segment_Limit : constant Positive := AML_Names.Segment_Array'Last;
   Segment_Bytes : constant Positive := AML_Names.Segment'Length;
   subtype Prefix_Count is Natural range 0 .. Prefix_Limit;
   subtype Segment_Length is Natural range 0 .. Segment_Bytes;
   Maximum_Source_Bytes : constant Positive := Prefix_Limit +
     Segment_Limit * Segment_Bytes + (Segment_Limit - 1) + 1;
   Root : constant Firmware_Tables.Byte := Character'Pos ('\');
   Parent : constant Firmware_Tables.Byte := Character'Pos ('^');
   Separator : constant Firmware_Tables.Byte := Character'Pos ('.');
   Case_Distance : constant Firmware_Tables.Byte := Character'Pos ('a') - Character'Pos ('A');
   function Parse (Source : Firmware_Tables.Bytes) return AML_Names.Name_Result is
      Position : Natural := 0;
      Prefixes : Prefix_Count := 0;
      Value : AML_Names.Name_Result (AML_Names.Accepted) :=
        (AML_Names.Accepted, False, 0, 0, [others => "____"], 1);
      function Byte_At return Firmware_Tables.Byte is (Source (Source'First + Position))
         with Pre => Position < Source'Length;
      function Upper (Byte : Firmware_Tables.Byte) return AML_Decode.Byte is
        (AML_Decode.Byte (if Byte in Character'Pos ('a') .. Character'Pos ('z') then Byte - Case_Distance else Byte));
   begin
      if Source'Length = 0 then return (Kind => AML_Names.Truncated); end if;
      if Source'Length > Maximum_Source_Bytes then return (Kind => AML_Names.Limit_Exceeded); end if;
      if Source (Source'Last) /= 0 then return (Kind => AML_Names.Truncated); end if;
      if Source'Length = 1 then return (Kind => AML_Names.Malformed); end if;
      if Byte_At = Root then
         Value.Rooted := True;
         while Position < Source'Length - 1 and then Byte_At = Root loop
            pragma Loop_Variant (Increases => Position);
            if Prefixes = Prefix_Limit then return (Kind => AML_Names.Limit_Exceeded); end if;
            Prefixes := Prefixes + 1; Position := Position + 1;
         end loop;
      else
         while Position < Source'Length - 1 and then Byte_At = Parent loop
            pragma Loop_Variant (Increases => Position);
            if Value.Parents = Prefix_Limit then return (Kind => AML_Names.Limit_Exceeded); end if;
            Value.Parents := Value.Parents + 1; Position := Position + 1;
         end loop;
      end if;
      while Position < Source'Length - 1 loop
         pragma Loop_Invariant
           (for all I in Value.Parts'Range => AML_Names.Valid (Value.Parts (I)));
         pragma Loop_Variant (Increases => Position);
         if Value.Count = Segment_Limit then return (Kind => AML_Names.Limit_Exceeded); end if;
         Value.Count := Value.Count + 1;
         declare
            Start : constant Natural := Position;
            Length : Segment_Length := 0;
            Byte : AML_Decode.Byte;
         begin
            while Position < Source'Length - 1 and then Byte_At /= Separator loop
               pragma Loop_Invariant (Position = Start + Length);
               pragma Loop_Invariant
                 (for all I in Value.Parts'Range => AML_Names.Valid (Value.Parts (I)));
               pragma Loop_Variant (Increases => Position);
               if Length = Segment_Bytes then return (Kind => AML_Names.Malformed); end if;
               Byte := Upper (Byte_At);
               if not (if Length = 0 then AML_Names.Lead (Byte) else AML_Names.Tail (Byte)) then
                  return (Kind => AML_Names.Malformed);
               end if;
               Length := Length + 1;
               Value.Parts (Value.Count) (Length) := Character'Val (Byte);
               Position := Position + 1;
            end loop;
            if Length = 0 then return (Kind => AML_Names.Malformed); end if;
         end;
         if Position < Source'Length - 1 then
            Position := Position + 1;
            if Position = Source'Length - 1 then return (Kind => AML_Names.Malformed); end if;
         end if;
      end loop;
      Value.Consumed := Source'Length;
      return Value;
   end Parse;
end ACPI_Resource_Names;
