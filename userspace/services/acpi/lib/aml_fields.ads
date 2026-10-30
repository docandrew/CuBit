pragma Ada_2022;
with AML_Decode;
with AML_Names;
--  FieldList entry framing only. Access attributes and connection resources
--  are retained for later semantic validation; parsing never authorizes I/O.
package AML_Fields with SPARK_Mode, Pure is
   use type AML_Decode.Status;
   use type AML_Decode.Byte;
   type Entry_Kind is
     (Named_Field, Reserved_Field, Access_Field, Extended_Access_Field,
      Name_Connection, Buffer_Connection);
   type Entry_Result (Status : AML_Decode.Status := AML_Decode.Truncated) is record
      case Status is
         when AML_Decode.Accepted =>
            Kind : Entry_Kind;
            Consumed : Positive;
            Name : AML_Names.Segment := "____";
            Bits : AML_Decode.Field_Bit_Length := 0;
            Access_Type, Attribute, Access_Length : AML_Decode.Byte := 0;
            -- Connection operand occupies bytes 2 .. Consumed relative to
            -- the supplied slice. BufferSize remains an unevaluated TermArg.
         when others => null;
      end case;
   end record;
   function Read_Entry (Data : AML_Decode.Bytes) return Entry_Result
     with Post =>
       (if Read_Entry'Result.Status = AML_Decode.Accepted then
          Read_Entry'Result.Consumed <= Data'Length and then
          (if Read_Entry'Result.Kind = Named_Field then
             Read_Entry'Result.Consumed >= 5 and then
             AML_Names.Valid (Read_Entry'Result.Name) and then
             (for all I in 1 .. 4 => Character'Pos (Read_Entry'Result.Name (I)) =
                Natural (Data (Data'First + (I - 1))))) and then
          (if Read_Entry'Result.Kind in Access_Field | Extended_Access_Field then
             Read_Entry'Result.Consumed =
               (if Read_Entry'Result.Kind = Access_Field then 3 else 4)
             and then Read_Entry'Result.Access_Type = Data (Data'First + 1)
             and then Read_Entry'Result.Attribute = Data (Data'First + 2)
             and then Read_Entry'Result.Access_Length =
               (if Read_Entry'Result.Kind = Access_Field then 0 else Data (Data'First + 3))) and then
          (if Read_Entry'Result.Kind in Name_Connection | Buffer_Connection
           then Read_Entry'Result.Consumed >= 2));
end AML_Fields;
