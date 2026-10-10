pragma Ada_2022;
with Interfaces;

--  Pure AML framing only. The caller supplies an immutable bounded slice,
--  starting at the operand, within its enclosing package/table.
package AML_Decode with SPARK_Mode, Pure is
   subtype Byte is Interfaces.Unsigned_8;
   Increment_Op : constant Byte := 16#75#;
   Decrement_Op : constant Byte := 16#76#;
   Extended_Op : constant Byte := 16#5B#;
   Revision_Extension : constant Byte := 16#30#;
   Revision_Bytes : constant Positive := 2;
   Debug_Extension : constant Byte := 16#31#;
   Debug_Target_Bytes : constant Positive := 2;
   type Integer_Origin is (Ordinary_Integer, AML_Constant);
   function Literal_Origin (Opcode : Byte) return Integer_Origin is
     (if Opcode in 0 | 1 | 16#FF# then AML_Constant else Ordinary_Integer);
   subtype Integer_Value is Interfaces.Unsigned_64;
   -- CuBit AML interpreter policy version; distinct from _REV/table revision.
   Interpreter_Revision : constant Integer_Value := 1;
   type Bytes is array (Positive range <>) of Byte;
   type Status is (Accepted, Truncated, Unsupported, Malformed, Limit_Exceeded);
   type Integer_Width is (Bits_32, Bits_64);
   type Integer_Result (Kind : Status := Truncated) is record
      case Kind is
         when Accepted =>
            Value : Integer_Value;
            Consumed : Positive range 1 .. 9;
         when others => null;
      end case;
   end record;
   Max_String_Length : constant := 255;
   subtype String_Storage is String (1 .. Max_String_Length);
   type String_Result (Kind : Status := Truncated) is record
      case Kind is
         when Accepted =>
            Text : String_Storage;
            Length : Natural range 0 .. Max_String_Length;
            Consumed : Positive range 2 .. Max_String_Length + 2;
         when others => null;
      end case;
   end record;
   function Read_String (Data : Bytes) return String_Result
     with Post =>
       (if Read_String'Result.Kind = Accepted then
          Read_String'Result.Consumed <= Data'Length and then
          Read_String'Result.Consumed = Read_String'Result.Length + 2 and then
          (for all I in 1 .. Read_String'Result.Length =>
             Read_String'Result.Text (I) = Character'Val (Data (Data'First + I))
             and then Character'Pos (Read_String'Result.Text (I)) in 1 .. 127));
   type Package_Result (Kind : Status := Truncated) is record
      case Kind is
         when Accepted =>
            Encoding_Bytes : Positive range 1 .. 4;
            --  Includes PkgLength bytes, excludes the preceding opcode.
            Extent : Positive;
         when others => null;
      end case;
   end record;
   subtype Field_Bit_Length is Natural range 0 .. 16#0FFF_FFFF#;
   type Field_Length_Result (Kind : Status := Truncated) is record
      case Kind is
         when Accepted =>
            Encoding_Bytes : Positive range 1 .. 4;
            Bits : Field_Bit_Length;
         when others => null;
      end case;
   end record;
   -- PkgLength encoding interpreted as a bit count, not a byte extent. Zero
   -- and nonminimal encodings are valid. No region access/allocation occurs.
   function Read_Field_Length (Data : Bytes) return Field_Length_Result with
     Post => Read_Field_Length'Result.Kind in Accepted | Truncated | Malformed
       and then (if Read_Field_Length'Result.Kind = Accepted then
         Read_Field_Length'Result.Encoding_Bytes <= Data'Length
         and then Read_Field_Length'Result.Encoding_Bytes = Natural (Data (Data'First)) / 64 + 1
         and then Read_Field_Length'Result.Bits =
           (if Read_Field_Length'Result.Encoding_Bytes = 1
            then Natural (Data (Data'First)) mod 64
            else Natural (Data (Data'First)) mod 16
              + Natural (Data (Data'First + 1)) * 16
              + (if Read_Field_Length'Result.Encoding_Bytes >= 3
                 then Natural (Data (Data'First + 2)) * 4096 else 0)
              + (if Read_Field_Length'Result.Encoding_Bytes = 4
                 then Natural (Data (Data'First + 3)) * 1048576 else 0)));

   --  Zero/One/Ones and Byte/Word/DWord/QWord constants only. Width is
   --  selected from the admitted DSDT revision, never the SSDT revision.
   function Read_Integer
     (Data : Bytes; Width : Integer_Width) return Integer_Result
     with Post =>
       (if Read_Integer'Result.Kind = Accepted then
          Read_Integer'Result.Consumed <= Data'Length);

   --  For package-bearing opcodes only: NOT a Field bit-length decoder.
   --  Empty bodies are legal; opcode-specific body requirements are separate.
   function Read_Package (Data : Bytes) return Package_Result
     with Post =>
       (if Read_Package'Result.Kind = Accepted then
          Read_Package'Result.Encoding_Bytes <= Read_Package'Result.Extent
          and then Read_Package'Result.Extent <= Data'Length);
   Max_Buffer_Length : constant := 1024;
   subtype Buffer_Storage is Bytes (1 .. Max_Buffer_Length);
   type Buffer_Result (Kind : Status := Truncated) is record
      case Kind is
         when Accepted =>
            Content : Buffer_Storage;
            Length : Natural range 0 .. Max_Buffer_Length;
            Consumed : Positive;
         when others => null;
      end case;
   end record;
   type Buffer_Count_Layout (Kind : Status := Truncated) is record
      case Kind is
         when Accepted =>
            Raw_Offset : Natural;
            Raw_Length : Natural;
            Consumed : Positive;
         when others => null;
      end case;
   end record;
   -- Structural span validation only: no integer conversion, quota or AML evaluation.
   function Check_Buffer_Count_Span
     (Data : Bytes; Count_Consumed : Natural) return Buffer_Count_Layout
     with Post => (if Check_Buffer_Count_Span'Result.Kind = Accepted then
       Check_Buffer_Count_Span'Result.Consumed <= Data'Length
       and then Check_Buffer_Count_Span'Result.Raw_Offset <= Check_Buffer_Count_Span'Result.Consumed
       and then Check_Buffer_Count_Span'Result.Raw_Length =
         Check_Buffer_Count_Span'Result.Consumed - Check_Buffer_Count_Span'Result.Raw_Offset);
   function Read_Buffer_With_Count
     (Data : Bytes; Width : Integer_Width; Count_Value : Integer_Value;
      Count_Consumed : Natural) return Buffer_Result
     with Post => (if Read_Buffer_With_Count'Result.Kind = Accepted then
       Read_Buffer_With_Count'Result.Consumed <= Data'Length);
   --  BufferOp with an integer-constant BufferSize. General TermArg evaluation
   --  belongs to the interpreter and is explicitly unsupported here.
   function Read_Buffer (Data : Bytes; Width : Integer_Width) return Buffer_Result
     with Post => (if Read_Buffer'Result.Kind = Accepted then
                     Read_Buffer'Result.Consumed <= Data'Length);
end AML_Decode;
