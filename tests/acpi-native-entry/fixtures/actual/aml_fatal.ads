with Interfaces;
with AML_Decode;

-- Pure decoded request data. A request does not assert platform delivery.
package AML_Fatal with SPARK_Mode, Pure is
   Extended_Opcode : constant AML_Decode.Byte := 16#5B#;
   Fatal_Opcode : constant AML_Decode.Byte := 16#32#;
   Opcode_Bytes : constant Positive := 2;
   Type_Bytes : constant Positive := 1;
   Code_Bytes : constant Positive := 4;
   Fixed_Bytes : constant Positive := Opcode_Bytes + Type_Bytes + Code_Bytes;
   use type AML_Decode.Integer_Value;
   subtype Code_Byte_Index is Natural range 0 .. Code_Bytes - 1;
   Bits_Per_Byte : constant Positive := 8;
   Host_Modulus : constant AML_Decode.Integer_Value :=
     AML_Decode.Integer_Value (Interfaces.Unsigned_32'Last) + 1;
   subtype Host_Argument is Interfaces.Unsigned_32;
   subtype Fatal_Type is AML_Decode.Byte;
   subtype Fatal_Code is Interfaces.Unsigned_32;
   type Header is record
      Kind : Fatal_Type;
      Code : Fatal_Code;
   end record;
   type Framing_Status is (Accepted, Truncated, Invalid_Opcode);
   type Framing_Result (Status : Framing_Status := Truncated) is record
      case Status is
         when Accepted => Item : Header;
         when others => null;
      end case;
   end record;
   -- Data must already be restricted to the current lexical window. This reads
   -- only the fixed header; the following TermArg is NOT parsed or evaluated.
   function Read_Fixed (Data : AML_Decode.Bytes) return Framing_Result
     with Global => null,
       Post =>
         (if Data'Length < Opcode_Bytes then
            Read_Fixed'Result.Status = Truncated
          elsif Natural (Data (Data'First)) /= Natural (Extended_Opcode)
            or else Natural (Data (Data'First + 1)) /= Natural (Fatal_Opcode)
          then Read_Fixed'Result.Status = Invalid_Opcode
          elsif Data'Length < Fixed_Bytes then
            Read_Fixed'Result.Status = Truncated
          else Read_Fixed'Result.Status = Accepted)
         and then
         (if Read_Fixed'Result.Status = Accepted then
            Data'Length >= Fixed_Bytes
            and then Natural (Read_Fixed'Result.Item.Kind) =
              Natural (Data (Data'First + Opcode_Bytes))
            and then AML_Decode.Integer_Value (Read_Fixed'Result.Item.Code) =
              AML_Decode.Integer_Value (Data (Data'First + 3))
              + AML_Decode.Integer_Value (Data (Data'First + 4)) * 2 ** 8
              + AML_Decode.Integer_Value (Data (Data'First + 5)) * 2 ** 16
              + AML_Decode.Integer_Value (Data (Data'First + 6)) * 2 ** 24);
   type Request is record
      Literal : Header;
      Evaluated_Argument : AML_Decode.Integer_Value;
      Width : AML_Decode.Integer_Width;
   end record;
   -- ACPICA's OS hook takes UINT32. Preserve the raw evaluated argument in
   -- Request; this named projection is compatibility policy, not AML evaluation.
   function ACPICA_Host_Argument (Item : Request) return Host_Argument
     with Global => null,
       Post => AML_Decode.Integer_Value (ACPICA_Host_Argument'Result) =
         Item.Evaluated_Argument mod Host_Modulus;
   type Observation (Present : Boolean := False) is record
      case Present is
         when True => Item : Request;
         when False => null;
      end case;
   end record;
end AML_Fatal;
