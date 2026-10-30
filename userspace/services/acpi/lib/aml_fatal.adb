package body AML_Fatal with SPARK_Mode is
   use type AML_Decode.Byte;
   use type Interfaces.Unsigned_32;

   function Read_Fixed (Data : AML_Decode.Bytes) return Framing_Result is
      Code : Fatal_Code := 0;
   begin
      if Data'Length < Opcode_Bytes then
         return (Status => Truncated);
      end if;
      if Data (Data'First) /= Extended_Opcode
        or else Data (Data'First + 1) /= Fatal_Opcode
      then
         return (Status => Invalid_Opcode);
      end if;
      if Data'Length < Fixed_Bytes then
         return (Status => Truncated);
      end if;
      for I in Code_Byte_Index loop
         Code := Code or Interfaces.Shift_Left
           (Fatal_Code (Data (Data'First + Opcode_Bytes + Type_Bytes + I)),
            Bits_Per_Byte * I);
      end loop;
      return (Status => Accepted,
              Item => (Kind => Data (Data'First + Opcode_Bytes), Code => Code));
   end Read_Fixed;

   function ACPICA_Host_Argument (Item : Request) return Host_Argument is
     (Interfaces.Unsigned_32 (Item.Evaluated_Argument mod Host_Modulus));

end AML_Fatal;
