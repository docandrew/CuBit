pragma Ada_2022;
with Interfaces; use Interfaces;
package body Osi_Test_Support is
   use type Bytes;
   use type AML_Interface_Policy.Add_Status;
   function Enc (Text : String) return Bytes is
      Result : Bytes (1 .. Text'Length);
   begin
      for I in Result'Range loop Result (I) := Character'Pos (Text (Text'First + (I - 1))); end loop;
      return Result;
   end Enc;
   function Configured return AML_Interface_Policy.Policy is
      Result : AML_Interface_Policy.Policy := AML_Interface_Policy.Empty;
      Status : AML_Interface_Policy.Add_Status;
   begin
      AML_Interface_Policy.Add (Result, Enc ("CuBitProbeSupported"), Status);
      if Status /= AML_Interface_Policy.Added then raise Program_Error; end if;
      return Result;
   end Configured;
   function Length_Code (Value : Natural) return Bytes is
   begin
      if Value < 64 then return [Unsigned_8 (Value)]; end if;
      if Value < 4096 then return [Unsigned_8 (16#40# + Value mod 16), Unsigned_8 (Value / 16)]; end if;
      return [Unsigned_8 (16#80# + Value mod 16), Unsigned_8 ((Value / 16) mod 256), Unsigned_8 (Value / 4096)];
   end Length_Code;
   function Block (Opcode, Payload : Bytes) return Bytes is
      Width : Positive := 1;
   begin
      loop
         declare L : constant Bytes := Length_Code (Payload'Length + Width); begin
            if L'Length = Width then return Opcode & L & Payload; end if;
            Width := L'Length;
         end;
      end loop;
   end Block;
   function Method (Name : String; Code : Bytes) return Bytes is
     (Block ([16#14#], Enc (Name) & [0] & Code));
   function String_Operand (Text : String) return Bytes is ([16#0D#] & Enc (Text) & [0]);
   function Table (Payload : Bytes) return Bytes is
      Data : Bytes (1 .. Firmware_Tables.Table_Header_Size + Payload'Length) := [others => 0];
      Sum : Unsigned_8 := 0;
   begin
      Data (1 .. 4) := Enc ("DSDT"); Data (9) := 2;
      for I in 1 .. 4 loop Data (4 + I) := Unsigned_8 ((Data'Length / 256 ** (I - 1)) mod 256); end loop;
      Data (11 .. 16) := Enc ("CUBIT "); Data (17 .. 24) := Enc ("OSIOWNER");
      Data (37 .. Data'Last) := Payload;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum; return Data;
   end Table;
end Osi_Test_Support;
