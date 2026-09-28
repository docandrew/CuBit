pragma Ada_2022;
with Interfaces; use Interfaces;
package body Boot_QR_Capsule with SPARK_Mode is
   function CRC16 (Value : String) return Unsigned_16 is
      CRC : Unsigned_16 := 16#FFFF#;
   begin
      for C of Value loop
         CRC := CRC xor Shift_Left (Unsigned_16 (Character'Pos (C)), 8);
         for I in 1 .. 8 loop
            CRC := Shift_Left (CRC, 1) xor
              (if (CRC and 16#8000#) /= 0 then 16#1021# else 0);
         end loop;
      end loop;
      return CRC;
   end CRC16;
   function Hex (Value : Unsigned_16) return String is
      Alphabet : constant String := "0123456789ABCDEF";
      Result : String (1 .. 4);
      Rest : Unsigned_16 := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Alphabet (Natural (Rest and 15) + 1);
         Rest := Shift_Right (Rest, 4);
      end loop;
      return Result;
   end Hex;
   procedure Field (Line : Boot_Panel.Line; Limit : Natural;
                    Buffer : in out String; Used : in out Natural) is
      Count : Natural := 0;
      Last : Natural := 0;
   begin
      for I in Line'Range loop
         if Line (I) /= ' ' then Last := I; end if;
      end loop;
      for I in Line'First .. Last loop
         exit when Count = Limit;
         --  Separators remain unambiguous for a scanner/parser.
         Buffer (Used + 1) :=
           (if Line (I) = ';' or else Line (I) = '=' then '_' else Line (I));
         Used := Used + 1;
         Count := Count + 1;
      end loop;
   end Field;
   procedure Add (Value : String; Buffer : in out String; Used : in out Natural) is
   begin
      for C of Value loop
         Buffer (Used + 1) := C;
         Used := Used + 1;
      end loop;
   end Add;
   function Build (Current, Completed, Detail, Failure : Boot_Panel.Line)
     return String
   is
      Buffer : String (1 .. Maximum_Length);
      Used : Natural := 0;
   begin
      Add ("CB1;C=", Buffer, Used); Field (Current, 12, Buffer, Used);
      Add (";L=", Buffer, Used); Field (Completed, 12, Buffer, Used);
      Add (";D=", Buffer, Used); Field (Detail, 18, Buffer, Used);
      Add (";F=", Buffer, Used); Field (Failure, 12, Buffer, Used);
      Add (";X=", Buffer, Used); Add (Hex (CRC16 (Buffer (1 .. Used))), Buffer, Used);
      return Buffer (1 .. Used);
   end Build;
end Boot_QR_Capsule;
