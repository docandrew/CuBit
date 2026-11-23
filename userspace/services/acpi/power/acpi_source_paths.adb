pragma Ada_2022;
package body ACPI_Source_Paths with SPARK_Mode is
   subtype Byte_Index is Natural range 0..Maximum_Characters+6;
   function Same(Left,Right:Path) return Boolean is
     (Left.State=Known and then Right.State=Known and then Left.Length=Right.Length
      and then Left.Text(1..Left.Length)=Right.Text(1..Right.Length));
   function Byte(Value:Path;Index:Byte_Index) return Interfaces.Unsigned_8 is
   begin
      if Index>=Value.Length or else Index+1 not in Value.Text'Range then return 0;end if;
      return Interfaces.Unsigned_8(Character'Pos(Value.Text(Index+1)));
   end Byte;
   function Chunk(Value:Path;Offset:Interfaces.Unsigned_64) return Interfaces.Unsigned_64 is
   begin
      if Value.State/=Known or else Offset>Interfaces.Unsigned_64(Value.Length) then return 0;end if;
      declare
         Start:constant Path_Length:=Natural(Offset);
      begin
         return Interfaces.Unsigned_64(Byte(Value,Start))
           +Interfaces.Unsigned_64(Byte(Value,Start+1))*256
           +Interfaces.Unsigned_64(Byte(Value,Start+2))*65536
           +Interfaces.Unsigned_64(Byte(Value,Start+3))*16777216
           +Interfaces.Unsigned_64(Byte(Value,Start+4))*4294967296
           +Interfaces.Unsigned_64(Byte(Value,Start+5))*1099511627776
           +Interfaces.Unsigned_64(Byte(Value,Start+6))*281474976710656;
      end;
   end Chunk;
end ACPI_Source_Paths;
