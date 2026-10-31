pragma Ada_2022;
with Interfaces; use Interfaces;
package body Firmware_Tables.SLIT with SPARK_Mode is
   Bits_Per_Byte : constant := 8;
   function Decode (Data : Bytes) return Table_Metadata is
      Header : constant Table_Result := Read_Table (Data, "SLIT");
      Raw_Count : Unsigned_64 := 0;
      Matrix_Size : Natural;
      Count : Locality_Count;
   begin
      if Header.Status /= Accepted then return (Valid => False); end if;
      if Header.Extent /= Data'Length or else Header.Extent < Fixed_Size then
         return (Valid => False);
      end if;
      for I in 0 .. Locality_Count_Size - 1 loop
         Raw_Count := Raw_Count or Shift_Left
           (Unsigned_64 (Data (Data'First + Table_Header_Size + I)), Bits_Per_Byte * I);
      end loop;
      Matrix_Size := Header.Extent - Fixed_Size;
      if Raw_Count = 0 then
         return (if Matrix_Size = 0 then (True, Header.Revision, 0)
                 else (Valid => False));
      end if;
      -- A nonempty N*N matrix has at least N bytes. This bound also makes
      -- conversion to the host's index type safe, before any division.
      if Raw_Count > Unsigned_64 (Matrix_Size) then return (Valid => False); end if;
      Count := Locality_Count (Raw_Count);
      if Matrix_Size / Count /= Count or else Matrix_Size mod Count /= 0 then
         return (Valid => False);
      end if;
      return (True, Header.Revision, Count);
   end Decode;
   function Read_Distance
     (Data : Bytes; From_Locality, To_Locality : Locality_Index)
      return Distance_Result
   is
      Table : constant Table_Metadata := Decode (Data);
   begin
      if not Table.Valid or else From_Locality >= Table.Count
        or else To_Locality >= Table.Count
      then return (Valid => False); end if;
      declare
         Offset : constant Natural :=
           Fixed_Size + From_Locality * Table.Count + To_Locality;
      begin
         return (True, Data (Data'First + Offset));
      end;
   end Read_Distance;
end Firmware_Tables.SLIT;
