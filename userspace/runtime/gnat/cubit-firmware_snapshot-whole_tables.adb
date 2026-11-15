pragma Ada_2022;

package body CuBit.Firmware_Snapshot.Whole_Tables is

   procedure Find
     (Tables : Authority; Signature : String;
      Table : out Table_ID; Bytes : out Table_Bytes; Status : out Read_Status)
   is
      Catalog : Catalog_Info;
      D : Table_Description;
      Result : Outcome;
   begin
      Table := 0;
      Bytes := 0;
      Status := Read_Failed;
      Info (Tables, Catalog, Result);
      if Result /= Ok or else not Catalog.Ready then
         return;
      end if;
      Status := Absent;
      for T in 1 .. Table_ID (Catalog.Table_Count) loop
         Describe (Tables, T, D, Result);
         if Result = Ok
           and then (for all I in Signature_Index =>
                       D.Signature (I) = Character'Pos (Signature (Signature'First + I - 1)))
         then
            if D.Extent > Byte_Count (Maximum_Table_Bytes) then
               Status := Too_Large;
            else
               Table := T;
               Bytes := Table_Bytes (D.Extent);
               Status := Found;
            end if;
            return;
         end if;
      end loop;
   end Find;

   procedure Copy
     (Tables : Authority; Table : Table_ID; Into : out Byte_Array; Status : out Read_Status)
   is
      Done : Natural := 0;
   begin
      Into := [others => 0];
      Status := Read_Failed;
      while Done < Into'Length loop
         declare
            Wanted : constant Positive := Natural'Min (Into'Length - Done, Chunk_Max);
            Chunk : Firmware_Snapshot.Byte_Array (1 .. Wanted);
            Copied : Byte_Count;
            Result : Outcome;
         begin
            Copy_Part (Tables, Table, Byte_Count (Done), Chunk, Copied, Result);
            if Result /= Ok or else Copied /= Byte_Count (Wanted) then
               Into := [others => 0];
               return;
            end if;
            for I in 1 .. Wanted loop
               Into (Done + I) := Chunk (I);
            end loop;
            Done := Done + Wanted;
         end;
      end loop;
      Status := Found;
   end Copy;

end CuBit.Firmware_Snapshot.Whole_Tables;
