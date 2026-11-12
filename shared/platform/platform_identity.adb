pragma Ada_2022;

package body Platform_Identity with SPARK_Mode is

   function From_Header (Header : Byte_Array) return Identity is
      Result : Identity := (Source => FADT_Header, others => <>);
   begin
      for I in 1 .. Signature_Bytes loop
         if Header (Signature_Offset + I - 1) /=
              Character'Pos (FADT_Signature (FADT_Signature'First + I - 1))
         then
            return No_Identity;
         end if;
         pragma Loop_Invariant
           (for all J in 1 .. I =>
              Header (Signature_Offset + J - 1) =
                Character'Pos (FADT_Signature (FADT_Signature'First + J - 1)));
      end loop;
      for I in OEM_ID_Text'Range loop
         Result.OEM_ID (I) := Character'Val (Header (OEM_ID_Offset + I - 1));
      end loop;
      for I in OEM_Table_ID_Text'Range loop
         Result.OEM_Table_ID (I) := Character'Val (Header (OEM_Table_ID_Offset + I - 1));
      end loop;
      for I in reverse 1 .. OEM_Revision_Bytes loop
         Result.OEM_Revision :=
           Shift_Left (Result.OEM_Revision, 8) or Unsigned_32 (Header (OEM_Revision_Offset + I - 1));
      end loop;
      return Result;
   end From_Header;

end Platform_Identity;
