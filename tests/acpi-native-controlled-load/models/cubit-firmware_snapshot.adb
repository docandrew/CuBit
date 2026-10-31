with Native_Test_Control;
package body CuBit.Firmware_Snapshot is
   package T renames Native_Test_Control;
   use type Interfaces.Unsigned_64;
   function Test_Authority return Authority is ((Tag => Expected_Tag));
   procedure Info (A : Authority; Result : out Catalog_Info; Status : out Outcome) is
   begin
      if A.Tag /= Expected_Tag then raise Program_Error with "unexpected snapshot authority"; end if;
      T.Infos := T.Infos + 1;
      Result := (others => <>);
      if T.Mode = "info-denied" then Status := Not_Authorized; return; end if;
      if T.Mode = "info-exception" then raise Program_Error with "modeled source exception"; end if;
      Status := Ok;
      if T.Mode = "not-ready" then
         Result := (Version => Firmware_Snapshot_ABI.Interface_Version, others => <>);
         return;
      end if;
      Result := (Version => Firmware_Snapshot_ABI.Interface_Version, Table_Count => 1,
        Ready => True, Largest_Table => Byte_Count (T.Length),
        Total_Bytes => Byte_Count (T.Length));
   end Info;
   procedure Describe
     (A : Authority; ID : Table_ID; Result : out Table_Description; Status : out Outcome) is
   begin
      if A.Tag /= Expected_Tag or else ID /= 1 then raise Program_Error with "descriptor identity"; end if;
      T.Descriptions := T.Descriptions + 1;
      Result := (others => <>);
      if T.Mode = "describe-denied" then Status := Not_Authorized; return; end if;
      Result := (Signature => [Character'Pos ('D'), Character'Pos ('S'), Character'Pos ('D'), Character'Pos ('T')],
        Revision => T.Data (9), Extent => Byte_Count (T.Length));
      Status := Ok;
   end Describe;
   procedure Copy_Part
     (A : Authority; ID : Table_ID; Offset : Byte_Count; Into : out Byte_Array;
      Copied : out Byte_Count; Status : out Outcome) is
      Amount : Natural;
   begin
      if A.Tag /= Expected_Tag or else ID /= 1 or else Offset > Byte_Count (T.Length) then
         raise Program_Error with "copy identity/extent";
      end if;
      T.Copies := T.Copies + 1;
      Into := [others => 0]; Copied := 0;
      if T.Mode = "copy-refused" and then Offset > 0 then Status := Copy_Failed; return; end if;
      Amount := Natural'Min (Into'Length, T.Length - Natural (Offset));
      for I in 1 .. Amount loop Into (Into'First + I - 1) := T.Data (Natural (Offset) + I); end loop;
      Copied := Byte_Count (Amount); Status := Ok;
   end Copy_Part;
end CuBit.Firmware_Snapshot;
