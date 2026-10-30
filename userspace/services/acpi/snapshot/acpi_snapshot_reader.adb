pragma Ada_2022;
package body ACPI_Snapshot_Reader with SPARK_Mode is
   use type F.Admission;
   use type Byte_Count;
   procedure Read_Table
     (Item : S.Public_Descriptor; Deadline : Tick; Data : out F.Bytes;
      Result : out Report) is
      Now, Previous : Tick := 0;
      Seen, Available, Accepted : Boolean := False;
      Offset : Natural := 0;
      Header : F.Table_Result;
      procedure Check_Time (OK : out Boolean) is
      begin
         OK := False;
         Clock (Now, Available);
         if not Available then Result.Status := Clock_Unavailable; return; end if;
         if Seen and then Now < Previous then Result.Status := Clock_Reversed; return; end if;
         Previous := Now; Seen := True;
         if Now >= Deadline then Result.Status := Deadline_Expired; return; end if;
         OK := True;
      end Check_Time;
      procedure Attempt is
      begin
         if Data'Length /= Item.Extent then return; end if;
         if Item.Name = "FACS" then Result.Status := Invalid_Descriptor; return; end if;
         while Offset < Item.Extent loop
            Result.Offset := Offset;
            Result.Wanted := Natural'Min (Chunk_Max, Item.Extent - Offset);
            Result.Returned := 0;
            Result.Source_Status := Copy_OK;
            Result.Copy_Attempted := False;
            Check_Time (Accepted); if not Accepted then return; end if;
            declare
               First : constant Positive := Data'First + Offset;
               Last : constant Positive := First + (Result.Wanted - 1);
            begin
               Result.Requests := Result.Requests + 1;
               Result.Copy_Attempted := True;
               Read_Part (Item.ID, Offset, Data (First .. Last),
                          Result.Returned, Result.Source_Status);
            end;
            if Result.Source_Status /= Copy_OK then Result.Status := Source_Refused; return; end if;
            if Result.Returned > Byte_Count (Result.Wanted) then Result.Status := Excess_Copy; return; end if;
            if Result.Returned /= Byte_Count (Result.Wanted) then Result.Status := Short_Copy; return; end if;
            Offset := Offset + Natural (Result.Returned);
            Result.Total_Copied := Offset;
            Check_Time (Accepted); if not Accepted then return; end if;
         end loop;
         Check_Time (Accepted); if not Accepted then return; end if;
         Header := F.Read_Table (Data, Item.Name);
         Result.Header_Status := Header.Status;
         if Header.Status /= F.Accepted then Result.Status := Table_Refused; return; end if;
         if Header.Extent /= Item.Extent or else Header.Revision /= Item.Revision then
            Result.Status := Metadata_Mismatch; return;
         end if;
         Check_Time (Accepted); if not Accepted then return; end if;
         Result.Status := Copied_Table;
      end Attempt;
   begin
      Result := (others => <>);
      Data := [others => 0];
      Attempt;
      if Result.Status /= Copied_Table then Data := [others => 0]; end if;
   end Read_Table;
end ACPI_Snapshot_Reader;
