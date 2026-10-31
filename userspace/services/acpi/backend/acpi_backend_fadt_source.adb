pragma Ada_2022;
package body ACPI_Backend_FADT_Source is
   use type Count, R.Tick, R.Copy_Status, R.Outcome;
   FADT_Signature : constant F.Signature := "FACP";
   function Observe (Run : Context) return Report is (Run.Info);
   function Sample (Run : in out Context) return Boolean is
      Now : R.Tick;
      Available : Boolean;
   begin
      if Run.Deadline = R.Tick'Last then Run.Info.Status := Deadline_Expired; return False; end if;
      Clock (Now, Available);
      if not Available then Run.Info.Status := Clock_Unavailable;
      elsif Run.Seen_Clock and then Now < Run.Last then Run.Info.Status := Clock_Reversed;
      elsif Now >= Run.Deadline then Run.Info.Status := Deadline_Expired;
      else Run.Last := Now; Run.Seen_Clock := True; return True; end if;
      return False;
   end Sample;
   package body Using_Source is
      procedure Inspect
        (Run : in out Context; Deadline : R.Tick; Budget : Natural; Result : out Report)
      is
         Catalog : Inventory;
         Descriptor : Description;
         Sum, Largest : Count := 0;
         procedure Fail (Why : Outcome) is
         begin Run.Stage := Finished; Run.Info.Status := Why; Result := Run.Info; end Fail;
      begin
         Result := (Run.Info with delta Status => Already_Consumed);
         if Run.Stage /= Fresh then return; end if;
         Run.Stage := Inspecting; Run.Deadline := Deadline;
         if not Sample (Run) then Fail (Run.Info.Status); return; end if;
         Info (Catalog, Run.Info.Source_Status);
         if not Sample (Run) then Fail (Run.Info.Status); return; end if;
         if Run.Info.Source_Status /= R.Copy_OK then Fail (Source_Refused); return; end if;
         if not Catalog.Ready then Fail (Inventory_Not_Ready); return; end if;
         if Catalog.Tables > Count (Positive'Last) then Fail (Representation_Limit); return; end if;
         if Catalog.Tables > Count (Budget) then Fail (Inspection_Budget_Exceeded); return; end if;
         if Catalog.Largest_Table > Catalog.Total_Bytes
           or else (Catalog.Tables = 0 and then (Catalog.Total_Bytes /= 0 or Catalog.Largest_Table /= 0))
         then Fail (Invalid_Inventory); return; end if;
         for ID in 1 .. Natural (Catalog.Tables) loop
            if not Sample (Run) then Fail (Run.Info.Status); return; end if;
            Describe (ID, Descriptor, Run.Info.Source_Status);
            Run.Info.Descriptions := Run.Info.Descriptions + 1;
            if not Sample (Run) then Fail (Run.Info.Status); return; end if;
            if Run.Info.Source_Status /= R.Copy_OK then Fail (Source_Refused); return; end if;
            if Descriptor.Extent < Count (F.Table_Header_Size) or else Descriptor.Name = "FACS" then
               Fail (Invalid_Description); return;
            end if;
            if Descriptor.Extent > Catalog.Largest_Table or else Descriptor.Extent > Catalog.Total_Bytes - Sum then
               Fail (Inventory_Mismatch); return;
            end if;
            Sum := Sum + Descriptor.Extent; Largest := Count'Max (Largest, Descriptor.Extent);
            if Descriptor.Name = FADT_Signature then
               if Run.Info.Selected then Fail (Duplicate_FADT); return; end if;
               Run.Info.Selected := True; Run.Info.ID := ID; Run.Info.Required_Bytes := Descriptor.Extent;
               Run.Selected := Descriptor;
            end if;
         end loop;
         if Sum /= Catalog.Total_Bytes or Largest /= Catalog.Largest_Table then Fail (Inventory_Mismatch); return; end if;
         if not Run.Info.Selected then Fail (Missing_FADT); return; end if;
         if Run.Info.Required_Bytes > Count (Positive'Last) then Fail (Representation_Limit); return; end if;
         Run.Catalog := Catalog; Run.Stage := Awaiting_Data; Run.Info.Status := Metadata_Ready; Result := Run.Info;
      end Inspect;
      procedure Read
        (Run : in out Context; Data : out F.Bytes;
         Item : out ACPI_FADT.Description; Result : out Report)
      is
         Descriptor : Description;
         Catalog : Inventory;
         procedure Fail (Why : Outcome) is
         begin
            Data := [others => 0]; Item := (others => <>);
            Run.Stage := Finished; Run.Info.Status := Why; Result := Run.Info;
         end Fail;
         procedure Tracked_Clock (Now : out R.Tick; Available : out Boolean) is
         begin Available := Sample (Run); Now := Run.Last; end Tracked_Clock;
         procedure Copy_Table is new R.Read_Table (Chunk_Max, Read_Part, Tracked_Clock);
      begin
         Data := [others => 0]; Item := (others => <>);
         Result := (Run.Info with delta Status => (if Run.Stage = Finished then Already_Consumed else Wrong_Order));
         if Run.Stage /= Awaiting_Data then return; end if;
         Run.Stage := Reading;
         if Count (Data'Length) /= Run.Info.Required_Bytes then Fail (Wrong_Capacity); return; end if;
         if not Sample (Run) then Fail (Run.Info.Status); return; end if;
         Describe (Run.Info.ID, Descriptor, Run.Info.Source_Status);
         if not Sample (Run) then Fail (Run.Info.Status); return; end if;
         if Run.Info.Source_Status /= R.Copy_OK then Fail (Source_Refused); return; end if;
         if Descriptor /= Run.Selected then Fail (Inventory_Mismatch); return; end if;
         Copy_Table ((R.S.Table_ID (Run.Info.ID), Run.Selected.Name, Run.Selected.Revision,
                      Positive (Run.Info.Required_Bytes)), Run.Deadline, Data, Run.Info.Copy);
         if Run.Info.Status in Clock_Unavailable | Clock_Reversed | Deadline_Expired then Fail (Run.Info.Status); return; end if;
         if Run.Info.Copy.Status /= R.Copied_Table then Fail (Table_Refused); return; end if;
         if not Sample (Run) then Fail (Run.Info.Status); return; end if;
         Info (Catalog, Run.Info.Source_Status);
         if not Sample (Run) then Fail (Run.Info.Status); return; end if;
         if Run.Info.Source_Status /= R.Copy_OK then Fail (Source_Refused); return; end if;
         if Catalog /= Run.Catalog then Fail (Inventory_Mismatch); return; end if;
         declare Parsed : constant ACPI_FADT.Result := ACPI_FADT.Decode (Data); begin
            if not Parsed.Valid then Fail (FADT_Refused); return; end if;
            Item := Parsed.Value;
         end;
         Run.Stage := Finished; Run.Info.Status := FADT_Ready; Result := Run.Info;
      end Read;
   end Using_Source;
end ACPI_Backend_FADT_Source;
