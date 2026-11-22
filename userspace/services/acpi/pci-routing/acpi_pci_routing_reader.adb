pragma Ada_2022;
with Interfaces;
package body ACPI_PCI_Routing_Reader with SPARK_Mode is
   package V renames Core.Values;
   package P renames ACPI_PCI_Routes;
   use type V.Access_Status;
   use type V.Value_Kind;
   use type V.Value_Handle;
   use type Interfaces.Unsigned_64;
   Field_Count : constant Positive := 4;
   subtype Row_Field is Location range Address_Field .. Source_Index_Field;
   Maximum_DWord : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (Interfaces.Unsigned_32'Last);
   procedure Read_Entry (Service : in out Core.State;
      Table : V.Value_Handle; Index : Natural;
      Item : out Route_Entry; Result : out Report) is
      Description : V.Value_Description;
      Status : V.Access_Status;
      Row, Cell : V.Value_Handle := V.No_Value;
      Candidate : Route_Entry;
      procedure Release (Handle : in out V.Value_Handle; Slot : Pending_Slot) is
         Released : V.Access_Status;
      begin
         if Handle = V.No_Value then return; end if;
         Core.Release_Result (Service, Handle, Released);
         if Released /= V.Available or else Handle /= V.No_Value then
            Result.Status := Cleanup_Failed; Result.Where := Cleanup;
            Result.Access_Status := Released; Result.Pending (Slot) := Handle;
         end if;
      end Release;
   begin
      Item := (others => <>); Result := (others => <>);
      Core.Describe_Result (Service, Table, Description, Status);
      Result.Access_Status := Status;
      if Status /= V.Available then return; end if;
      if Description.Kind /= V.Package_Description then Result.Status := Not_A_Package; return; end if;
      if Index >= Description.Length then Result.Status := Row_Out_Of_Bounds; return; end if;
      Result.Where := Row_Object;
      Core.Read_Result_Element (Service, Table, Index, Row, Status);
      Result.Access_Status := Status;
      if Status /= V.Available then Result.Status := Field_Access_Failed; return; end if;
      Core.Describe_Result (Service, Row, Description, Status);
      Result.Access_Status := Status;
      if Status /= V.Available then Result.Status := Field_Access_Failed;
      elsif Description.Kind /= V.Package_Description then Result.Status := Not_A_Package;
      elsif Description.Length /= Field_Count then Result.Status := Bad_Row_Length;
      else
         Result.Status := Parsed;
         for Field in Row_Field loop
            Result.Where := Field;
            Core.Read_Result_Element (Service, Row, Natural (Location'Pos (Field) - Location'Pos (Address_Field)), Cell, Status);
            Result.Access_Status := Status;
            if Status /= V.Available then Result.Status := Field_Access_Failed;
            else
               Core.Describe_Result (Service, Cell, Description, Status);
               Result.Access_Status := Status;
               if Status /= V.Available then Result.Status := Field_Access_Failed;
               elsif Field /= Source_Field and then Description.Kind /= V.Integer_Description then
                  Result.Status := Wrong_Field_Type;
               else
                  case Field is
                     when Address_Field =>
                        declare Address : constant P.Address_Result := P.Decode_Address (Description.Number); begin
                           if Address.Valid then Candidate.Device := Address.Device;
                           else Result.Status := Bad_Address; end if;
                        end;
                     when Pin_Field =>
                        if Description.Number > P.Interrupt_Pin'Pos (P.Interrupt_Pin'Last) then Result.Status := Bad_Pin;
                        else Candidate.Pin := P.Interrupt_Pin'Val (Natural (Description.Number)); end if;
                     when Source_Field =>
                        if Description.Kind = V.Integer_Description and then Description.Number = 0 then
                           Candidate.Target := (Kind => Global_Pool, Interrupt => 0);
                        elsif Description.Kind = V.Metadata_Description then
                           declare Device : Core.Node_Descriptor; begin
                              Core.Read_Result_Device (Service, Cell, Device, Status);
                              Result.Access_Status := Status;
                              if Status = V.Available then
                                 Candidate.Target := (Kind => Link_Device, Device => Device, Index => 0);
                              else Result.Status := Bad_Source; end if;
                           end;
                        else Result.Status := Bad_Source; end if;
                     when Source_Index_Field =>
                        if Description.Number > Maximum_DWord then Result.Status := Bad_Source_Index;
                        elsif Candidate.Target.Kind = Global_Pool then
                           Candidate.Target.Interrupt := P.Global_Interrupt (Description.Number);
                        else Candidate.Target.Index := P.Source_Index (Description.Number); end if;
                  end case;
               end if;
            end if;
            Release (Cell, 1);
            exit when Result.Status /= Parsed;
         end loop;
      end if;
      Release (Row, 2);
      if Result.Status = Parsed then Item := Candidate; end if;
   end Read_Entry;
end ACPI_PCI_Routing_Reader;
