with AML_Names;
with AML_Decode;
package body ACPI_Battery_Information_Reader with SPARK_Mode is
   package V renames Core.Values;
   package N renames Core.Namespace;
   package F renames Format;
   use type V.Access_Status;
   use type V.Value_Handle;
   use type V.Value_Kind;
   use type N.Object_Kind;
   use type N.Lookup_Status;
   use type AML_Execute.Execution_Status;
   use type F.Outcome;
   use type F.Field;
   use type Interfaces.Unsigned_64;
   BIX_Member : constant AML_Names.Segment := "_BIX";
   subtype Readable_Field is F.Field range F.Revision .. F.Swapping_Capability;
   procedure Query (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Report : out Query_Report) is
      Before_Roots : constant Natural := Core.Retained_Results (Service);
      Found : N.Lookup_Result;
      Member : Core.Node_Descriptor;
      Value : V.Result;
      Root, Element : V.Value_Handle := V.No_Value;
      Root_Tried, Element_Tried, Cleanup_Error : Boolean := False;
      Description : V.Value_Description;
      Identifiers : F.Texts := F.Empty_Texts;
      Data : AML_Decode.Bytes (1 .. F.Text_Capacity) := [others => 0];
      Copied : Natural;
      Decoded : F.Decode_Result;
      procedure Drop (Handle : in out V.Value_Handle; Tried : in out Boolean) is
         Status : V.Access_Status;
      begin
         if Tried or else Handle = V.No_Value then return; end if;
         Tried := True;
         Core.Release_Result (Service, Handle, Status);
         if Status /= V.Available or else Handle /= V.No_Value then
            Report.Cleanup_Result := Status; Cleanup_Error := True;
         end if;
      end Drop;
      function Read_Field (Item : Readable_Field) return Boolean is
      begin
         Report.Failure_Field := Item;
         if Report.Charged = Budget then Report.Outcome := Budget_Exhausted; return False; end if;
         Report.Charged := Report.Charged + 1;
         Element_Tried := False;
         Core.Read_Result_Element (Service, Root, F.Field'Pos (Item) - 1,
                                   Element, Report.Access_Result);
         if Report.Access_Result /= V.Available then Report.Outcome := Evaluation_Failed; return False; end if;
         Core.Describe_Result (Service, Element, Description, Report.Access_Result);
         if Report.Access_Result /= V.Available then Report.Outcome := Evaluation_Failed; return False; end if;
         if Item in F.Text_Field then
            if Description.Kind /= V.String_Description then Report.Outcome := Wrong_Element_Type; return False; end if;
            if Description.Length > F.Text_Capacity then Report.Outcome := Text_Too_Long; return False; end if;
            if Description.Length > Budget - Report.Charged then Report.Outcome := Budget_Exhausted; return False; end if;
            Report.Charged := Report.Charged + Description.Length;
            if Description.Length > 0 then
               Core.Read_Result_Bytes (Service, Element, 0, Data (1 .. Description.Length), Copied, Report.Access_Result);
               if Report.Access_Result /= V.Available or else Copied /= Description.Length then
                  Report.Outcome := Evaluation_Failed; return False;
               end if;
               for I in 1 .. Description.Length loop
                  Identifiers (Item).Data (I) := Interfaces.Unsigned_8 (Data (I));
               end loop;
            end if;
            Identifiers (Item).Length := Description.Length;
         else
            if Description.Kind /= V.Integer_Description then Report.Outcome := Wrong_Element_Type; return False; end if;
            if Item = F.Swapping_Capability then Report.Raw_Swap := Interfaces.Unsigned_64 (Description.Number);
            else Report.Raw (Item) := Interfaces.Unsigned_64 (Description.Number);
            end if;
         end if;
         Report.Fields_Read := Report.Fields_Read + 1;
         Drop (Element, Element_Tried);
         return not Cleanup_Error;
      end Read_Field;
      procedure Inspect is
      begin
         if Value.Status /= AML_Execute.Object_Returned then Report.Outcome := Wrong_Result_Type; return; end if;
         Core.Describe_Result (Service, Root, Description, Report.Access_Result);
         if Report.Access_Result /= V.Available then Report.Outcome := Evaluation_Failed; return; end if;
         if Description.Kind /= V.Package_Description then Report.Outcome := Wrong_Result_Type; return; end if;
         Report.Elements := Description.Length;
         if Report.Elements not in F.Revision_Zero_Elements | F.Revision_One_Elements then
            Report.Outcome := Wrong_Element_Count; return;
         end if;
         for Item in F.Numeric_Field loop
            if not Read_Field (Item) then return; end if;
            if Item = F.Revision then
               if Report.Raw (Item) > 1 then
                  Report.Outcome := Invalid_Package_Value;
                  Report.Decode_Status := F.Unsupported_Revision; return;
               end if;
               if (Report.Raw (Item) = 0 and then Report.Elements /= F.Revision_Zero_Elements)
                 or else (Report.Raw (Item) = 1 and then Report.Elements /= F.Revision_One_Elements)
               then Report.Outcome := Wrong_Element_Count; return; end if;
            end if;
         end loop;
         for Item in F.Text_Field loop
            if not Read_Field (Item) then return; end if;
         end loop;
         if Report.Elements = F.Revision_One_Elements then
            if not Read_Field (F.Swapping_Capability) then return; end if;
         end if;
         Decoded := F.Decode (Report.Raw, Identifiers,
                              Report.Elements = F.Revision_One_Elements, Report.Raw_Swap);
         Report.Decode_Status := Decoded.Status;
         Report.Failure_Field := Decoded.Failure_Field;
         if Decoded.Status /= F.Decoded then Report.Outcome := Invalid_Package_Value; return; end if;
         Report.Value := Decoded.Value;
         Report.Outcome := Sampled;
      end Inspect;
      procedure Evaluate is
      begin
         if not Core.Current_Node (Service, Device) or else V.Node_Kind (Device) /= N.Device_Object then return; end if;
         if Budget = 0 then Report.Outcome := Budget_Exhausted; return; end if;
         Core.Find_Object (Service, V.Position (Device), BIX_Member, Found, Report.Access_Result);
         if Report.Access_Result /= V.Available then Report.Outcome := Lookup_Failed; return; end if;
         if Found.Status = N.Not_Found then Report.Outcome := Missing_BIX; return; end if;
         if Found.Status /= N.Found then Report.Outcome := Lookup_Failed; return; end if;
         Core.Describe_Node (Service, Found.Node, Member, Report.Access_Result);
         if Report.Access_Result /= V.Available then Report.Outcome := Lookup_Failed; return; end if;
         Evaluate_Named (Service, Member, Budget, Value, Report.Access_Result);
         Report.Execution := Value.Status;
         Report.Last_Charge := Value.Charged;
         if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
                            AML_Execute.Metadata_Returned then Root := Value.Handle; end if;
         if Value.Charged > Budget then Report.Outcome := Provider_Contract_Error; return; end if;
         Report.Charged := Value.Charged;
         if not Core.Current_Node (Service, Device) or else not Core.Current_Node (Service, Member) then
            Report.Outcome := Identity_Changed;
         elsif Report.Access_Result /= V.Available then Report.Outcome := Evaluation_Failed;
         elsif Value.Status = AML_Execute.Budget_Exceeded then Report.Outcome := Budget_Exhausted;
         elsif Value.Status in AML_Execute.Returned | AML_Execute.Object_Returned |
                               AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned then Inspect;
         else Report.Outcome := Evaluation_Failed;
         end if;
      end Evaluate;
   begin
      Report := (others => <>);
      Evaluate;
      Drop (Element, Element_Tried); Drop (Root, Root_Tried);
      if Cleanup_Error then Report.Outcome := Cleanup_Failed;
      elsif Core.Retained_Results (Service) /= Before_Roots then Report.Outcome := Provider_Contract_Error;
      end if;
      if Report.Outcome /= Sampled then Report.Value := F.Empty_Information; end if;
   end Query;
end ACPI_Battery_Information_Reader;
