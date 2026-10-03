with CCL.Interfaces.Processes;
with CCL.Objects;
with CCL_Processes;
with CuBit.Failures;

package body CCL_Process_Bindings is
   use Interfaces;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Objects.Build_Result;
   package Processes renames CCL.Interfaces.Processes;
   Contract : CCL.Objects.Binding;

   function Handles (Binding : Unsigned_32) return Boolean is
     (Binding = Processes.Binding_Of (Processes.List));

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
   begin
      Processes.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Catalog.Resolve_Schema (Catalog, Processes.PROCESSES_KEY, Contract);
      CCL.Catalog.Resolve (Catalog, "proc.list", Resolved, Found);
      Success := Found and then CCL.Objects.Is_Bound (Contract);
      if not Success then return; end if;
      CCL.Catalog.Install (Grants, Resolved, Processes.Binding_Of (Processes.List), Grant);
      Success := Grant = CCL.Catalog.Grant_Added;
   end Install;

   procedure Invoke
     (Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (Argument);
      Entries : CCL_Processes.Listing;
      Count : CCL_Processes.Listed_Count;
      Total : Natural;
      Result : CCL_Processes.Result_Kind;
      use type CCL_Processes.Result_Kind;
      Image : CCL.Objects.Image;
      Step : CCL.Objects.Build_Result := CCL.Objects.Added;
      procedure Put (Cell : CCL.Objects.Cell) is
      begin
         if Step = CCL.Objects.Added then CCL.Objects.Append (Image, Cell, Step); end if;
      end Put;
      function Number (Value : Integer_64) return CCL.Objects.Cell is (CCL.Objects.Integer_Cell (Value));
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if not Handles (Binding) then return; end if;
      CCL_Processes.List (Entries, Count, Total, Result);
      if Result = CCL_Processes.Not_Granted then
         Reply.Why := CuBit.Failures.Failed
           (CuBit.Failures.Not_Granted, "seeing what runs needs procmgr's process-observer role",
            "the program's manifest must request the process-observer service " &
            "(request-service process-observer read-write process-observer)");
         return;
      elsif Result = CCL_Processes.Unavailable then
         Reply.Why := CuBit.Failures.Failed
           (CuBit.Failures.Unavailable, "the process manager did not answer");
         return;
      end if;
      Image := CCL.Objects.Empty (Contract);
      Put (CCL.Objects.Sequence_Cell (Count));
      for I in 1 .. Count loop
         Put (CCL.Objects.Product_Cell (Processes.PROCESS_FIELDS));
         Put (Number (Integer_64 (Entries (I).Pid)));
         if Step = CCL.Objects.Added then
            CCL.Objects.Append_Text (Image, Entries (I).Name (1 .. Entries (I).Name_Length), Step);
         end if;
         if Step = CCL.Objects.Added then
            CCL.Objects.Append_Text (Image, Entries (I).Identity (1 .. Entries (I).Identity_Length), Step);
         end if;
         Put (CCL.Objects.Variant_Cell (Processes.Run_State'Pos (Entries (I).State) + 1));
         Put (CCL.Objects.Unit_Cell);
         Put (Number (Integer_64 (Unsigned_64'Min (Entries (I).Memory, Unsigned_64 (Integer_64'Last)))));
         Put (Number (Integer_64 (Entries (I).Launcher)));
         Put (Number (Integer_64 (Unsigned_64'Min (Entries (I).Age_Ms, Unsigned_64 (Integer_64'Last)))));
      end loop;
      if Step = CCL.Objects.Added and then CCL.Objects.Validate (Image, Contract) then
         Reply := (Value => CCL.Host_Values.Object_Constant (Image), Success => True, Why => <>);
      else
         Reply.Why := CuBit.Failures.Failed
           (CuBit.Failures.Exhausted, Natural'Image (Total) & " processes do not fit one result");
      end if;
   end Invoke;
end CCL_Process_Bindings;
