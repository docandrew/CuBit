with CCL.Interfaces.Wifi;
with CCL.Objects;
with CCL_Wifi;
with CuBit.Failures;

package body CCL_Wifi_Bindings is
   use Interfaces;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Objects.Build_Result;
   use type CCL.Host_Values.Value_Kind;
   package Wifi renames CCL.Interfaces.Wifi;
   Contract, Status_Contract : CCL.Objects.Binding;

   function Handles (Binding : Unsigned_32) return Boolean is
     (for some Op in Wifi.Operation => Binding = Wifi.Binding_Of (Op));

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
   begin
      Wifi.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Catalog.Resolve_Schema (Catalog, Wifi.NETWORKS_KEY, Contract);
      CCL.Catalog.Resolve_Schema (Catalog, Wifi.STATUS_KEY, Status_Contract);
      Success := CCL.Objects.Is_Bound (Contract) and then CCL.Objects.Is_Bound (Status_Contract);
      for Op in Wifi.Operation loop
         exit when not Success;
         CCL.Catalog.Resolve (Catalog, "wifi." & Wifi.Name (Op), Resolved, Found);
         Success := Found;
         if Success then
            CCL.Catalog.Install (Grants, Resolved, Wifi.Binding_Of (Op), Grant);
            Success := Grant = CCL.Catalog.Grant_Added;
         end if;
      end loop;
   end Install;

   --  A Wifi_Status object, or why the call failed.
   procedure Answer_Status
     (Status : CCL_Wifi.Connection; Result : CCL_Wifi.Control_Result;
      Reply : out CCL.Host_Values.Call_Result)
   is
      use type CCL_Wifi.Control_Result;
      Image : CCL.Objects.Image := CCL.Objects.Empty (Status_Contract);
      Step : CCL.Objects.Build_Result := CCL.Objects.Added;
      procedure Put (Cell : CCL.Objects.Cell) is
      begin
         if Step = CCL.Objects.Added then CCL.Objects.Append (Image, Cell, Step); end if;
      end Put;
      procedure Text (Value : String) is
      begin
         if Step = CCL.Objects.Added then CCL.Objects.Append_Text (Image, Value, Step); end if;
      end Text;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if Result /= CCL_Wifi.Done then
         Reply.Why := CuBit.Failures.Failed
           ((case Result is
               when CCL_Wifi.Not_Granted => CuBit.Failures.Not_Granted,
               when CCL_Wifi.Unavailable => CuBit.Failures.Unavailable,
               when others => CuBit.Failures.Refused),
            "wifi: " & CCL_Wifi.Explain (Result));
         return;
      end if;
      Put (CCL.Objects.Product_Cell (Wifi.STATUS_FIELDS));
      Put (CCL.Objects.Variant_Cell (Wifi.State'Pos (Status.State) + 1));
      Put (CCL.Objects.Unit_Cell);
      Text (Status.Failure (1 .. Status.Failure_Length));
      Text (Status.BSSID);
      Put (CCL.Objects.Variant_Cell (Wifi.Band'Pos (Status.On_Band) + 1));
      Put (CCL.Objects.Unit_Cell);
      Put (CCL.Objects.Integer_Cell (Integer_64 (Status.Channel)));
      if Step = CCL.Objects.Added and then CCL.Objects.Validate (Image, Status_Contract) then
         Reply := (Value => CCL.Host_Values.Object_Constant (Image), Success => True, Why => <>);
      else
         Reply.Why := CuBit.Failures.Failed (CuBit.Failures.Exhausted, "wifi: the status does not fit");
      end if;
   end Answer_Status;

   procedure Invoke
     (Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      Entries : CCL_Wifi.Listing;
      Count : CCL_Wifi.Listed_Count;
      Total : Natural;
      Result : CCL_Wifi.Result_Kind;
      use type CCL_Wifi.Result_Kind;
      Image : CCL.Objects.Image;
      Step : CCL.Objects.Build_Result := CCL.Objects.Added;
      procedure Put (Cell : CCL.Objects.Cell) is
      begin
         if Step = CCL.Objects.Added then CCL.Objects.Append (Image, Cell, Step); end if;
      end Put;
      procedure Text (Value : String) is
      begin
         if Step = CCL.Objects.Added then CCL.Objects.Append_Text (Image, Value, Step); end if;
      end Text;
      function Number (Value : Integer_64) return CCL.Objects.Cell is (CCL.Objects.Integer_Cell (Value));
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if not Handles (Binding) then return; end if;
      if Binding /= Wifi.Binding_Of (Wifi.Scan) then
         declare
            Status : CCL_Wifi.Connection;
            Done : CCL_Wifi.Control_Result;
            use type CCL_Wifi.Control_Result;
            function Argument_Text return String is
              (if Argument.Kind = CCL.Host_Values.Text_Value
               then Argument.Content.Data (1 .. Argument.Content.Length) else "");
            Notice : CCL.Host_Values.Text;
            Copied : Boolean;
         begin
            if Binding = Wifi.Binding_Of (Wifi.Join) then
               CCL_Wifi.Join (Argument_Text, Status, Done);
               Answer_Status (Status, Done, Reply);
            elsif Binding = Wifi.Binding_Of (Wifi.Leave) then
               CCL_Wifi.Leave (Status, Done);
               Answer_Status (Status, Done, Reply);
            elsif Binding = Wifi.Binding_Of (Wifi.Status) then
               CCL_Wifi.Get_Status (Status, Done);
               Answer_Status (Status, Done, Reply);
            else
               CCL_Wifi.Request_Passphrase (Argument_Text, Done);
               if Done = CCL_Wifi.Done then
                  CCL.Host_Values.Copy_Text
                    ("Type the passphrase for """ & Argument_Text & """ and press Enter. " &
                     "It is not shown, not kept in history, and goes only to wifi.svc.", Notice, Copied);
                  Reply := (Value => CCL.Host_Values.Text_Constant (Notice), Success => Copied, Why => <>);
               else
                  Reply.Why := CuBit.Failures.Failed
                    ((if Done = CCL_Wifi.Not_Granted then CuBit.Failures.Not_Granted
                      else CuBit.Failures.Refused), "wifi: " & CCL_Wifi.Explain (Done));
               end if;
            end if;
         end;
         return;
      end if;
      CCL_Wifi.Scan (Entries, Count, Total, Result);
      if Result = CCL_Wifi.Not_Granted then
         Reply.Why := CuBit.Failures.Failed
           (CuBit.Failures.Not_Granted, "reading what the radios heard needs netstack's endpoint",
            "the program's manifest must request the network-stack service " &
            "(service ""network-stack"" ""network"")");
         return;
      elsif Result = CCL_Wifi.Unavailable then
         Reply.Why := CuBit.Failures.Failed
           (CuBit.Failures.Unavailable, "netstack did not answer");
         return;
      end if;
      Image := CCL.Objects.Empty (Contract);
      Put (CCL.Objects.Sequence_Cell (Count));
      for I in 1 .. Count loop
         Put (CCL.Objects.Product_Cell (Wifi.NETWORK_FIELDS));
         Text (Entries (I).Interface_Name (1 .. Entries (I).Interface_Length));
         Text (Entries (I).BSSID);
         Text (Entries (I).SSID (1 .. Entries (I).SSID_Length));
         Put (CCL.Objects.Variant_Cell (Wifi.Band'Pos (Entries (I).On_Band) + 1));
         Put (CCL.Objects.Unit_Cell);
         Put (Number (Integer_64 (Entries (I).Channel)));
         Put (Number (Integer_64 (Entries (I).Signal)));
         Put (CCL.Objects.Variant_Cell (Wifi.Security'Pos (Entries (I).Protection) + 1));
         Put (CCL.Objects.Unit_Cell);
         Put (Number (Integer_64 (Entries (I).Capability)));
         Put (Number (Integer_64 (Entries (I).Heard)));
      end loop;
      if Step = CCL.Objects.Added and then CCL.Objects.Validate (Image, Contract) then
         Reply := (Value => CCL.Host_Values.Object_Constant (Image), Success => True, Why => <>);
      else
         Reply.Why := CuBit.Failures.Failed
           (CuBit.Failures.Exhausted, Natural'Image (Total) & " networks do not fit one result");
      end if;
   end Invoke;
end CCL_Wifi_Bindings;
