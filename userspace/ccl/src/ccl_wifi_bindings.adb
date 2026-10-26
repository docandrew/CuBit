with CCL.Interfaces.Wifi;
with CCL.Objects;
with CCL_Wifi;
with CuBit.Failures;

package body CCL_Wifi_Bindings is
   use Interfaces;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Objects.Build_Result;
   package Wifi renames CCL.Interfaces.Wifi;
   Contract : CCL.Objects.Binding;

   function Handles (Binding : Unsigned_32) return Boolean is
     (Binding = Wifi.Binding_Of (Wifi.Scan));

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
      CCL.Catalog.Resolve (Catalog, "wifi.scan", Resolved, Found);
      Success := Found and then CCL.Objects.Is_Bound (Contract);
      if not Success then return; end if;
      CCL.Catalog.Install (Grants, Resolved, Wifi.Binding_Of (Wifi.Scan), Grant);
      Success := Grant = CCL.Catalog.Grant_Added;
   end Install;

   procedure Invoke
     (Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (Argument);
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
