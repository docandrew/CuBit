pragma Ada_2022;
package body ACPI_Backend_NVS_Dispatch with SPARK_Mode is
   package P renames ACPI_NVS_Protocol;
   use type Interfaces.Unsigned_32, Interfaces.Unsigned_64, Records.Outcome;
   function Classify (Status : Records.Outcome) return P.Outcome is
     (case Status is
        when Records.Denied => P.Denied,
        when Records.Stale | Records.Catalog_Changed => P.Stale,
        when Records.Read_Failed => P.Fault,
        when others => P.Unavailable);
   procedure Dispatch
     (State : in out Records.Context; Scope : Interfaces.Unsigned_64;
      Item : P.Packet; Result : out P.Packet) is
      Request : constant P.Request := P.Decode_Request (Item);
      Kind : P.Operation := P.Read;
      Header : Records.Header_Report;
      Description : Records.Description_Report;
      Status : Records.Outcome;
      Value : Interfaces.Unsigned_64;
   begin
      if Item.Label = P.Header_Operation then Kind := P.Header;
      elsif Item.Label = P.Describe_Operation then Kind := P.Describe; end if;
      Result := P.Error_Reply (Kind, P.Malformed);
      if not Request.Valid then return; end if;
      case Request.Kind is
         when P.Header =>
            Records.Header (State, Scope, Header);
            if Header.Status = Records.Ready then
               if Interfaces.Unsigned_64 (Header.Count) > P.Maximum_Records then
                  Result := P.Error_Reply (P.Header, P.Fault);
               else Result := P.Header_Result (Interfaces.Unsigned_64 (Header.Epoch), Interfaces.Unsigned_64 (Header.Count)); end if;
            else Result := P.Error_Reply (P.Header, Classify (Header.Status)); end if;
         when P.Describe =>
            if Request.Position > Interfaces.Unsigned_64 (Natural'Last) then
               Result := P.Error_Reply (P.Describe, P.Denied); return;
            end if;
            Records.Describe (State, Scope, Records.Epoch_Number (Request.Epoch), Natural (Request.Position), Description);
            if Description.Status = Records.Ready then
               Result := P.Describe_Result (Interfaces.Unsigned_64 (Description.ID), Description.Base, Description.Length);
            else Result := P.Error_Reply (P.Describe, Classify (Description.Status)); end if;
         when P.Read =>
            Records.Read (State, Scope, Records.Epoch_Number (Request.Epoch), Records.Record_ID (Request.ID),
              Request.Position, Request.Width, Value, Status);
            if Status = Records.Done then Result := P.Read_Result (Value);
            else Result := P.Error_Reply (P.Read, Classify (Status)); end if;
      end case;
   end Dispatch;
end ACPI_Backend_NVS_Dispatch;
