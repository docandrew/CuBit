pragma Ada_2022;
with ACPI_Backend_Binding_Types;
package body ACPI_Backend_PCI_Dispatch with SPARK_Mode is
   package Q renames ACPI_PCI_Protocol;
   use type Interfaces.Unsigned_32, Interfaces.Unsigned_64, Reader.Outcome;
   function Classify (Code : Reader.Outcome) return Q.Outcome is
     (case Code is
        when Reader.Denied => Q.Denied,
        when Reader.Stale => Q.Stale,
        when Reader.Catalog_Changed => Q.Catalog_Changed,
        when Reader.Not_Ready => Q.Not_Ready,
        when Reader.Backing_Unavailable => Q.Backing_Unavailable,
        when Reader.Deadline_Expired => Q.Deadline_Expired,
        when Reader.Clock_Unavailable => Q.Clock_Unavailable,
        when Reader.Clock_Reversed => Q.Clock_Reversed,
        when Reader.Read_Failed => Q.Fault,
        when Reader.Reentered => Q.Reentered,
        when others => Q.Unavailable);
   procedure Dispatch
     (State : in out Reader.Context; Scope : Interfaces.Unsigned_64;
      Item : Q.Packet; Result : out Q.Packet) is
      Request : constant Q.Request := Q.Decode_Request (Item);
      Kind : Q.Operation := Q.Read;
      Header : Reader.Header_Report;
      Code : Reader.Outcome;
      Reason : Q.P.Outcome;
      Value : Interfaces.Unsigned_64;
   begin
      if Item.Label = Q.Header_Operation then Kind := Q.Header;
      elsif Item.Label = Q.Describe_Operation then Kind := Q.Describe; end if;
      Result := Q.Error_Reply (Kind, Q.Malformed);
      if not Request.Valid then return; end if;
      case Request.Kind is
         when Q.Header =>
            Reader.Header (State, Scope, Header);
            if Header.Status = Reader.Ready then
               Result := Q.Header_Result (Interfaces.Unsigned_64 (Header.Epoch), Interfaces.Unsigned_64 (Header.ID));
            else Result := Q.Error_Reply (Q.Header, Classify (Header.Status)); end if;
         when Q.Describe =>
            Reader.Header (State, Scope, Header);
            if Header.Status /= Reader.Ready then
               Result := Q.Error_Reply (Q.Describe, Classify (Header.Status));
            elsif Request.Epoch /= Interfaces.Unsigned_64 (Header.Epoch) then
               Result := Q.Error_Reply (Q.Describe, Q.Stale);
            elsif Request.ID /= Interfaces.Unsigned_64 (Header.ID) then
               Result := Q.Error_Reply (Q.Describe, Q.Denied);
            else Result := Q.Describe_Result (Header.First, Q.P.Bus_Bytes); end if;
         when Q.Read =>
            Reader.Read (State, Scope, Reader.Epoch_Number (Request.Epoch), Reader.Record_ID (Request.ID),
              Request.Region, Request.Offset, Request.Width, Value, Code, Reason,
              ACPI_Backend_Binding_Types.Tick (Request.Deadline), Request.For_Write);
            if Code = Reader.Done then Result := Q.Read_Result (Value);
            elsif Code = Reader.Policy_Refused then Result := Q.Error_Reply (Q.Read, Q.Policy_Refused, Reason);
            else Result := Q.Error_Reply (Q.Read, Classify (Code)); end if;
      end case;
   end Dispatch;
end ACPI_Backend_PCI_Dispatch;
