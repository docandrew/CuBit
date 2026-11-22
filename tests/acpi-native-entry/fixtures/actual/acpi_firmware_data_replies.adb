pragma Ada_2022;
with Interfaces;
package body ACPI_Firmware_Data_Replies with SPARK_Mode is
   use Interfaces;
   use type F.Identifier, F.Operation;
   function Is_Data (Kind : F.Operation) return Boolean is
      (Kind in F.Read_Datum | F.Write_Datum | F.Query_Operation);
   function Valid (Item : Reply) return Boolean is
   begin
      if Item.Correlation = F.No_ID then return False; end if;
      if Is_Data (Item.Kind) then
         if Item.Auxiliary = F.No_ID or else Item.Status not in
            Running | Done | Invalid_Request | Request_Changed | Capacity_Refused |
            Busy | Denied | Indeterminate | No_Evidence | Backend_Fault
         then return False; end if;
         if Item.Status in Running | Done then
            if Item.Effect = No_Effect then return False; end if;
         elsif Item.Status in Invalid_Request | Request_Changed | Capacity_Refused | Busy | No_Evidence then
            if Item.Effect /= No_Effect then return False; end if;
         end if;
         if (Item.Kind = F.Read_Datum and then Item.Effect = Write_Effect)
           or else (Item.Kind = F.Write_Datum and then Item.Effect = Read_Effect)
         then return False; end if;
         return (Item.Status = Done and then Item.Effect = Read_Effect) or else Item.Value = 0;
      end if;
      if Item.Effect /= No_Effect or else
        (if Item.Kind = F.Select_Datum then Item.Auxiliary = F.No_ID else Item.Auxiliary /= F.No_ID)
      then return False; end if;
      case Item.Kind is
         when F.Publish | F.Describe_Publication | F.Recover_Publication =>
            if Item.Status not in Ready | Pending | Refused | Stopped | Capacity_Refused |
               Invalid_Request | Request_Changed | Not_Found | Busy | Denied then return False; end if;
         when F.Begin_Scope | F.Describe_Scope =>
            if Item.Status not in Ready | Closed | Stopped | Invalid_Request | Request_Changed |
               Denied | Clock_Unavailable | Clock_Reversed | Expired | Lease_Refused |
               Capacity_Refused | Busy | Not_Found then return False; end if;
         when F.Select_Datum =>
            if Item.Status not in Ready | Closed | Stopped | Invalid_Request | Request_Changed |
               Denied | Clock_Unavailable | Clock_Reversed | Expired | Lease_Refused |
               Selection_Refused | Capacity_Refused | Busy | Not_Found then return False; end if;
         when F.Close_Scope =>
            if Item.Status not in Closed | Invalid_Request | Busy | Not_Found | Denied then return False; end if;
         when others => return False;
      end case;
      return (if Item.Status = Ready then Item.Value /= 0 else Item.Value = 0);
   end Valid;
   function Encode (Item : Reply) return IPC.Packet is
   begin
      return (Label => F.Reply_Label_For (Item.Kind),
         Data => [Word (Item.Correlation), Word (Item.Auxiliary), Word (Outcome'Pos (Item.Status))
            or Shift_Left (Word (Effect_Kind'Pos (Item.Effect)),Code_Bits), Item.Value], others => <>);
   end Encode;
   function Decode (Item : IPC.Packet; Request : F.Request;
      Expected_Effect : Effect_Kind; Has_Capability : Boolean) return Decoded is
      Auxiliary : F.Identifier := F.No_ID;
      Status_Code, Effect_Code : Word;
   begin
      if not F.Valid (Request) or else Has_Capability or else Item.Length /= 4
        or else Item.Flags /= 0 or else Item.Reserved /= 0
        or else Item.Label /= F.Reply_Label_For (Request.Kind)
        or else Item.Data (0) /= Word (Request.ID)
        or else Shift_Right (Item.Data (2),Used_Status_Bits) /= 0
      then return (Valid => False); end if;
      case Request.Kind is
         when F.Select_Datum => Auxiliary := Request.Client_Descriptor_ID;
         when F.Read_Datum | F.Query_Operation => Auxiliary := Request.Operation_ID;
         when F.Write_Datum => Auxiliary := Request.Write_Operation_ID;
         when others => null;
      end case;
      if Item.Data (1) /= Word (Auxiliary) then return (Valid => False); end if;
      if (Is_Data (Request.Kind) and then Expected_Effect = No_Effect)
        or else (not Is_Data (Request.Kind) and then Expected_Effect /= No_Effect)
        or else (Request.Kind = F.Read_Datum and then Expected_Effect /= Read_Effect)
        or else (Request.Kind = F.Write_Datum and then Expected_Effect /= Write_Effect)
      then return (Valid => False); end if;
      Status_Code := Item.Data (2) and Code_Mask;
      Effect_Code := Shift_Right (Item.Data (2),Code_Bits) and Code_Mask;
      if Status_Code > Outcome'Pos (Outcome'Last) or else Effect_Code > Effect_Kind'Pos (Effect_Kind'Last)
      then return (Valid => False); end if;
      declare
         Result : constant Reply := (Request.Kind,Request.ID,Auxiliary,
            Outcome'Val (Status_Code),Effect_Kind'Val (Effect_Code),Item.Data (3));
      begin
         if not Valid (Result) or else (Result.Effect /= No_Effect and then Result.Effect /= Expected_Effect)
         then return (Valid => False); end if;
         return (Valid => True,Item => Result);
      end;
   end Decode;
end ACPI_Firmware_Data_Replies;
