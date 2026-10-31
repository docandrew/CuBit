pragma Ada_2022;
package body ACPI_Inventory_Client is
   use Interfaces;
   use type Wire.Time_Instant, Wire.Delivery;
   procedure Load
     (Transport : in out Transport_State; Destination : Endpoint;
      Deadline : Wire.Time_Instant; Result : out Snapshot)
   is
      Copy : Snapshot;
      Previous : Wire.Time_Instant := 0;
      First_Clock : Boolean := True;
      Reply : Wire.Packet;
      OK : Boolean;
      function Within_Deadline return Boolean is
         Now : Wire.Time_Instant;
         Available : Boolean;
      begin
         Read_Clock (Now, Available);
         if not Available or else Now >= Deadline
           or else (not First_Clock and then Now < Previous) then return False; end if;
         Previous := Now; First_Clock := False; return True;
      end Within_Deadline;
      procedure Call (Item : Wire.Packet) is
         Delivered : Wire.Delivery;
      begin
         OK := False;
         if not Within_Deadline then return; end if;
         Exchange (Transport, Destination, Deadline, Item, Reply, Delivered);
         if not Within_Deadline then return; end if;
         OK := Delivered = Wire.Reply_Received;
      end Call;
   begin
      Result := (others => <>);
      if Destination = No_Endpoint or else Deadline = Wire.No_Finite_Deadline then return; end if;
      Call (Proto.Header_Request); if not OK then return; end if;
      declare Header : constant Proto.Header := Proto.Decode_Header (Reply); begin
         if not Header.Valid or else Header.Count > Unsigned_64 (Maximum_Registers) then return; end if;
         Copy.Revision := Header.Revision; Copy.Count := Register_Count (Header.Count);
      end;
      for Index in Register_Index loop
         exit when Index > Copy.Count;
         declare Parts : Proto.Parts; begin
            for Part in Proto.Part_Kind loop
               Call (Proto.Part_Request (Copy.Revision, Unsigned_64 (Index), Part));
               if not OK then return; end if;
               declare Decoded : constant Proto.Decoded_Part := Proto.Decode_Part (Reply); begin
                  if not Decoded.Valid then return; end if;
                  Parts (Part) := Decoded.Value;
               end;
            end loop;
            declare Decoded : constant Proto.Decoded_Description := Proto.Decode (Parts); begin
               if not Decoded.Valid then return; end if;
               for Prior in Register_Index loop
                  exit when Prior >= Index;
                  if Copy.Items (Prior).ID = Decoded.Value.ID then return; end if;
               end loop;
               Copy.Items (Index) := Decoded.Value;
            end;
         end;
      end loop;
      Call (Proto.Header_Request); if not OK then return; end if;
      declare Header : constant Proto.Header := Proto.Decode_Header (Reply); begin
         if not Header.Valid or else Header.Revision /= Copy.Revision
           or else Header.Count /= Unsigned_64 (Copy.Count) then return; end if;
      end;
      Copy.Complete := True; Result := Copy;
   end Load;
end ACPI_Inventory_Client;
