------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body USB_Disk_Probe with SPARK_Mode is

   function Command (P : Probe) return SCSI.Command_Block is
     (case P.Current is
         when Inquire     => SCSI.Inquiry (Inquiry_Allocation),
         when Test_Ready  => SCSI.Test_Unit_Ready,
         when Read_Sense  => SCSI.Request_Sense (Sense_Allocation),
         when Capacity_10 => SCSI.Read_Capacity_10,
         when Capacity_16 => SCSI.Read_Capacity_16,
         when Mode_Sense  => SCSI.Mode_Sense_6 (Mode_Allocation),
         when Check_ISO   =>
           (if ISO_Block (P.Geometry) <= Unsigned_64 (Unsigned_32'Last)
            then SCSI.Read_10 (Unsigned_32 (ISO_Block (P.Geometry)), 1,
                               SCSI_Disks.Bytes_Of (P.Geometry.Size))
            else SCSI.Read_16 (ISO_Block (P.Geometry), 1,
                               SCSI_Disks.Bytes_Of (P.Geometry.Size))),
         when Finished    => SCSI.Test_Unit_Ready);

   --  Another TEST UNIT READY, or the end.
   procedure Retry (P : in out Probe)
     with Pre  => P.Current in Test_Ready | Read_Sense,
          Post => Remaining (P) < Remaining (P'Old)
                  and then P.Current in Test_Ready | Failed
                  and then P.Geometry = P'Old.Geometry
                  and then P.Write_Protected = P'Old.Write_Protected;

   procedure Retry (P : in out Probe) is
   begin
      if P.Attempts < Ready_Attempt_Limit then
         P.Attempts := P.Attempts + 1;
         P.Current := Test_Ready;
      else
         P.Current := Failed;
      end if;
   end Retry;

   --  After the capacity: the protection, then the ISO check.
   procedure After_Mode_Sense (P : in out Probe)
     with Pre  => P.Current = Mode_Sense,
          Post => P.Current = (if ISO_Block (P.Geometry) < P.Geometry.Blocks
                               then Check_ISO else Ready)
                  and then P.Geometry = P'Old.Geometry
                  and then P.Write_Protected = P'Old.Write_Protected;

   procedure After_Mode_Sense (P : in out Probe) is
   begin
      P.Current := (if ISO_Block (P.Geometry) < P.Geometry.Blocks
                    then Check_ISO else Ready);
   end After_Mode_Sense;

   procedure Advance
     (P : in out Probe; Kind : USB_Bulk_Only.Status_Kind; Data : SCSI.Bytes)
   is
      Good : constant Boolean := Kind = USB_Bulk_Only.Good;
   begin
      case Active'(P.Current) is
         when Inquire =>
            declare
               Item : SCSI.Inquiry_Data;
               Valid : Boolean;
            begin
               if not Good then
                  P.Current := Failed;
                  return;
               end if;
               SCSI.Decode_Inquiry (Data, Item, Valid);
               if Valid and then SCSI_Disks.Is_Disk (Item) then
                  P.Identity := Item;
                  P.Removable := Item.Removable;
                  P.Current := Test_Ready;
               else
                  P.Current := Not_A_Disk;
               end if;
            end;

         when Test_Ready =>
            if Good then
               P.Current := Capacity_10;
            elsif Kind = USB_Bulk_Only.Check_Condition then
               P.Current := Read_Sense;
            else
               Retry (P);
            end if;

         when Read_Sense =>
            declare
               Sense : SCSI.Sense_Data;
               Valid : Boolean;
            begin
               if Good then
                  SCSI.Decode_Sense (Data, Sense, Valid);
                  if Valid then
                     P.Sense := SCSI_Disks.Meaning (Sense);
                     case P.Sense is
                        when SCSI_Disks.Medium_Absent =>
                           P.Current := No_Medium;
                           return;
                        when SCSI_Disks.No_Error | SCSI_Disks.Retry_Later
                           | SCSI_Disks.Medium_Changed =>
                           null;
                        when others =>
                           P.Current := Failed;
                           return;
                     end case;
                  end if;
               end if;
               Retry (P);
            end;

         when Capacity_10 | Capacity_16 =>
            declare
               G : SCSI_Disks.Geometry;
               Outcome : SCSI_Disks.Capacity_Outcome;
               use type SCSI_Disks.Capacity_Outcome;
            begin
               if not Good then
                  P.Current := Failed;
                  return;
               end if;
               if P.Current = Capacity_10 then
                  SCSI_Disks.From_Capacity_10 (Data, G, Outcome);
               else
                  SCSI_Disks.From_Capacity_16 (Data, G, Outcome);
               end if;
               case Outcome is
                  when SCSI_Disks.Capacity_Valid =>
                     P.Geometry := G;
                     P.Current := Mode_Sense;
                  when SCSI_Disks.Capacity_16_Required =>
                     P.Current := Capacity_16;
                  when others =>
                     P.Current := Failed;
               end case;
            end;

         when Mode_Sense =>
            declare
               Header : SCSI.Mode_Header;
               Valid : Boolean := False;
            begin
               if Good then
                  SCSI.Decode_Mode_Header_6 (Data, Header, Valid);
               end if;
               P.Write_Protected := Valid and then Header.Write_Protected;
               P.Mode_Unknown := not Valid;
               After_Mode_Sense (P);
            end;

         when Check_ISO =>
            if Good and then Data'Length >= ISO_Signature_Bytes then
               P.Current := (if ISO_Signature (Data) then ISO_Medium else Ready);
            else
               P.Current := Failed;
            end if;
      end case;
   end Advance;
end USB_Disk_Probe;
