pragma Ada_2022;
with ACPI_Inventory_Protocol;
with ACPI_Backend_Register_IDs;
package body ACPI_Timer_Probe is
   use Interfaces;
   use ACPI_Timer_Probe_Types;
   package P renames ACPI_Inventory_Protocol;
   package W renames ACPI_Region_Protocol;
   use type W.Time_Instant;
   use type P.T.Access_Width;
   Sample_Limit : constant := 8;
   Counter_Mask : constant Unsigned_64 := 16#00FF_FFFF#;
   Transaction_Mask : constant Unsigned_64 := 16#FFFF_FFFF#;
   procedure Run
     (State : in out Context; Deadline : Time_Instant; Result : out Report)
   is
      Items : Descriptions (1 .. Maximum_Registers);
      Complete : Boolean;
      Count : Natural;
      Selected : Natural := 0;
      Previous : Time_Instant := 0;
      Have_Time : Boolean := False;
      function In_Time return Boolean is
         Now : Time_Instant;
         Available : Boolean;
      begin
         Read_Clock (Now, Available);
         if not Available then
            State.Saved.Status := Clock_Unavailable;
         elsif Have_Time and then Now < Previous then
            State.Saved.Status := Clock_Regressed;
         elsif Now >= Deadline then
            State.Saved.Status := Deadline_Expired;
         else
            Previous := Now;
            Have_Time := True;
            return True;
         end if;
         return False;
      end In_Time;
      procedure Execute is
         Status : Read_Status;
         Value : Unsigned_64;
      begin
         if Deadline = W.No_Finite_Deadline then
            State.Saved.Status := Invalid_Deadline;
            return;
         end if;
         if not In_Time then return; end if;
         Load (Deadline, Complete, Count, Items);
         if not In_Time then return; end if;
         if not Complete then
            State.Saved.Status := Metadata_Failed;
            return;
         end if;
         if Count > Items'Length then
            State.Saved.Status := Invalid_Inventory;
            return;
         end if;
         for I in 1 .. Count loop
            if not P.Valid (Items (I)) then
               State.Saved.Status := Invalid_Inventory;
               return;
            end if;
            for J in 1 .. I - 1 loop
               if Items (I).ID = Items (J).ID then
                  State.Saved.Status := Invalid_Inventory;
                  return;
               end if;
            end loop;
            if Items (I).ID = ACPI_Backend_Register_IDs.PM_Timer then
               Selected := I;
            end if;
         end loop;
         if Selected = 0 then
            State.Saved.Status := Timer_Missing;
            return;
         end if;
         declare
            D : P.Description renames Items (Selected);
         begin
            if not D.Readable or else D.Writable
              or else D.Width /= P.T.Dword_Access or else D.Write_Mask /= 0
            then
               State.Saved.Status := Unsafe_Timer;
               return;
            end if;
            for Sample in 1 .. Sample_Limit loop
               if not In_Time then return; end if;
               State.Saved.Attempts := State.Saved.Attempts + 1;
               Read (Deadline,
                 (Epoch => W.Epoch_Number (D.Epoch),
                  Register => W.Register_ID (D.ID), For_Write => False, Value => 0),
                 Status, Value);
               -- Preserve uncertain completion even if the clock also fails.
               case Status is
                  when Rejected => State.Saved.Status := Read_Rejected; return;
                  when Failed => State.Saved.Status := Read_Failed; return;
                  when Unknown => State.Saved.Status := Read_Unknown; return;
                  when Completed => null;
               end case;
               State.Saved.Completed_Reads := State.Saved.Completed_Reads + 1;
               if not In_Time then return; end if;
               if Value > Transaction_Mask then
                  State.Saved.Status := Invalid_Value;
                  return;
               end if;
               State.Saved.Last := Value and Counter_Mask;
               if Sample = 1 then
                  State.Saved.First := State.Saved.Last;
               elsif State.Saved.Last /= State.Saved.First then
                  State.Saved.Status := Counter_Changed;
                  return;
               end if;
            end loop;
         end;
         State.Saved.Status := Counter_Unchanged;
      end Execute;
   begin
      if not State.Started then
         State.Started := True;
         State.Saved.Status := Running;
         Execute;
      end if;
      Result := State.Saved;
   end Run;
end ACPI_Timer_Probe;
