pragma Ada_2022;
with Firmware_Tables.PCI_Windows;
package body Firmware_Tables.PCI_Aliases with SPARK_Mode is
   package W renames Firmware_Tables.PCI_Windows;
   use type Address_Value, Byte;
   type Lookup_State is record
      Base, Length : Address_Value := 0;
      Candidate : Selection;
      Matches : M.Allocation_Count := 0;
      Crossing, Invalid : Boolean := False;
   end record;
   procedure Visit (State : in out Lookup_State; Item : M.Allocation) is
   begin
      if State.Invalid then return; end if;
      if not W.Valid (Item, Address_Value'Last) then
         State.Invalid := True; return;
      end if;
      declare
         Low : constant Address_Value := Item.Base + Address_Value (Item.First_Bus) * W.Bus_Bytes;
         High : constant Address_Value := Item.Base + W.Upper_Bytes (Item);
      begin
         if State.Base < High and then Low < State.Base + State.Length then
            State.Matches := State.Matches + 1;
            if State.Base < Low or else State.Base + State.Length > High then
               State.Crossing := True;
            else
               declare
                  Bus : constant Address_Value := (State.Base - Item.Base) / W.Bus_Bytes;
                  First : constant Address_Value := Item.Base + Bus * W.Bus_Bytes;
               begin
                  if State.Length > W.Bus_Bytes - (State.Base - First) then
                     State.Crossing := True;
                  else
                     State.Candidate := (Resolved, Item.Segment, M.Bus_Number (Bus), First);
                  end if;
               end;
            end if;
         end if;
      end;
   end Visit;
   procedure Scan is new M.Scan (Lookup_State, Visit);
   function Resolve (Data : Bytes; Base, Length : Address_Value) return Selection is
      Header : M.Table_Metadata;
      State : Lookup_State := (Base => Base, Length => Length, others => <>);
   begin
      if Length = 0 or else Base > Address_Value'Last - Length then
         return (Status => Invalid_Range);
      end if;
      Scan (Data, State, Header);
      if not Header.Valid or else Header.Revision /= 1
        or else Header.Reserved /= 0 or else Header.Count = 0 or else State.Invalid then
         return (Status => Invalid_Table);
      elsif State.Matches > 1 then return (Status => Ambiguous);
      elsif State.Matches = 0 then return (Status => Outside_Window);
      elsif State.Crossing then return (Status => Crosses_Bus);
      else return State.Candidate;
      end if;
   end Resolve;
end Firmware_Tables.PCI_Aliases;
