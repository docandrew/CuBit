pragma Ada_2022;
package body ACPI_EC_Backend_Progress is
   use type Registers.Progress_Kind, Events.Phase;
   Prefer_Field : Boolean := True;
   function Next (Now : ACPI_EC_Types.Microseconds) return Report is
      Result : Report;
      Hint : constant Registers.Progress := Registers.Next_Progress (Now);
      Publishing, Querying : Boolean;
   begin
      Result.Field_Reply_Pending := Calls.Pending;
      Result.Notice_Retained := Events.Retained.Present;
      Result.Hardware_Recovery := Registers.Retained_For_Recovery;
      Result.Event_Stage := Events.Observe.Stage;
      Publishing := Result.Event_Stage = Events.Publication_Pending;
      Querying := Result.Event_Stage = Events.Querying;
      if Calls.Faulted then Result.Action := Owner_Fault; return Result; end if;
      if (Result.Field_Reply_Pending and Querying)
        or else ((Result.Field_Reply_Pending or Querying) and Hint.Kind = Registers.No_Work)
        or else (not Result.Field_Reply_Pending and not Querying and Hint.Kind /= Registers.No_Work)
      then Result.Action := Inconsistent_Owners; return Result; end if;
      if Publishing and then (not Result.Field_Reply_Pending
        or else Hint.Kind = Registers.Wait_Until or else not Prefer_Field)
      then Result.Action := Publish_Notice;
      elsif Result.Field_Reply_Pending or Querying then
         if Hint.Kind = Registers.Wait_Until then
            Result.Action := Wait_Until; Result.At_Time := Hint.At_Time;
         else Result.Action := (if Querying then Advance_Query else Advance_Field);
         end if;
      end if;
      return Result;
   end Next;
   procedure Execute
     (Before : Report; Now : ACPI_EC_Types.Microseconds; Result : out Report) is
      Field : Calls.Report;
   begin
      case Before.Action is
         when Advance_Field => Calls.Advance (Field); Prefer_Field := False;
         when Advance_Query | Publish_Notice => Events.Advance; Prefer_Field := True;
         when others => null;
      end case;
      Result := Next (Now);
      --  Action identifies the work actually selected,not a second recommendation.
      Result.Action := Before.Action; Result.At_Time := Before.At_Time;
      if Before.Action = Advance_Field then Result.Field := Field; end if;
   end Execute;
   procedure Advance (Now : ACPI_EC_Types.Microseconds; Result : out Report) is
   begin Execute (Next (Now), Now, Result); end Advance;
   procedure Advance_Waiter (Now : ACPI_EC_Types.Microseconds; Result : out Report) is
      Selected : Report := Next (Now);
   begin
      if Selected.Action = Wait_Until then
         Selected.Action := (if Selected.Field_Reply_Pending then Advance_Field else Advance_Query);
         Selected.At_Time := 0;
      end if;
      Execute (Selected, Now, Result);
   end Advance_Waiter;
end ACPI_EC_Backend_Progress;
