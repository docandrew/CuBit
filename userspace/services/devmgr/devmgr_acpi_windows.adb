pragma Ada_2022;

package body Devmgr_ACPI_Windows with SPARK_Mode is

   function Plan (Device : Admitted; Resources : RS.Section_Plan) return Plan_Result is
      Result : Plan_Result := (Status => Granted, others => <>);
      Has_Device : constant Boolean :=
        (for some I in 1 .. Resources.Count => Resources.Entries (I).Kind in RS.Device_Resource);

      function Refused (Why : Outcome; At_Entry : RS.Entry_Count) return Plan_Result is
        ((Status => Why, Request => At_Entry, Total => 0, Windows => [others => (others => <>)],
          Parent_Line => False));
   begin
      --  No device resource (none, or only scheduling): nothing to give.
      if not Has_Device then
         return Result;
      elsif Resources.Basis.Kind /= RS.ACPI_Basis then
         return Refused (Not_ACPI_Resources, 0);
      end if;
      for I in 1 .. Resources.Count loop
         pragma Loop_Invariant (Result.Status = Granted and then Result.Request = 0);
         pragma Loop_Invariant
           (Result.Parent_Line = (for some J in 1 .. I - 1 => Is_Parent_Line (Resources.Entries (J))));
         pragma Loop_Invariant
           (for all K in 1 .. Result.Total =>
              Result.Windows (K).From_Span <= Device.Count
              and then Fits (Result.Windows (K), Device.Items (Result.Windows (K).From_Span),
                             RS.MAX_DEVICE_MEMORY_BYTES));
         declare
            R : constant RS.Resource := Resources.Entries (I);
         begin
            case R.Kind is
               when RS.Device_Memory =>
                  if R.Index >= Unsigned_32 (Device.Count) then
                     return Refused (Not_Admitted, I);
                  elsif Result.Total = Window_Count'Last then
                     return Refused (Too_Many_Windows, I);
                  end if;
                  declare
                     K : constant Span_Index := Span_Index (R.Index + 1);
                     S : constant Span := Device.Items (K);
                     Bytes : constant Unsigned_64 :=
                       Unsigned_64'Min (Unsigned_64'Min (R.Amount, S.Length),
                                        RS.MAX_DEVICE_MEMORY_BYTES);
                  begin
                     if R.Rights /= RS.Read_Only and then not S.Writable then
                        return Refused (Span_Read_Only, I);
                     elsif Bytes = 0 then
                        --  A zero max_bytes never decodes; refuse it by name anyway.
                        return Refused (Not_Admitted, I);
                     end if;
                     Result.Total := Result.Total + 1;
                     Result.Windows (Result.Total) :=
                       (First => S.Base, Bytes => Bytes,
                        Read_Only => R.Rights = RS.Read_Only, From_Span => K);
                  end;
               when RS.Interrupt =>
                  if not Is_Parent_Line (R) or else Result.Parent_Line then
                     return Refused (Not_Parent_Line, I);
                  end if;
                  Result.Parent_Line := True;
               when RS.IO_Ports | RS.DMA =>
                  return Refused (Not_Device_Memory, I);
               when RS.Scheduling =>
                  null;
            end case;
         end;
      end loop;
      return Result;
   end Plan;

end Devmgr_ACPI_Windows;
