package body ACPI_Sample_Cache with SPARK_Mode is
   procedure Clear (Cache : in out State; New_Status : Cache_Status)
     with Post => Revision (Cache) = Revision (Cache)'Old
       and then Status (Cache) = New_Status
       and then Stored_Value (Cache) = Empty_Sample
       and then Stored_Time (Cache) = (Available => False, Value => 0);
   procedure Clear (Cache : in out State; New_Status : Cache_Status) is
   begin
      Cache.Current := New_Status;
      Cache.Value := Empty_Sample;
      Cache.Sample_Time := (others => <>);
   end Clear;
   procedure Invalidate (Cache : in out State; Result : out Invalidation_Result) is
   begin
      if Cache.Current = Exhausted or else Cache.Version = Revision_Number'Last then
         Clear (Cache, Exhausted); Result := Counter_Exhausted; return;
      end if;
      Cache.Version := Cache.Version + 1;
      Clear (Cache, Pending);
      Result := Invalidated;
   end Invalidate;
   procedure Publish (Cache : in out State; Expected : Revision_Number;
      Kind : Publication; Value : Sample_Type; At_Time : Clock_Reading;
      Result : out Publication_Result) is
   begin
      if Cache.Current = Exhausted then Result := Counter_Exhausted; return; end if;
      if Expected /= Cache.Version then Result := Stale_Revision; return; end if;
      if Cache.Current /= Pending then Result := Not_Pending; return; end if;
      if Cache.Version = Revision_Number'Last then
         Clear (Cache, Exhausted); Result := Counter_Exhausted; return;
      end if;
      Cache.Version := Cache.Version + 1;
      case Kind is
         when Sampled => Cache.Current := Ready; Cache.Value := Value;
         when Known_Absent => Cache.Current := Absent; Cache.Value := Empty_Sample;
         when Acquisition_Failed => Cache.Current := Failed; Cache.Value := Empty_Sample;
      end case;
      Cache.Sample_Time := (Available => At_Time.Available,
                            Value => (if At_Time.Available then At_Time.Value else 0));
      Result := Published;
   end Publish;
   function Read (Cache : State; Now : Clock_Reading; Maximum_Age : Tick) return View is
      Result : View := (Revision => Cache.Version, Time => Cache.Sample_Time, others => <>);
   begin
      case Cache.Current is
         when Never_Sampled | Pending => return Result;
         when Exhausted => Result.Status := Counter_Exhausted; return Result;
         when Failed => Result.Status := Failure; return Result;
         when Ready | Absent => null;
      end case;
      if not Now.Available or else not Cache.Sample_Time.Available then
         Result.Status := Clock_Unknown;
      elsif Now.Value < Cache.Sample_Time.Value then
         Result.Status := Clock_Regressed;
      elsif Now.Value - Cache.Sample_Time.Value > Maximum_Age then
         Result.Status := Too_Old;
      elsif Cache.Current = Absent then
         Result.Status := Missing;
      else
         Result.Status := Available;
         Result.Value := Cache.Value;
      end if;
      return Result;
   end Read;
end ACPI_Sample_Cache;
