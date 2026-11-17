pragma Ada_2022;
package body ACPI_Event_Waits with SPARK_Mode is
   procedure Run (Milliseconds : AML_Events.Timeout; Result : out Report) is
      Remaining : AML_Events.Timeout := Milliseconds;
      Outcome : AML_Delays.Outcome;
   begin
      Result := (others => <>);
      if Milliseconds in AML_Events.Poll_Only | AML_Events.Wait_Forever then return; end if;
      while Remaining > 0 loop
         pragma Loop_Invariant
           (Result.Completed_Milliseconds <= Milliseconds
            and then Remaining = Milliseconds - Result.Completed_Milliseconds);
         pragma Loop_Variant (Decreases => Remaining);
         if Result.Attempts = Chunk_Count'Last then
            Result.Code := AML_Events.Failed; return;
         end if;
         declare
            Chunk : constant AML_Delays.Sleep_Milliseconds :=
              AML_Delays.Sleep_Milliseconds (AML_Events.Timeout'Min
                (Remaining, AML_Events.Timeout (Chunk_Milliseconds)));
         begin
            Result.Attempts := Result.Attempts + 1;
            Perform ((Kind => AML_Delays.Sleep_Delay, Milliseconds => Chunk), Outcome);
            case Outcome is
               when AML_Delays.Completed =>
                  Result.Completed_Milliseconds :=
                    Result.Completed_Milliseconds + AML_Events.Timeout (Chunk);
                  Remaining := Remaining - AML_Events.Timeout (Chunk);
               when AML_Delays.Unavailable => Result.Code := AML_Events.Unavailable; return;
               when AML_Delays.Failed => Result.Code := AML_Events.Failed; return;
            end case;
         end;
      end loop;
      Result.Code := AML_Events.Elapsed;
   end Run;
end ACPI_Event_Waits;
