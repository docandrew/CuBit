
package body CCL.Control with SPARK_Mode is
   procedure Execute
     (Session : in out CCL.Sessions.Session; Op : Operation;
      Source : String; Host : Observation; Result : out Response) is
   begin
      Result := (Observed => Host, others => <>);
      case Op is
         when Inspect_Bindings | Read_Clock | Read_Image_Rows | Present_Monitor => null;
         when Complete_Expression =>
            CCL.Sessions.Complete_At (Session, Source, ' ', Result.Completion);
         when Evaluate_Expression | Present_Expression =>
            CCL.Sessions.Submit (Session, Source, CCL.Sessions.Default_Fuel, Result.Outcome);
         when Start_Monitor | Stop_Monitor | Inspect_Monitor =>
            null; -- The owning host must explicitly provide a periodic slot.
      end case;
   end Execute;
end CCL.Control;
