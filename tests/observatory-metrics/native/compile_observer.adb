with CuBit.Handles;
with Observatory_Metric_Observer;
procedure Compile_Observer is
   --  Compile-only: the observer instantiated over an endpoint handle.
   package Observer is new Observatory_Metric_Observer (CuBit.Handles.No_Endpoint);
begin
   Observer.Close;
end Compile_Observer;
