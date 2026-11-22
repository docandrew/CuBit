pragma Ada_2022;
with GPIO_Loop_Time;
with CuBit.Messages;
package GPIO_Native_Wait is
   procedure Await (Plan : GPIO_Loop_Time.Wait_Plan; Fallback : GPIO_Loop_Time.Duration;
      Result : out CuBit.Messages.Activity_Result);
   -- Caller handles terminal/fault reports before waiting. Run between bounded
   -- turns, never inside a controller/RMW operation.
   -- Explicit Wait_Forever only for idle activity; deadlines/relative delays
   -- are finite. Unavailable wait attempts one bounded relative fallback and
   -- remains Unavailable for the caller to diagnose/supervise (no hidden loop).
end GPIO_Native_Wait;
