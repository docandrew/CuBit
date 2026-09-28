with Interfaces;

-- Port access is supplied by the native driver or a hosted hardware fixture.
generic
   with function Read_Port (Port : Interfaces.Unsigned_16)
     return Interfaces.Unsigned_8;
package PS2_Boot_Probe is
   type Probe_Result is (Quiescent, Controller_Unavailable, Drain_Limit);
   Max_Stale_Bytes : constant := 256;
   procedure Drain (Result : out Probe_Result);
end PS2_Boot_Probe;
