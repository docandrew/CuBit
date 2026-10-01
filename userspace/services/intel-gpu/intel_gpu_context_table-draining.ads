generic
   with function Now_Us return Unsigned_64;
package Intel_GPU_Context_Table.Draining is
   type Drain_State is limited private;
   -- One bounded pass; caller continues dispatching CT events between ticks.
   -- No memory/ID reclamation, including on disable completion. Frozen/bad
   -- clocks, timeout and uncertain sends quarantine, never retry publication.
   procedure Tick (Object : in out Table; Drain : in out Drain_State;
                   New_Fault : out Boolean);
private
   type Progress is record
      Started, Finished : Boolean := False;
      First, Previous : Unsigned_64 := 0;
      Polls : Natural range 0 .. 100_000 := 0;
   end record;
   type Progress_Array is array (Positive range 1 .. Capacity) of Progress;
   type Drain_State is limited record
      Items : Progress_Array;
   end record;
end Intel_GPU_Context_Table.Draining;
