-- Hosted owner-boundary model only; no native authority or AML execution.
with Interfaces;
package Producer_Model is
   Mode, Opens, Ends, Collections, Checks : Natural := 0;
   Submitted_Reads, History_Reads : Natural := 0;
   Window_Open, Pending : Boolean := False;
   Boundary, History_Complete : Boolean := True;
   Original_US : Interfaces.Unsigned_64 := 5_000_000;
   procedure Check (OK : Boolean; Why : String);
end Producer_Model;
