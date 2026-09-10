with Interfaces;
with CCL.Language;
with CCL.Periodic_Programs;

-- Native trusted binding adapter, not part of the parser/compiler or wire
-- codec. No application-supplied PID, binding number, or endpoint slot.
package Control_Host is
   procedure Initialize (Success : out Boolean);
   procedure Read_Clock (Available : out Boolean; Value : out Interfaces.Unsigned_64);
   procedure Evaluate (Source : String; Result : out CCL.Language.Interpretation_Result);
   procedure Start_Monitor (Source : String; Accepted : out Boolean);
   procedure Stop_Monitor (Identity : Interfaces.Unsigned_64; Accepted : out Boolean);
   procedure Pump;
   function Monitor return CCL.Periodic_Programs.Program;
   function Next_Deadline return Interfaces.Unsigned_64;
end Control_Host;
