with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Backing;
package Intel_GPU_Budget_Query with SPARK_Mode is
   -- Serialized event-loop transaction. Caller submits request0238 on the
   -- pinned supervisor endpoint, using Token, then routes kernel completions.
   -- No waiting, retries, allocation or endpoint mutation occurs here.
   package B renames Intel_GPU_Buffer_Backing;
   type Query is limited private;
   procedure Start (Object : in out Query; Now : Unsigned_64;
                    Owner : Boolean; Token : out Unsigned_64);
   procedure Tick (Object : in out Query; Now : Unsigned_64; Owner : Boolean);
   procedure Cancel (Object : in out Query);
   procedure Complete
     (Object : in out Query; Token, Now : Unsigned_64; Owner, Transport_OK : Boolean;
      Label : Unsigned_32; Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Data : B.Budget_Words; Consumed : out Boolean);
   function Pending (Object : Query) return Boolean;
   function Result (Object : Query) return B.Budget_Snapshot;
   -- Each token is issued at most once by this instance. Retain the instance
   -- for the whole endpoint lifetime; do not reset it while old replies exist.
private
   Token_Base : constant Unsigned_64 := 16#4947_8000_0000_0000#;
   Timeout : constant Unsigned_64 := 30_000;
   type Query is limited record
      Active : Boolean := False;
      Serial : Unsigned_32 := 0;
      Started, Previous : Unsigned_64 := 0;
      Value : B.Budget_Snapshot;
   end record;
end Intel_GPU_Budget_Query;
