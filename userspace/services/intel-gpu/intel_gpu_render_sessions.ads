with Interfaces; use Interfaces;
package Intel_GPU_Render_Sessions with SPARK_Mode is
   -- One serialized registry for one GPU service endpoint incarnation.
   -- Only trusted admission code reserves/finalizes sessions. A request
   -- handler may Resolve using kernel-returned sender and authorityTag, never
   -- a tag/session supplied in message words. No kernel minting occurs here.
   Capacity : constant := 16;
   Tag_Base : constant Unsigned_64 := 16#4750_0000_0000_0000#;
   type Registry is limited private;
   procedure Reserve
     (Object : in out Registry; Sender : Unsigned_64; Tag : out Unsigned_64)
     with Post => Tag = 0 or else
       (Sender /= 0 and Tag > Tag_Base and Tag <= Tag_Base + Capacity);
   -- Reserved tags do not resolve. After trusted grant completion, finalize
   -- exactly once; failed/uncertain grants retire the tag permanently.
   procedure Finalize
     (Object : in out Registry; Sender, Tag : Unsigned_64;
      Granted : Boolean; Accepted : out Boolean);
   function Resolve
     (Object : Registry; Sender, Stamped_Tag : Unsigned_64) return Unsigned_64
     with Post => (Resolve'Result = 0 or else
       (Sender /= 0 and Resolve'Result = Stamped_Tag and
        Stamped_Tag > Tag_Base and Stamped_Tag <= Tag_Base + Capacity));
   procedure Close (Object : in out Registry; Sender, Tag : Unsigned_64);
   procedure Quarantine (Object : in out Registry);
   -- Sender alone never resolves an old session. PID reuse with a newly
   -- minted tag cannot recover earlier names. No tag/slot reuse or reset.
private
   type Phase is (Reserved, Active, Retired);
   type Entry_State is record
      Sender : Unsigned_64 := 0;
      State : Phase := Retired;
   end record;
   type Entries is array (Positive range 1 .. Capacity) of Entry_State;
   type Registry is limited record
      Used : Natural range 0 .. Capacity := 0;
      Failed : Boolean := False;
      Items : Entries;
   end record;
end Intel_GPU_Render_Sessions;
