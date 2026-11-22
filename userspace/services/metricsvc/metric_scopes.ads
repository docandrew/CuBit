pragma Ada_2022;
with Interfaces; use Interfaces;
private with Heap_Arrays;

--  What each process may read from metrics.svc (MET-002): the read scopes
--  its manifest declared, (metrics-scope (rights read) "desktop.") or
--  (Scope.Metrics ...), as procmgr installs them (Set_Scopes) at launch and
--  clears them for a reused process number (Revoke_Scopes). Holding the
--  metrics-observer role is not enough: a series outside every scope of the
--  reader is refused (CuBit.Metric_Protocol.Out_Of_Scope) and the refusal
--  is logged. An empty prefix is every series (Scope_Place.Everything).
--
--  The scope names series by their name's prefix. It is kept as the
--  reader's subscription capability, so a later rewiring can retarget it
--  (narrow the prefix, add a source) without the reader asking again.
package Metric_Scopes is
   Maximum_Processes : constant := 4_096;
   Maximum_Scopes : constant := 16;
   Maximum_Prefix : constant := 64;

   type Table is limited private;

   --  Process's scopes, replacing any it had. Accepted False: a malformed
   --  entry (bad right, prefix) or no room; nothing changes then.
   type Prefix_Text is record
      Text : String (1 .. Maximum_Prefix) := [others => ' '];
      Length : Natural range 0 .. Maximum_Prefix := 0;
   end record;
   type Prefix_List is array (Positive range <>) of Prefix_Text;
   procedure Set (T : in out Table; Process : Unsigned_64; Prefixes : Prefix_List; Accepted : out Boolean)
   with Pre => Prefixes'Length <= Maximum_Scopes;
   procedure Revoke (T : in out Table; Process : Unsigned_64);

   --  Whether Process may read the series named Name; and whether it may
   --  read every series whose name starts with Prefix (a selection).
   function May_Read (T : Table; Process : Unsigned_64; Name : String) return Boolean;
   function May_Read_Prefix (T : Table; Process : Unsigned_64; Prefix : String) return Boolean;
   --  Whether Process holds any read scope.
   function Has_Any (T : Table; Process : Unsigned_64) return Boolean;
private
   type Prefix_Array is array (1 .. Maximum_Scopes) of Prefix_Text;
   type Entry_Record is record
      Process : Unsigned_64 := 0;
      Count : Natural range 0 .. Maximum_Scopes := 0;
      Prefixes : Prefix_Array;
   end record;
   package Entry_Blocks is new Heap_Arrays (Entry_Record);
   type Table is limited record
      Entries : Entry_Blocks.Block := Entry_Blocks.Empty;
      Used : Natural := 0;
   end record;
end Metric_Scopes;
