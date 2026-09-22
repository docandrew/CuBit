pragma Ada_2022;
with Interfaces;
with CuBit.Window_Placement;

generic
   Maximum_Revision : Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
package CuBit.Output_Registry with SPARK_Mode, Pure is
   package W renames CuBit.Window_Placement;
   package L renames W.L;
   type Registry_Incarnation is new Interfaces.Unsigned_64
     range 1 .. Interfaces.Unsigned_64'Last;
   type Driver_Incarnation is new Interfaces.Unsigned_64
     range 1 .. Interfaces.Unsigned_64'Last;
   type Output_Number is new Interfaces.Unsigned_32;
   type Revision is new Interfaces.Unsigned_64;
   type Backend_Output is record
      Driver : Driver_Incarnation := 1;
      Number : Output_Number := 0;
   end record;
   type Connector_Presence is (Unknown, Absent, Present);
   type Power_Policy is (Enabled, Blanked, Disabled);
   type Readiness is (Discovering, Preparing, Ready, Retrying, Failed);
   type Description is record
      Backend : Backend_Output;
      Area : W.Work_Area;
      Presence : Connector_Presence := Unknown;
      Power : Power_Policy := Enabled;
      Stage : Readiness := Discovering;
   end record;
   function Presentable (Item : Description) return Boolean is
     (Item.Presence = Present and then Item.Power = Enabled and then
      Item.Stage = Ready);

   --  Owner supplies a fresh incarnation for each registry lifetime. It must
   --  come from authenticated session/lifetime state, never a monitor's EDID.
   type State (Instance : Registry_Incarnation) is private;
   type Output_Reference is private;
   No_Output : constant Output_Reference;
   type Mutation_Result is
     (Applied, Stale_Reference, Identity_Conflict, Full, Exhausted, Closed);
   function Is_Closed (Registry : State) return Boolean;
   function Version (Registry : State) return Revision;
   function Live (Registry : State; Reference : Output_Reference)
      return Boolean;
   function Describe (Registry : State; Reference : Output_Reference)
      return Description with Pre => Live (Registry, Reference);
   procedure Register
     (Registry : in out State; Item : Description;
      Reference : out Output_Reference; Result : out Mutation_Result)
     with Post =>
       (if Result = Applied then
          Live (Registry, Reference) and then
          Describe (Registry, Reference) = Item and then
          Version (Registry) > Version (Registry'Old)
        else Reference = No_Output) and then
       (if Result = Exhausted then Is_Closed (Registry)
        elsif Result /= Applied then Registry = Registry'Old);
   --  Every accepted update rotates this output reference, even if the payload
   --  is identical. Delayed notifications cannot overwrite newer observations.
   --  Identity changes require retirement/rebinding, not a forged update.
   procedure Update
     (Registry : in out State; Reference : Output_Reference;
      Item : Description; Replacement : out Output_Reference;
      Result : out Mutation_Result)
     with Post =>
       (if Result = Applied then
          not Live (Registry, Reference) and then
          Live (Registry, Replacement) and then
          Describe (Registry, Replacement) = Item and then
          Version (Registry) > Version (Registry'Old)
        else Replacement = No_Output) and then
       (if Result = Exhausted then Is_Closed (Registry)
        elsif Result /= Applied then Registry = Registry'Old);
   --  Retiring metadata is NOT permission to free scanout or DMA memory.
   procedure Retire
     (Registry : in out State; Reference : Output_Reference;
      Result : out Mutation_Result)
     with Post =>
       (if Result = Applied then
          not Live (Registry, Reference) and then
          Version (Registry) > Version (Registry'Old)) and then
       (if Result = Exhausted then Is_Closed (Registry)
        elsif Result /= Applied then Registry = Registry'Old);

   type Snapshot is private;
   function Areas (View : Snapshot) return W.Ready_Areas;
   function Reference_At (View : Snapshot; Index : L.Viewport_Index)
      return Output_Reference with Pre => Index <= Areas (View).Count;
   function Current (Registry : State; View : Snapshot) return Boolean;
   function Sound (Registry : State; View : Snapshot) return Boolean
     with Ghost;
   function Capture (Registry : State) return Snapshot
     with Post => Sound (Registry, Capture'Result) and then
       (if Is_Closed (Registry) then Areas (Capture'Result).Count = 0
        else Current (Registry, Capture'Result));
private
   function Valid (Registry : State) return Boolean with Ghost;
   type Output_Reference is record
      Instance : Registry_Incarnation := 1;
      Slot : L.Viewport_Index := 1;
      Stamp : Revision := 0;
   end record;
   No_Output : constant Output_Reference := (others => <>);
   type Output_Entry is record
      Occupied : Boolean := False;
      Stamp : Revision := 0;
      Item : Description;
   end record;
   type Entry_Array is array (L.Viewport_Index) of Output_Entry;
   type State (Instance : Registry_Incarnation) is record
      Serial : Revision := 0;
      Closing : Boolean := False;
      Entries : Entry_Array := [others => <>];
   end record with Type_Invariant => Valid (State);
   function Valid (Registry : State) return Boolean is
     (for all Item of Registry.Entries =>
        (if Item.Occupied then
           Item.Stamp > 0 and then Item.Stamp <= Registry.Serial));
   type Reference_Array is array (L.Viewport_Index) of Output_Reference;
   type Snapshot is record
      Instance : Registry_Incarnation := 1;
      Serial : Revision := 0;
      Visible : W.Ready_Areas;
      References : Reference_Array := [others => <>];
   end record;
   function Is_Closed (Registry : State) return Boolean is (Registry.Closing);
   function Version (Registry : State) return Revision is (Registry.Serial);
   function Live (Registry : State; Reference : Output_Reference)
      return Boolean is
     (not Registry.Closing and then Reference.Instance = Registry.Instance
      and then Reference.Stamp /= 0 and then
      Registry.Entries (Reference.Slot).Occupied and then
      Registry.Entries (Reference.Slot).Stamp = Reference.Stamp);
   function Describe (Registry : State; Reference : Output_Reference)
      return Description is (Registry.Entries (Reference.Slot).Item);
   function Areas (View : Snapshot) return W.Ready_Areas is (View.Visible);
   function Reference_At (View : Snapshot; Index : L.Viewport_Index)
      return Output_Reference is (View.References (Index));
   function Current (Registry : State; View : Snapshot) return Boolean is
     (not Registry.Closing and then Registry.Instance = View.Instance and then
      Registry.Serial = View.Serial);
end CuBit.Output_Registry;
