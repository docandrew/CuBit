pragma Ada_2022;
with Interfaces; use Interfaces;
-- Caller serializes an owner-incarnation's ledger. This unit accounts pages;
-- it does not establish authority, allocation identity or retirement evidence.
package Process_Memory_Budget with SPARK_Mode, Pure is
   type Charge_Kind is (Ordinary, DMA_Backing, Metadata, Retained_DMA_Backing);
   type Ledger is private with Default_Initial_Condition =>
     Used (Ledger) = 0 and Limit (Ledger) = 0;
   function Used (Object : Ledger) return Unsigned_64;
   function Charged (Object : Ledger; Kind : Charge_Kind) return Unsigned_64;
   function Limit (Object : Ledger) return Unsigned_64;
   -- Zero preserves the existing unlimited-quota meaning (arithmetic remains
   -- bounded). Failed adoption never changes policy or releases backing.
   procedure Adopt (Object : in out Ledger; Pages : Unsigned_64; OK : out Boolean)
     with Post => Used (Object) = Used (Object'Old) and then
       OK = (Pages = 0 or else Used (Object'Old) <= Pages) and then
       Limit (Object) = (if OK then Pages else Limit (Object'Old)) and then
       (if not OK then Object = Object'Old);
   procedure Reserve
     (Object : in out Ledger; Kind : Charge_Kind; Pages : Unsigned_64;
      OK : out Boolean)
     with Post =>
       (if OK then Used (Object) = Used (Object'Old) + Pages and then
          Charged (Object, Kind) = Charged (Object'Old, Kind) + Pages
        else Object = Object'Old);
   -- Only after unpublished rollback or confirmed physical retirement;
   -- owner death and a GPU timeout do not authorize Release.
   procedure Release
     (Object : in out Ledger; Kind : Charge_Kind; Pages : Unsigned_64;
      OK : out Boolean)
     with Post =>
       (if OK then Used (Object) = Used (Object'Old) - Pages and then
          Charged (Object, Kind) = Charged (Object'Old, Kind) - Pages
        else Object = Object'Old);
private
   type Charges is array (Charge_Kind) of Unsigned_64;
   type Ledger is record
      Counts : Charges := [others => 0];
      Total : Unsigned_64 := 0;
      Ceiling : Unsigned_64 := 0;
   end record;
end Process_Memory_Budget;
