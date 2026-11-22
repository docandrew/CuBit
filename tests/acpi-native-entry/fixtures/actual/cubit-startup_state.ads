------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The process's startup table, once loaded (CAP-002).
--
--  @description
--  The runtime's startup code loads the table its launcher installed
--  (CuBit.Startup_Tables) exactly once, before the program runs; the
--  generated Startup package then reads handles from it by name, through
--  CuBit.Capability_Handles, and its elaboration checks it against the
--  program's declaration. A table is loaded only if it decodes and is
--  complete (every entry Granted, or Absent and optional); a second Load
--  is refused, so nothing later can replace what the launcher gave.
--
--  Not yet wired: native startup does not call Load until procmgr installs
--  the table in the launch block (docs/capability-handles.md,
--  "Implementation notes (step 2)").
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Startup_Tables; use CuBit.Startup_Tables;

package CuBit.Startup_State
  with SPARK_Mode, Abstract_State => Loaded_Table, Initializes => Loaded_Table
is
   type Load_Result is (Loaded, Already_Loaded, Malformed, Incomplete);

   procedure Load (Raw : Bytes; Outcome : out Load_Result)
   with Global => (In_Out => Loaded_Table),
        Pre    => Raw'First = 1,
        Contract_Cases =>
          (Is_Loaded => (Outcome = Already_Loaded and Is_Loaded)
                        and Current = Current'Old,
           others    => Outcome /= Already_Loaded
                        and then Is_Loaded = (Outcome = Loaded));

   function Is_Loaded return Boolean
   with Global => Loaded_Table;

   --  The loaded table (Empty_Table before Load).
   function Current return Table
   with Global => Loaded_Table,
        Post   => Valid_Table (Current'Result) and then Complete (Current'Result);

   --  The Granted handle named Label with kind Kind, or No_Handle.
   function Granted (Label : String; Kind : Capability_Kind) return Unsigned_64
   with Global => Loaded_Table;

   ---------------------------------------------------------------------------
   --  Checking a program's declaration against the loaded table.
   ---------------------------------------------------------------------------
   type Expectation is record
      Label       : Name;
      Kind        : Capability_Kind := Endpoint;
      Requirement : Need := Required;
   end record;
   type Expectations is array (Positive range <>) of Expectation;

   --  Ready; Not_Loaded; or, for the first expectation that fails: Missing
   --  (no entry by that name), Wrong_Kind, Wrong_Need (declared required,
   --  table says optional or the reverse), Not_Granted (a required entry
   --  the table holds Absent; a valid table cannot, but the check is total).
   type Verification_Status is
     (Ready, Not_Loaded, Missing, Wrong_Kind, Wrong_Need, Not_Granted);
   type Verification is record
      Status : Verification_Status := Not_Loaded;
      Index  : Natural := 0;   --  the failing expectation, 0 when Ready
   end record;

   function Ready (Item : Verification) return Boolean is (Item.Status = Ready);

   function Verify (Expected : Expectations) return Verification
   with Global => Loaded_Table,
        Post   => (Verify'Result.Status = Ready) =
                  (Is_Loaded
                   and then (for all E of Expected =>
                               Find (Current, E.Label) /= 0
                               and then Current.Entries (Find (Current, E.Label)).Kind = E.Kind
                               and then Current.Entries (Find (Current, E.Label)).Requirement
                                        = E.Requirement
                               and then (E.Requirement = Optional
                                         or else Current.Entries (Find (Current, E.Label)).State
                                                 = Granted)));

end CuBit.Startup_State;
