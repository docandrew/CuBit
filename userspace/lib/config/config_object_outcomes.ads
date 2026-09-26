with Interfaces;
with CCL.Catalog;
with CCL.Objects;
with CCL.Types;
with CCL.VM;
with CCL.Host_Values;
with Config_Object_Messages;

-- Public data schema, not an authority source or a new language primitive.
-- The declaration is config-write-outcome.schema; Key is its SHA-256, split
-- into four big-endian words. Provider identity still comes from authority.
package Config_Object_Outcomes with SPARK_Mode is
   Key : constant CCL.Objects.Schema_Key :=
     [16#DD59CE4691502DA7#, 16#16720C86A84D1E3A#,
      16#F3D4E732EF8ED37D#, 16#B386C79A5D6C91A7#];
   type Write_Alternative is
     (Committed, Invalid_Request, Denied, Busy, Unavailable, Conflict, Rejected, Uncertain);
   for Write_Alternative use
     (Committed => 1, Invalid_Request => 2, Denied => 3, Busy => 4,
      Unavailable => 5, Conflict => 6, Rejected => 7, Uncertain => 8);
   function Name (Choice : Write_Alternative) return String is
     (case Choice is when Committed => "Committed", when Invalid_Request => "InvalidRequest",
      when Denied => "Denied", when Busy => "Busy", when Unavailable => "Unavailable",
      when Conflict => "Conflict", when Rejected => "Rejected", when Uncertain => "Uncertain");
   function Schema return CCL.Objects.Binding;
   procedure Publish (Catalog : in out CCL.Catalog.Interface_Catalog; Accepted : out Boolean);
   procedure To_VM
     (Local_Types : CCL.Types.Registry; Transport_Valid : Boolean;
      Code : Config_Object_Messages.Status; Revision : Interfaces.Unsigned_64;
      Value : out CCL.VM.Value; Accepted : out Boolean);
   procedure To_Host
     (Transport_Valid : Boolean; Code : Config_Object_Messages.Status;
      Revision : Interfaces.Unsigned_64; Reply : out CCL.Host_Values.Call_Result);
   -- Only Committed carries a revision. A malformed or ambiguous receipt maps
   -- to Uncertain, never a definite rejection; retry may duplicate a write.
   -- The target registry must contain the complete approved nominal definition,
   -- not merely a matching local type number or the ConfigWrite name.
end Config_Object_Outcomes;
