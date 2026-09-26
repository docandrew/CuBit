with Interfaces;
with CCL.Types;
with CCL.Objects;
with Config_Object_Messages;

-- Ordinary CCL products/sums, specialized to an approved stored value type.
-- Names and schema identities are supplied by the authorized host/catalog;
-- constructing metadata does not authenticate a publisher or grant access.
package Config_Read_Outcomes with SPARK_Mode is
   type Alternative is
     (Found, Stale, Missing, Denied, Busy, Unavailable, Schema_Mismatch,
      Invalid_Request, Invalid_Completion);
   for Alternative use
     (Found => 1, Stale => 2, Missing => 3, Denied => 4, Busy => 5,
      Unavailable => 6, Schema_Mismatch => 7, Invalid_Request => 8,
      Invalid_Completion => 9);
   function Name (Choice : Alternative) return String is
     (case Choice is
        when Found => "Found", when Stale => "Stale", when Missing => "Missing",
        when Denied => "Denied", when Busy => "Busy", when Unavailable => "Unavailable",
        when Schema_Mismatch => "SchemaMismatch", when Invalid_Request => "InvalidRequest",
        when Invalid_Completion => "InvalidCompletion");

   type Description is private;
   procedure Define
     (Value_Type : CCL.Objects.Binding;
      Snapshot_Name, Result_Name : CCL.Types.Name;
      Result_Key : CCL.Objects.Schema_Key;
      Item : out Description; Accepted : out Boolean);
   -- Snapshot = (revision: Integer, value: T).
   -- Result = Found(Snapshot) | Stale(Snapshot) | Missing | Denied | ...
   -- No value is created and no registry is changed on failure. All branches
   -- must fit the ordinary CCL object budgets, including the envelope cells.
   function Is_Defined (Item : Description) return Boolean;
   function Schema (Item : Description) return CCL.Objects.Binding;
   function Matches (Item : Description; Value_Type : CCL.Objects.Binding) return Boolean;
   procedure Build
     (Item : Description; Transport_Valid : Boolean;
      Code : Config_Object_Messages.Status; Revision : Interfaces.Unsigned_64;
      Value : CCL.Objects.Image;
      Output : out CCL.Objects.Image; Accepted : out Boolean)
     with Post => (if Accepted then CCL.Objects.Validate (Output, Schema (Item)));
   -- Failed/malformed completion becomes InvalidCompletion, never Missing or
   -- a made-up default value. Found/Stale carry the exact value and revision.
   -- Owned native objects throughout: no CBOR/SQL/string conversion here.
private
   type Description is record
      Value_Type, Result_Type : CCL.Objects.Binding;
      Defined : Boolean := False;
   end record;
   function Is_Defined (Item : Description) return Boolean is (Item.Defined);
   function Schema (Item : Description) return CCL.Objects.Binding is (Item.Result_Type);
end Config_Read_Outcomes;
