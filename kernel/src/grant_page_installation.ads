--  Serialized mapping transaction shared by owned and derived grant adapters.
--  The caller authenticates authority/range/permissions and reserves a grant
--  before entering. None of these callbacks may publish a grant identity.
--  Resolve_And_Pin must either return one independent frame pin, or fail with
--  no pin. Install must leave no mapping for its page on failure. Retire_Prefix
--  must remove every installed mapping and acknowledge remote TLB invalidation
--  before releasing its pins. Callback failures that violate these obligations
--  are fatal kernel errors, not recoverable transaction failures.
generic
   type Physical_Address is private;
   with procedure Resolve_And_Pin
     (Page : Natural; Physical : out Physical_Address;
      Success : out Boolean);
   with procedure Install
     (Page : Natural; Physical : Physical_Address;
      Success : out Boolean);
   with procedure Release_Unpublished (Physical : Physical_Address);
   with procedure Retire_Prefix (Pages : Positive);
procedure Grant_Page_Installation
  (Pages : Positive; Installed : out Natural; Success : out Boolean);
