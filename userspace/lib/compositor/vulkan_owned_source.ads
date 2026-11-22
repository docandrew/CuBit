with System;
with Vulkan_Image_Owner;
with Vulkan_Context_Owner;
with Vulkan_Submission;
-- Own sampled-image backing before it is imported into the drawing provider.
-- The matching ledger, private request and reader-retirement observation are
-- trusted inputs. Never copy/reset an owner; Initialize supports closed reuse.
package Vulkan_Owned_Source with SPARK_Mode is
   package I renames Vulkan_Image_Owner;
   package A renames I.Accounting;
   package C renames Vulkan_Context_Owner;
   package V renames Vulkan_Submission;
   use type I.Phase, A.State, C.State, C.Phase, C.Child, System.Address;
   type State is private;
   function Current (S : State) return I.Phase;
   function Parent_Held (S : State; Context : C.State) return Boolean;
   function Description (S : State) return System.Address;
   function Lease (S : State) return A.Ticket;
   procedure Initialize
     (S : in out State; Context : in out C.State; Submission : V.State;
      Request : System.Address; Budget : in out A.State; Allowed_Types : I.U32;
      Accepted : out Boolean)
     with Pre => A.Valid (Budget),
       Post => A.Valid (Budget) and A.Limit (Budget) = A.Limit (Budget'Old) and
         C.Current (Context) = C.Current (Context'Old) and
         C.Context (Context) = C.Context (Context'Old) and
         (if Accepted then Current (S) = I.Live and Parent_Held (S, Context) and
                            Description (S) = Request) and
         (if Current (S'Old) in I.Fresh | I.Closed and
             Current (S) in I.Live | I.Quarantined then Parent_Held (S, Context)) and
         (if Parent_Held (S'Old, Context'Old) then Parent_Held (S, Context));
   -- All_Readers_Retired must cover upload/drawing work and successful descriptor
   -- retirement. Completion alone is insufficient while a descriptor remains.
   -- False makes no foreign calls or state changes; an uncertain release retains
   -- both backing charge and the context dependency.
   procedure Close
     (S : in out State; Context : in out C.State; Budget : in out A.State;
      All_Readers_Retired : Boolean; Released : out Boolean)
     with Pre => A.Valid (Budget),
       Post => A.Valid (Budget) and A.Limit (Budget) = A.Limit (Budget'Old) and
         C.Current (Context) = C.Current (Context'Old) and
         C.Context (Context) = C.Context (Context'Old) and
         (if not All_Readers_Retired then
            not Released and S = S'Old and Context = Context'Old and Budget = Budget'Old) and
         (if Released then Current (S) = I.Closed and not Parent_Held (S, Context)) and
         (if Parent_Held (S'Old, Context'Old) and not Released then Parent_Held (S, Context));
private
   type State is record
      Image : I.State;
      Parent : C.Child := C.No_Child;
      Request : System.Address := System.Null_Address;
   end record;
   function Current (S : State) return I.Phase is (I.Status (S.Image));
   function Parent_Held (S : State; Context : C.State) return Boolean is
     (C.Held (Context, S.Parent));
   function Description (S : State) return System.Address is
     (if Current (S) = I.Live then S.Request else System.Null_Address);
   function Lease (S : State) return A.Ticket is (I.Lease (S.Image));
end Vulkan_Owned_Source;
