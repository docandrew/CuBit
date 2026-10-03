with System; with Vulkan_Image_Owner; with Vulkan_Context_Owner;
package Vulkan_Upload_Owner with SPARK_Mode is
   package A renames Vulkan_Image_Owner.Accounting;
   package C renames Vulkan_Context_Owner;
   package V renames C.V;
   use type A.State, A.Ticket, A.Phase, C.State, C.Child, C.Phase, System.Address;
   subtype Capacity_Range is Positive range 1 .. 16 * 1024 * 1024;
   type Phase is (Fresh, Live, Closed, Quarantined);
   type State is private;
   function Current (S : State) return Phase;
   function Parent_Held (S : State; Context : C.State) return Boolean;
   function Lease (S : State) return A.Ticket;
   function Capacity (S : State) return Natural;
   -- Private mapping only; presence is not permission to write while submitted.
   -- Upload scheduling must exclude CPU writes until all GPU readers retire.
   function Mapping (S : State) return System.Address;
   function Description (S : State) return System.Address;
   procedure Initialize (S : in out State; Context : in out C.State;
      Submission : V.State; Request : System.Address; Size : Capacity_Range;
      Budget : in out A.State; Accepted : out Boolean)
     with Pre => A.Valid (Budget),
       Post => A.Valid (Budget) and A.Limit (Budget) = A.Limit (Budget'Old) and
         C.Current (Context) = C.Current (Context'Old) and C.Context (Context) = C.Context (Context'Old) and
         (if Accepted then Current (S) = Live and Parent_Held (S, Context) and
            Capacity (S) = Size and Mapping (S) /= System.Null_Address and
            A.Current (Budget, Lease (S)) and A.Status (Budget, Lease (S)) = A.Live) and
         (if Current (S'Old) in Fresh | Closed and Current (S) in Live | Quarantined then Parent_Held (S, Context)) and
         (if Parent_Held (S'Old, Context'Old) then Parent_Held (S, Context));
   procedure Close (S : in out State; Context : in out C.State;
      Budget : in out A.State; Readers_Retired : Boolean; Released : out Boolean)
     with Pre => A.Valid (Budget),
       Post => A.Valid (Budget) and A.Limit (Budget) = A.Limit (Budget'Old) and
         C.Current (Context) = C.Current (Context'Old) and C.Context (Context) = C.Context (Context'Old) and
         (if not Readers_Retired then not Released and S = S'Old and Context = Context'Old and Budget = Budget'Old) and
         (if Released then Current (S) = Closed and not Parent_Held (S, Context) and not A.Current (Budget, Lease (S))) and
         (if Parent_Held (S'Old, Context'Old) and not Released then Parent_Held (S, Context));
private
   type State is record
      Mode : Phase := Fresh;
      Request, Mapped : System.Address := System.Null_Address;
      Size : Natural := 0;
      Ticket : A.Ticket := A.No_Ticket;
      Parent : C.Child := C.No_Child;
   end record;
   function Current (S : State) return Phase is (S.Mode);
   function Parent_Held (S : State; Context : C.State) return Boolean is (C.Held (Context, S.Parent));
   function Lease (S : State) return A.Ticket is (S.Ticket);
   function Capacity (S : State) return Natural is (if S.Mode = Live then S.Size else 0);
   function Description (S : State) return System.Address is
     (if S.Mode = Live then S.Request else System.Null_Address);
   function Mapping (S : State) return System.Address is
     (if S.Mode = Live then S.Mapped else System.Null_Address);
end Vulkan_Upload_Owner;
