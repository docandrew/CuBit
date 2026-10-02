with Interfaces; use Interfaces;
with Hardware_Authority;
-- Kernel-owned inventory metadata, not a syscall and not authority supplied by
-- callers. Platform discovery must independently admit backing before Add.
-- Serialize mutations; actual backing/in-flight I/O lifetimes are external.
package Hardware_Catalog with SPARK_Mode is
   Maximum_Resources : constant := 64;
   type Resource_Group is (ACPI, GPIO);
   type Address_Space is (Memory_Space, IO_Space);
   subtype Access_Bytes is Positive range 1 .. 8;
   type Descriptor is record
      Permission : Hardware_Authority.Permission;
      Group : Resource_Group := ACPI;
      Space : Address_Space := Memory_Space;
      Address : Unsigned_64 := 0;
      Width : Access_Bytes := 1;
   end record;
   function Valid (Item : Descriptor) return Boolean;
   type State is private;
   function Active (S : State) return Boolean;
   function Epoch (S : State) return Unsigned_64;
   function Busy (S : State) return Boolean;
   function Receipt (S : State) return Unsigned_64;
   function Ready_To_Release (S : State) return Boolean is
     (not Active (S) and then not Busy (S));
   function Count (S : State) return Natural;
   function Resource_At (S : State; Index : Positive) return Descriptor with
     Pre => Index <= Count (S);
   -- Begin a replacement inventory only after revocation. Epoch never wraps.
   -- Old backing must remain retained separately until in-flight users finish.
   procedure Begin_Inventory (S : in out State; Accepted : out Boolean) with
     Post => (if Accepted then not Active (S) and then Count (S) = 0
       and then Epoch (S'Old) < Unsigned_64'Last
       and then Epoch (S) = Epoch (S'Old) + 1 else S = S'Old)
       and then (if Busy (S'Old) then not Accepted and then S = S'Old);
   procedure Add (S : in out State; Item : Descriptor; Accepted : out Boolean) with
     Post => Epoch (S) = Epoch (S'Old) and then Active (S) = Active (S'Old)
       and then (if Accepted then not Active (S) and then Valid (Item)
         and then Count (S) = Count (S'Old) + 1
         and then Resource_At (S, Count (S)) = Item
         and then (for all I in 1 .. Count (S'Old) => Resource_At (S, I) = Resource_At (S'Old, I))
       else S = S'Old);
   procedure Seal (S : in out State; Accepted : out Boolean) with
     Post => Epoch (S) = Epoch (S'Old) and then Count (S) = Count (S'Old)
       and then (if Accepted then Active (S) else S = S'Old);
   procedure Revoke (S : in out State) with
     Post => not Active (S) and then Epoch (S) = Epoch (S'Old)
       and then Count (S) = Count (S'Old)
       and then Busy (S) = Busy (S'Old) and then Receipt (S) = Receipt (S'Old)
       and then (for all I in 1 .. Count (S) => Resource_At (S, I) = Resource_At (S'Old, I));
   function Contains (S : State; Group : Resource_Group; ID : Unsigned_64)
     return Boolean;
   -- Group/Epoch here must come from a kernel-authenticated authority object,
   -- not user claims. This predicate checks membership and narrowing only;
   -- cspace installation, parent revocation and caller authentication are not
   -- implemented by passing a record to this function.
   function Can_Select
     (S : State; Token : Unsigned_64; Group : Resource_Group;
      Child : Hardware_Authority.Permission) return Boolean with
     Post => (if Can_Select'Result then Active (S) and then Token = Epoch (S)
       and then Contains (S, Group, Child.Resource_ID));
   type Decision (Allowed : Boolean := False) is record
      case Allowed is
         when True => Resource : Descriptor;
         when False => null;
      end case;
   end record;
   -- Scope/Token must be obtained from authenticated kernel cspace state.
   -- A wire request supplies only ID/op/value to the eventual syscall; it may
   -- not manufacture this Permission. Result stays inside the kernel. Hold
   -- the catalog/backing stable through the actual transaction; this readonly
   -- lookup is not a lifetime reservation or an I/O operation.
   function Resolve
     (S : State; Token : Unsigned_64; Scope : Hardware_Authority.Permission;
      For_Write : Boolean; Value : Unsigned_64) return Decision with
     Post => (if Resolve'Result.Allowed then Active (S) and then Token = Epoch (S)
       and then Valid (Resolve'Result.Resource)
       and then Hardware_Authority.Is_Subset (Resolve'Result.Resource.Permission, Scope)
       and then Hardware_Authority.Permits
         (Scope, Resolve'Result.Resource.Permission.Resource_ID, For_Write, Value)
       and then Hardware_Authority.Permits
         (Resolve'Result.Resource.Permission, Scope.Resource_ID, For_Write, Value)
       and then (for some I in 1 .. Count (S) =>
         Resource_At (S, I) = Resolve'Result.Resource));
   -- One serialized in-flight transaction. Revoke blocks new reservations but
   -- preserves the inventory until the matching trusted completion. Unknown
   -- hardware completion must remain busy; tickets never travel over IPC.
   procedure Begin_Access
     (S : in out State; Token : Unsigned_64; Scope : Hardware_Authority.Permission;
      For_Write : Boolean; Value : Unsigned_64;
      Result : out Decision; Ticket : out Unsigned_64) with
     Pre => not Result'Constrained,
     Post => Epoch (S) = Epoch (S'Old) and then Active (S) = Active (S'Old)
       and then Count (S) = Count (S'Old)
       and then (for all I in 1 .. Count (S) => Resource_At (S, I) = Resource_At (S'Old, I))
       and then (if Result.Allowed then not Busy (S'Old) and then Busy (S)
         and then Result = Resolve (S'Old, Token, Scope, For_Write, Value)
         and then Ticket = Receipt (S) and then Ticket /= 0
         and then Receipt (S'Old) < Unsigned_64'Last
         and then Receipt (S) = Receipt (S'Old) + 1
       else S = S'Old and then Ticket = 0);
   procedure Finish_Access
     (S : in out State; Ticket : Unsigned_64; Accepted : out Boolean) with
     Post => Accepted = (Busy (S'Old) and then Ticket = Receipt (S'Old))
       and then Epoch (S) = Epoch (S'Old) and then Active (S) = Active (S'Old)
       and then Receipt (S) = Receipt (S'Old) and then Count (S) = Count (S'Old)
       and then (for all I in 1 .. Count (S) => Resource_At (S, I) = Resource_At (S'Old, I))
       and then (if Accepted then not Busy (S) else S = S'Old);
private
   type Entries is array (Positive range 1 .. Maximum_Resources) of Descriptor;
   type State is record
      Enabled : Boolean := False;
      Building : Boolean := False;
      In_Flight : Boolean := False;
      Sequence : Unsigned_64 := 0;
      Version : Unsigned_64 := 0;
      Used : Natural range 0 .. Maximum_Resources := 0;
      Items : Entries;
   end record with Type_Invariant =>
     (if State.Enabled or State.Building then State.Version /= 0)
     and then not (State.Enabled and State.Building)
     and then (if State.In_Flight then not State.Building
       and then State.Version /= 0 and then State.Sequence /= 0)
     and then (for all I in 1 .. State.Used => Valid (State.Items (I)))
     and then (for all I in 1 .. State.Used =>
       (for all J in 1 .. State.Used =>
         (if I /= J then State.Items (I).Permission.Resource_ID /=
           State.Items (J).Permission.Resource_ID)));
end Hardware_Catalog;
