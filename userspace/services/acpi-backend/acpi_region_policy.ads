pragma Ada_2022;
with Interfaces; use Interfaces;
with ACPI_FADT.Transactions;
with Hardware_Authority;
-- Backend-owned policy for one resource behind existing endpoint authority.
-- This is not a capability minting interface. Install/Revoke are trusted-only
-- operations, never dispatched from ACPI/AML requests. Platform ownership must
-- be independently established before Install. Serialize through actual I/O.
package ACPI_Region_Policy with SPARK_Mode is
   subtype Access_Width is ACPI_FADT.Transactions.Access_Width;
   type Width_Set is array (Access_Width) of Boolean;
   type Address_Space is (Memory_Space, IO_Space);
   type Configuration is record
      Tag, Base, Length : Unsigned_64 := 0;
      Space : Address_Space := Memory_Space;
      Readable, Writable : Boolean := False;
      Widths : Width_Set := [others => False];
      -- Trusted named-register binding, frozen with this record's epoch.
      -- ID zero disables the public register endpoint. These fields are never
      -- installed or altered by a wire request. Kernel catalog admission is
      -- still required before real access can be enabled.
      Register_Authority : Hardware_Authority.Permission;
      Register_Offset : Unsigned_64 := 0;
      Register_Width : Access_Width := ACPI_FADT.Transactions.Byte_Access;
   end record;
   function Valid (C : Configuration) return Boolean is
     (C.Tag /= 0 and then C.Base /= 0 and then C.Length /= 0
      and then C.Length - 1 <= Unsigned_64'Last - C.Base
      and then (C.Readable or C.Writable)
      and then (if C.Space = IO_Space then
        C.Base <= 65535 and then C.Length - 1 <= 65535 - C.Base
        and then not C.Widths (ACPI_FADT.Transactions.Qword_Access)));
   type State is private;
   function Active (S : State) return Boolean;
   function Epoch (S : State) return Unsigned_64;
   function Policy (S : State) return Configuration;
   function Busy (S : State) return Boolean;
   function Receipt (S : State) return Unsigned_64;
   function Ready_To_Release (S : State) return Boolean is
     (not Active (S) and then not Busy (S));
   -- Fresh state starts disabled. Preserve the record and epoch across revoke
   -- and reinstallation; resetting a record can resurrect old request tokens.
   procedure Install (S : in out State; C : Configuration; Accepted : out Boolean) with
     Post => (if Accepted then Active (S) and then Valid (Policy (S))
       and then Policy (S) = C and then Epoch (S'Old) < Unsigned_64'Last
       and then Epoch (S) = Epoch (S'Old) + 1
       else S = S'Old)
       and then (if Busy (S'Old) then not Accepted and then S = S'Old)
       and then Receipt (S) = Receipt (S'Old);
   procedure Revoke (S : in out State) with
     Post => not Active (S) and then Epoch (S) = Epoch (S'Old)
       and then Policy (S) = Policy (S'Old)
       and then Busy (S) = Busy (S'Old) and then Receipt (S) = Receipt (S'Old);
   type Decision (Allowed : Boolean := False) is record
      case Allowed is
         when True => Address : Unsigned_64;
         when False => null;
      end case;
   end record;
   -- Stamp is authenticated kernel metadata. Token, offset, width and write
   -- selection are untrusted request inputs. No request accepts a base address.
   function Resolve
     (S : State; Stamp, Token, Offset : Unsigned_64;
      Width : Access_Width; For_Write : Boolean) return Decision with
     Post => (if Resolve'Result.Allowed then
       Active (S) and then Valid (Policy (S))
       and then Stamp = Policy (S).Tag and then Token = Epoch (S)
       and then Policy (S).Widths (Width)
       and then (if For_Write then Policy (S).Writable else Policy (S).Readable)
       and then Offset < Policy (S).Length
       and then Unsigned_64 (ACPI_FADT.Transactions.Octets (Width)) <= Policy (S).Length - Offset
       and then Resolve'Result.Address = Policy (S).Base + Offset
       and then Resolve'Result.Address >= Policy (S).Base
       and then Unsigned_64 (ACPI_FADT.Transactions.Octets (Width) - 1) <=
         Unsigned_64'Last - Resolve'Result.Address
       and then ACPI_FADT.Transactions.Aligned (Resolve'Result.Address, Width));
   -- Native callers must reserve through Begin_Access rather than execute a
   -- bare Resolve result. Serialize all state transitions. Hold the underlying
   -- resource until Ready_To_Release; callbacks may complete after revocation.
   -- Tickets are backend completion correlation, not delegable authority.
   procedure Begin_Access
     (S : in out State; Stamp, Token, Offset : Unsigned_64;
      Width : Access_Width; For_Write : Boolean;
      Result : out Decision; Ticket : out Unsigned_64) with
     Pre => not Result'Constrained,
     Post => Policy (S) = Policy (S'Old) and then Epoch (S) = Epoch (S'Old)
       and then Active (S) = Active (S'Old)
       and then (if Result.Allowed then
         not Busy (S'Old) and then Busy (S) and then Ticket = Receipt (S)
         and then Receipt (S'Old) < Unsigned_64'Last
         and then Receipt (S) = Receipt (S'Old) + 1
         and then Result = Resolve (S'Old, Stamp, Token, Offset, Width, For_Write)
       else S = S'Old and then Ticket = 0);
   procedure Finish_Access
     (S : in out State; Ticket : Unsigned_64; Accepted : out Boolean) with
     Post => Accepted = (Busy (S'Old) and then Ticket = Receipt (S'Old))
       and then Policy (S) = Policy (S'Old) and then Epoch (S) = Epoch (S'Old)
       and then Active (S) = Active (S'Old) and then Receipt (S) = Receipt (S'Old)
       and then (if Accepted then not Busy (S) else S = S'Old);
private
   type State is record
      Enabled : Boolean := False;
      In_Flight : Boolean := False;
      Sequence : Unsigned_64 := 0;
      Version : Unsigned_64 := 0;
      Config : Configuration;
   end record with Type_Invariant =>
     (if State.Enabled or State.In_Flight then State.Version /= 0 and then Valid (State.Config))
     and then (if State.In_Flight then State.Sequence /= 0);
end ACPI_Region_Policy;
