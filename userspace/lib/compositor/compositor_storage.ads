-- Reclaimable CPU pixel accounting. External facts are supplied by an audited
-- adapter; address validity, reader quiescence and physical release are trusted.
generic
   Last_Identity : Positive := Positive'Last;
   Slot_Count : Positive := 8;
package Compositor_Storage with SPARK_Mode, Pure is
   type Slot is new Positive range 1 .. Slot_Count;
   type Ticket is private;
   No_Ticket : constant Ticket;
   type Phase is (Free, Allocating, Live, Releasing, Quarantined);
   type State is private;
   function Valid (S : State) return Boolean;
   function Charged (S : State) return Natural;
   function Limit (S : State) return Natural;
   function Current (S : State; T : Ticket) return Boolean;
   function Status (S : State; T : Ticket) return Phase
     with Pre => Current (S, T);
   function Bytes (S : State; T : Ticket) return Positive
     with Pre => Valid (S) and Current (S, T);
   function Index (T : Ticket) return Slot;
   function Identity (T : Ticket) return Natural;
   function Issued (S : State) return Natural;
   function Others_Unchanged (S, Before : State; T : Ticket) return Boolean;
   -- Fresh process state only; never reset a ledger with issued identities.
   function Open (Byte_Limit : Natural) return State
     with Post => Valid (Open'Result) and Charged (Open'Result) = 0 and
       Limit (Open'Result) = Byte_Limit;
   -- Charge before invoking allocation. Exhaustion never wraps an identity.
   procedure Reserve (S : in out State; Size : Positive; T : out Ticket)
     with Pre => Valid (S),
       Post => Valid (S) and Limit (S) = Limit (S'Old) and
         (if T = No_Ticket then S = S'Old
          else Current (S, T) and Status (S, T) = Allocating and
            Bytes (S, T) = Size and Charged (S) = Charged (S'Old) + Size and
            Issued (S) = Issued (S'Old) + 1 and Identity (T) = Issued (S) and
            Others_Unchanged (S, S'Old, T));
   -- A zero allocator result may retain a quarantined prefix: never refund it.
   procedure Allocated (S : in out State; T : Ticket; Success : Boolean)
     with Pre => Valid (S) and then Current (S, T) and then Status (S, T) = Allocating,
       Post => Valid (S) and Current (S, T) and
         Status (S, T) = (if Success then Live else Quarantined) and
         Charged (S) = Charged (S'Old) and Limit (S) = Limit (S'Old) and
         Issued (S) = Issued (S'Old) and Others_Unchanged (S, S'Old, T);
   procedure Begin_Release (S : in out State; T : Ticket; Readers_Retired : Boolean)
     with Pre => Valid (S) and then Current (S, T) and then Status (S, T) = Live,
       Post => Valid (S) and Current (S, T) and
         (if Readers_Retired then Status (S, T) = Releasing else S = S'Old) and
         Charged (S) = Charged (S'Old) and Limit (S) = Limit (S'Old) and
         Issued (S) = Issued (S'Old) and Others_Unchanged (S, S'Old, T);
   procedure Released (S : in out State; T : Ticket; Confirmed : Boolean)
     with Pre => Valid (S) and then Current (S, T) and then Status (S, T) = Releasing,
       Post => Valid (S) and Limit (S) = Limit (S'Old) and
         Issued (S) = Issued (S'Old) and Others_Unchanged (S, S'Old, T) and
         (if Confirmed then not Current (S, T) and
            Charged (S) = Charged (S'Old) - Bytes (S'Old, T)
          else Current (S, T) and Status (S, T) = Quarantined and
            Charged (S) = Charged (S'Old));
private
   type Ticket is record
      Position : Slot := Slot'First;
      Identity : Natural := 0;
   end record;
   No_Ticket : constant Ticket := (Slot'First, 0);
   type Entry_Record is record
      Identity, Size : Natural := 0;
      Stage : Phase := Free;
   end record;
   type Entries is array (Slot) of Entry_Record;
   type State is record
      Capacity, Used : Natural := 0;
      Serial : Natural range 0 .. Last_Identity := 0;
      Items : Entries;
   end record;
   function Prefix (S : State; N : Natural) return Long_Long_Integer
     with Pre => N <= Slot_Count,
       Post => Prefix'Result >= 0 and Prefix'Result <= Long_Long_Integer (N) * Long_Long_Integer (Natural'Last),
       Subprogram_Variant => (Decreases => N);
   function Total (S : State) return Long_Long_Integer is (Prefix (S, Slot_Count));
   function Valid (S : State) return Boolean is
     (S.Used <= S.Capacity and Total (S) = Long_Long_Integer (S.Used) and
      (for all I in Slot =>
         S.Items (I).Identity <= S.Serial and
         (if S.Items (I).Stage = Free then S.Items (I).Size = 0
          else S.Items (I).Size > 0 and S.Items (I).Identity > 0)));
   function Charged (S : State) return Natural is (S.Used);
   function Limit (S : State) return Natural is (S.Capacity);
   function Current (S : State; T : Ticket) return Boolean is
     (T.Identity /= 0 and S.Items (T.Position).Identity = T.Identity and
      S.Items (T.Position).Stage /= Free);
   function Status (S : State; T : Ticket) return Phase is (S.Items (T.Position).Stage);
   function Bytes (S : State; T : Ticket) return Positive is (S.Items (T.Position).Size);
   function Index (T : Ticket) return Slot is (T.Position);
   function Identity (T : Ticket) return Natural is (T.Identity);
   function Issued (S : State) return Natural is (S.Serial);
   function Others_Unchanged (S, Before : State; T : Ticket) return Boolean is
     (for all I in Slot => (if I /= T.Position then S.Items (I) = Before.Items (I)));

end Compositor_Storage;
