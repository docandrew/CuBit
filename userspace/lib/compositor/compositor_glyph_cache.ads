with Compositor_Identity;
with Compositor_Glyph_Layout;
generic
   Last_Identity : Compositor_Identity.Positive_Value := Compositor_Identity.Positive_Value'Last;
   Maximum_Readers : Positive := 32;
package Compositor_Glyph_Cache with SPARK_Mode, Pure is
   subtype Serial_Number is Compositor_Identity.Value;
   use type Serial_Number;
   package L renames Compositor_Glyph_Layout;
   subtype Slot is Positive range 1 .. 128;
   subtype Search_Result is Natural range 0 .. Slot'Last;
   subtype Reader_Slot is Positive range 1 .. Maximum_Readers;
   subtype Byte_Count is Natural range 0 .. Slot'Last * L.Maximum_Bytes;
   type Key is record
      Face : Natural range 0 .. 1 := 0;
      Code : Natural range 32 .. 126 := 63;
      Scale : L.G.UI_Scale := (1, 1);
   end record;
   function Same (A, B : Key) return Boolean is
     (A.Face = B.Face and then A.Code = B.Code and then
      Positive (A.Scale.Numerator) * Positive (B.Scale.Denominator) =
      Positive (B.Scale.Numerator) * Positive (A.Scale.Denominator));
   type Phase is (Free, Building, Ready, Retiring);
   type Token is record
      Position : Slot := Slot'First;
      Identity : Serial_Number := 0;
   end record;
   No_Token : constant Token := (Slot'First, 0);
   type Lease is record
      Position : Reader_Slot := Reader_Slot'First;
      Identity : Serial_Number := 0;
   end record;
   No_Lease : constant Lease := (Reader_Slot'First, 0);
   type State is private;
   function Valid (S : State) return Boolean;
   function Charged (S : State) return Byte_Count;
   function Limit (S : State) return Byte_Count;
   function Sequence (S : State) return Serial_Number;
   function Read_Sequence (S : State) return Serial_Number;
   function Current (S : State; T : Token) return Boolean;
   function Status (S : State; T : Token) return Phase
     with Pre => Current (S, T);
   function Bytes (S : State; T : Token) return Positive
     with Pre => Valid (S) and then Current (S, T);
   function Matches (S : State; I : Slot; K : Key) return Boolean;
   function At_Slot (S : State; I : Slot) return Token;
   function Pinned (S : State; I : Slot) return Boolean;
   function Active (S : State; R : Lease) return Boolean;
   function Reader_Count (S : State) return Natural;
   function Reads_Slot (S : State; R : Lease; I : Slot) return Boolean;
   function Free_Reader (S : State; I : Reader_Slot) return Boolean;
   function Same_Readers (Before, After : State) return Boolean with Ghost;
   function Same_Glyphs (Before, After : State) return Boolean with Ghost;
   function Keeps_Readers (Before, After : State) return Boolean with Ghost;
   function Keeps_Others (Before, After : State; I : Reader_Slot) return Boolean with Ghost;
   -- Fresh cache owner only. Never reset an existing owner or route its old
   -- callbacks into a new cache state; tokens are scoped to that owner.
   function Open (Budget : Byte_Count) return State
     with Post => Valid (Open'Result) and then Charged (Open'Result) = 0 and then
       Limit (Open'Result) = Budget and then Reader_Count (Open'Result) = 0;
   -- Occupied includes pending construction and retirement: avoid duplicate
   -- work for the same face/code/rational density while it is already held.
   function Find (S : State; K : Key) return Search_Result
     with Post => (if Find'Result = 0 then
         (for all I in Slot => not Matches (S, I, K))
       else Matches (S, Find'Result, K));
   -- Bounded round-robin candidate; caller explicitly retires it before reuse.
   function Victim (S : State) return Search_Result
     with Post => (if Victim'Result /= 0 then
       Current (S, At_Slot (S, Victim'Result)) and then
       Status (S, At_Slot (S, Victim'Result)) = Ready and then
       not Pinned (S, Victim'Result));
   procedure Reserve (S : in out State; K : Key; T : out Token)
     with Pre => Valid (S), Post => Valid (S) and then Same_Readers (S'Old, S) and then Limit (S) = Limit (S'Old) and then
       Reader_Count (S) = Reader_Count (S'Old) and then
       Read_Sequence (S) = Read_Sequence (S'Old) and then
       (if T = No_Token then S = S'Old else
         Current (S, T) and then Status (S, T) = Building and then
         Matches (S, T.Position, K) and then
         Sequence (S) = Sequence (S'Old) + 1 and then T.Identity = Sequence (S) and then
         Charged (S) = Charged (S'Old) + L.Plan (K.Scale).Bytes);
   procedure Publish (S : in out State; T : Token; Raster_Completed : Boolean)
     with Pre => Valid (S), Post => Valid (S) and then Limit (S) = Limit (S'Old) and then Same_Readers (S'Old, S) and then Charged (S) = Charged (S'Old) and then
       Sequence (S) = Sequence (S'Old) and then Read_Sequence (S) = Read_Sequence (S'Old) and then
       (if not Current (S'Old, T) or else Status (S'Old, T) /= Building or else
         not Raster_Completed then S = S'Old
        else Current (S, T) and then Status (S, T) = Ready);
   procedure Acquire (S : in out State; T : Token; R : out Lease)
     with Pre => Valid (S), Post => Valid (S) and then Limit (S) = Limit (S'Old) and then Keeps_Readers (S'Old, S) and then Charged (S) = Charged (S'Old) and then
       Sequence (S) = Sequence (S'Old) and then
       (if R = No_Lease then S = S'Old else Active (S, R) and then Free_Reader (S'Old, R.Position) and then Reads_Slot (S, R, T.Position) and then
         Current (S, T) and then Status (S, T) = Ready and then Pinned (S, T.Position) and then
         Read_Sequence (S) = Read_Sequence (S'Old) + 1 and then R.Identity = Read_Sequence (S) and then
         Reader_Count (S) = Reader_Count (S'Old) + 1);
   -- Completion must identify the exact acquisition, not just the glyph slot.
   -- Unknown completion leaves the lease active and the mask non-evictable.
   procedure Complete (S : in out State; R : Lease; Quiescent : Boolean)
     with Pre => Valid (S), Post => Valid (S) and then Same_Glyphs (S, S'Old) and then Limit (S) = Limit (S'Old) and then Keeps_Others (S'Old, S, R.Position) and then Charged (S) = Charged (S'Old) and then
       Sequence (S) = Sequence (S'Old) and then Read_Sequence (S) = Read_Sequence (S'Old) and then
       (if not Active (S'Old, R) or else not Quiescent then S = S'Old
        else not Active (S, R) and then Reader_Count (S) = Reader_Count (S'Old) - 1);
   procedure Begin_Retirement (S : in out State; T : Token; Accepted : out Boolean)
     with Pre => Valid (S), Post => Valid (S) and then Limit (S) = Limit (S'Old) and then Same_Readers (S'Old, S) and then Charged (S) = Charged (S'Old) and then
       Sequence (S) = Sequence (S'Old) and then Read_Sequence (S) = Read_Sequence (S'Old) and then
       Accepted = (Current (S'Old, T) and then Status (S'Old, T) in Building | Ready and then
                   not Pinned (S'Old, T.Position)) and then
       (if not Accepted then S = S'Old else Current (S, T) and then Status (S, T) = Retiring);
   -- Confirmation covers all foreign imports AND backing storage. Merely
   -- submitting release, or an ambiguous release result, cannot refund bytes.
   procedure Retired (S : in out State; T : Token; Confirmed : Boolean)
     with Pre => Valid (S), Post => Valid (S) and then Same_Readers (S'Old, S) and then Limit (S) = Limit (S'Old) and then
       Sequence (S) = Sequence (S'Old) and then Read_Sequence (S) = Read_Sequence (S'Old) and then
       (if not Current (S'Old, T) or else Status (S'Old, T) /= Retiring or else
         not Confirmed then S = S'Old else not Current (S, T) and then
         Charged (S) = Charged (S'Old) - Bytes (S'Old, T));
private
   type Item is record
      Stage : Phase := Free;
      Identity : Serial_Number := 0;
      Glyph : Key;
      Bytes : Natural range 0 .. L.Maximum_Bytes := 0;
   end record;
   type Items is array (Slot) of Item;
   type Reader is record
      Identity : Serial_Number := 0;
      Mask : Token := No_Token;
   end record;
   type Readers is array (Reader_Slot) of Reader;
   type State is record
      Budget : Byte_Count := 0;
      Issued, Read_Issued : Serial_Number range 0 .. Last_Identity := 0;
      Cursor : Slot := Slot'First;
      Masks : Items;
      Reading : Readers;
   end record;
   function Prefix (V : Items; N : Search_Result) return Byte_Count is
     (if N = 0 then 0 else Prefix (V, N - 1) + V (N).Bytes)
     with Subprogram_Variant => (Decreases => N),
       Post => Prefix'Result <= N * L.Maximum_Bytes and then
         (if (for all I in 1 .. N => V (I).Bytes = 0) then Prefix'Result = 0);
   function Read_Prefix (V : Readers; N : Natural) return Natural is
     (if N = 0 then 0 else Read_Prefix (V, N - 1) + (if V (N).Identity = 0 then 0 else 1))
     with Pre => N <= Reader_Slot'Last, Subprogram_Variant => (Decreases => N),
       Post => Read_Prefix'Result <= N and then
         (if (for all I in 1 .. N => V (I).Identity = 0) then Read_Prefix'Result = 0);
   function Same_Glyphs (Before, After : State) return Boolean is (Before.Masks = After.Masks);
   function Charged (S : State) return Byte_Count is (Prefix (S.Masks, Slot'Last));
   function Limit (S : State) return Byte_Count is (S.Budget);
   function Sequence (S : State) return Serial_Number is (S.Issued);
   function Read_Sequence (S : State) return Serial_Number is (S.Read_Issued);
   function Reader_Count (S : State) return Natural is (Read_Prefix (S.Reading, Reader_Slot'Last));
   function Current (S : State; T : Token) return Boolean is
     (T.Identity > 0 and then S.Masks (T.Position).Identity = T.Identity and then S.Masks (T.Position).Stage /= Free);
   function At_Slot (S : State; I : Slot) return Token is ((I, S.Masks (I).Identity));
   function Status (S : State; T : Token) return Phase is (S.Masks (T.Position).Stage);
   function Bytes (S : State; T : Token) return Positive is (S.Masks (T.Position).Bytes);
   function Matches (S : State; I : Slot; K : Key) return Boolean is
     (S.Masks (I).Stage /= Free and then Same (S.Masks (I).Glyph, K));
   function Pinned (S : State; I : Slot) return Boolean is
     (for some J in Reader_Slot => S.Reading (J).Identity /= 0 and then S.Reading (J).Mask.Position = I);
   function Active (S : State; R : Lease) return Boolean is
     (R.Identity /= 0 and then S.Reading (R.Position).Identity = R.Identity);
   function Reads_Slot (S : State; R : Lease; I : Slot) return Boolean is
     (Active (S, R) and then S.Reading (R.Position).Mask.Position = I);
   function Free_Reader (S : State; I : Reader_Slot) return Boolean is (S.Reading (I).Identity = 0);
   function Same_Readers (Before, After : State) return Boolean is (Before.Reading = After.Reading);
   function Keeps_Readers (Before, After : State) return Boolean is
     (for all I in Reader_Slot => (if Before.Reading (I).Identity /= 0 then Before.Reading (I) = After.Reading (I)));
   function Keeps_Others (Before, After : State; I : Reader_Slot) return Boolean is
     (for all J in Reader_Slot => (if J /= I then Before.Reading (J) = After.Reading (J)));
   function Valid (S : State) return Boolean is
     (Charged (S) <= S.Budget and then
       (for all I in Slot => (if S.Masks (I).Stage = Free then
         S.Masks (I).Identity = 0 and then S.Masks (I).Bytes = 0
         else S.Masks (I).Identity in 1 .. S.Issued and then S.Masks (I).Bytes > 0)) and then
       (for all J in Reader_Slot => (if S.Reading (J).Identity = 0 then S.Reading (J).Mask = No_Token
         else S.Reading (J).Identity <= S.Read_Issued and then
           Current (S, S.Reading (J).Mask) and then Status (S, S.Reading (J).Mask) = Ready)));
   function Open (Budget : Byte_Count) return State is ((Budget => Budget, others => <>));
end Compositor_Glyph_Cache;
