with CuBit.Display_Pool_Protocol;
with CuBit.Grant_References;
package CuBit.Display_Pool_Registry with Pure, SPARK_Mode is
   package P renames CuBit.Display_Pool_Protocol;
   package D renames P.D;
   use type CuBit.Grant_References.Reference, D.Buffer_Layout, D.Attachment_Request;
   type State is private;
   function Count (S : State) return Natural;
   function Active (S : State) return Boolean;
   function Faulted (S : State) return Boolean;
   function Registered (S : State; B : P.Buffer_Slot) return Boolean;
   function Item (S : State; B : P.Buffer_Slot) return D.Attachment_Request
     with Pre => Registered (S, B);
   function Admits (S : State; A : P.Attachment) return Boolean;
   -- Caller first acquires the complete range using authenticated owner and
   -- read rights. References are identities, never proof of memory authority.
   procedure Register (S : in out State; A : P.Attachment; Accepted : out Boolean)
     with Post => (Accepted = Admits (S'Old, A)) and
       (if Accepted then Count (S) = Count (S'Old) + 1 and
          Registered (S, A.Buffer) and Item (S, A.Buffer) = A.Source
        else S = S'Old) and
       (for all B in P.Buffer_Slot =>
          (if Registered (S'Old, B) then Registered (S, B) and Item (S, B) = Item (S'Old, B)));
   procedure Open (S : in out State; Accepted : out Boolean)
     with Post => (Accepted = (Count (S'Old) = 3 and not Active (S'Old) and not Faulted (S'Old))) and
       (if Accepted then Active (S) else S = S'Old) and Count (S) = Count (S'Old);
   procedure Quarantine (S : in out State)
     with Post => Faulted (S) and Count (S) = Count (S'Old);
private
   type Entry_Info is record
      Present : Boolean := False;
      Source : D.Attachment_Request := (Grant => <>, Layout => (1, 1, 4));
   end record;
   type Entries is array (P.Buffer_Slot) of Entry_Info;
   type State is record
      Sources : Entries;
      Live, Failed : Boolean := False;
   end record;
   function Count (S : State) return Natural is
     (Boolean'Pos (S.Sources (1).Present) + Boolean'Pos (S.Sources (2).Present) +
      Boolean'Pos (S.Sources (3).Present));
   function Active (S : State) return Boolean is (S.Live);
   function Faulted (S : State) return Boolean is (S.Failed);
   function Registered (S : State; B : P.Buffer_Slot) return Boolean is (S.Sources (B).Present);
   function Item (S : State; B : P.Buffer_Slot) return D.Attachment_Request is (S.Sources (B).Source);
   function Admits (S : State; A : P.Attachment) return Boolean is
     (not S.Live and not S.Failed and not Registered (S, A.Buffer) and
      D.DP.Valid_Layout (A.Source.Layout) and
      (for all B in P.Buffer_Slot => (if Registered (S, B) then
         Item (S, B).Layout = A.Source.Layout and Item (S, B).Grant /= A.Source.Grant)));
end CuBit.Display_Pool_Registry;
