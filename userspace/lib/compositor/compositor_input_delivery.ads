with Compositor_Input_Batch_Wire;
with CuBit.Grant_References;

-- Callbacks are the audited boundary: acquisition authenticates the owner,
-- generation, write access and complete snapshot extent. A failed acquisition
-- transfers no ownership. Return confirms that this acquisition is gone.
generic
   with procedure Acquire
     (Owner : Compositor_Input_Batch_Wire.Identity;
      Grant : CuBit.Grant_References.Reference;
      Mapping : out Compositor_Input_Batch_Wire.Word; Acquired : out Boolean);
   with procedure Write
     (Mapping : Compositor_Input_Batch_Wire.Word;
      Payload : Compositor_Input_Batch_Wire.Snapshot_Words;
      Written : out Boolean);
   with procedure Return_Loan
     (Grant : CuBit.Grant_References.Reference; Confirmed : out Boolean);
package Compositor_Input_Delivery with SPARK_Mode is
   package W renames Compositor_Input_Batch_Wire;
   package GR renames CuBit.Grant_References;
   use type GR.Reference;
   type State is private;
   function Pending (S : State) return Boolean;
   function Reference (S : State) return GR.Reference;
   type Outcome is (Published, Acquisition_Failed, Publication_Failed,
                    Quarantined, Busy);

   -- At most one acquire, one write and one return. No waiting, allocation,
   -- source-queue mutation or implicit retry. Never overwrite a pending loan.
   procedure Deliver
     (S : in out State; Owner : W.Identity; Grant : GR.Reference;
      Payload : W.Snapshot_Words; Result : out Outcome)
   with Post =>
     (if Pending (S'Old) then Result = Busy and S = S'Old
      else Result /= Busy and then
        Pending (S) = (Result = Quarantined) and then
        (if Pending (S) then Reference (S) = Grant));

   -- One cleanup attempt, including after surface/process destruction.
   -- The containing storage must outlive any channel and keep pending states.
   procedure Retire (S : in out State)
   with Post => Reference (S) = Reference (S'Old) and then
     (if not Pending (S'Old) then S = S'Old);
private
   type State is record
      Held : Boolean := False;
      Loan : GR.Reference;
   end record;
   function Pending (S : State) return Boolean is (S.Held);
   function Reference (S : State) return GR.Reference is (S.Loan);
end Compositor_Input_Delivery;
