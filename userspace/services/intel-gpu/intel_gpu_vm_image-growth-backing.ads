generic
   with function Owned_Table (DMA : Unsigned_64) return Boolean;
package Intel_GPU_VM_Image.Growth.Backing is
   type Link is record
      Parent_DMA, Child_DMA, Expected, Value, Fill : Unsigned_64 := 0;
      Index : Intel_GPU_ADLN_PPGTT.Table_Index := 0;
   end record;
   type Links is array (Positive range <>) of Link;
   type Resolution_Phase is
     (Empty, Inspecting, Capturing, Checking_Aliases, Checking_Duplicates,
      Preflighting, Resolving, Complete_Resolution, Failed_Resolution);
   type Resolution is limited private;
   procedure Start_Resolution
     (State : in out Resolution; Source : Image; GPU, Bytes, Retained_Root : Unsigned_64;
      Page_Count, Output_Capacity : Natural; Accepted : out Boolean);
   procedure Cancel_Resolution (State : in out Resolution);
   function Phase (State : Resolution) return Resolution_Phase;
   function Resolution_Valid (State : Resolution; Source : Image) return Boolean;
   generic
      with function Authorized return Boolean;
      with function Read_Page (Ordinal : Positive) return Unsigned_64;
      with procedure Emit
        (Ordinal : Positive; Item : Link; Topology : Node; Accepted : out Boolean);
   procedure Step_Resolution (State : in out Resolution; Source : Image);
   -- One inspection/description quantum, one capture, or at most32 alias or
   -- duplicate checks per step. A description quantum invokes at most32 node
   -- callbacks, each with at most2 Read_Page,1 Owned_Table and1 Emit calls.
   -- External callback duration is not bounded here. No allocation or GPU IO.
   -- Source and page list must remain retained, stable and serialized; callbacks
   -- are non-reentrant, may revoke authority/cancel, and must not publish output.
   -- Rejected/cancelled output is private garbage, never an accepted plan.
   generic
      with function Read_Page (Ordinal : Positive) return Unsigned_64;
      with procedure Emit
        (Ordinal : Positive; Item : Link; Topology : Node; Accepted : out Boolean);
   procedure Resolve_Into
     (Source : Image; GPU, Bytes, Retained_Root : Unsigned_64;
      Page_Count, Output_Capacity : Natural; Accepted : out Boolean);
   -- Private staging only: a rejected emission can leave an uncommitted
   -- prefix. Stable inputs and serialized non-reentrant callbacks required.
   generic
      with function Read_Page (Ordinal : Positive) return Unsigned_64;
   procedure Resolve_From_Pages
     (Source : Image; GPU, Bytes, Retained_Root : Unsigned_64;
      Page_Count : Natural; Output : out Links; Accepted : out Boolean);
   -- Trusted stable retained input, possibly read more than once. No input
   -- array retained; same complete alias/topology/ownership preflight.
   procedure Resolve
     (Source : Image; GPU, Bytes, Retained_Root : Unsigned_64;
      New_Pages : Data_Pages; Output : out Links; Accepted : out Boolean);
   -- Pure preparation, no publication. Owned_Table must authenticate retained
   -- page-table backing in this VM, not merely test address geometry. Caller
   -- serializes source/backing lifetime. Retained_Root is trusted context state,
   -- never a client address. New pages are distinct from ALL reserved/source
   -- tables, mapped data, scratch and retained root. No hidden aliases allowed.
   -- Fill is the fallback word for all512 child entries. Expected is the
   -- parent fallback; writer must compare it before publishing Value, then
   -- flush/read back. This plan is not a TLB completion or commit receipt.
private
   type Resolution is limited record
      Value : Resolution_Phase := Empty;
      Query : Inspection;
      Topology : Description;
      GPU, Bytes, Retained_Root, Root, Epoch, Child : Unsigned_64 := 0;
      Count, Limit, Cursor, Previous, Scan : Natural := 0;
      Cancelled : Boolean := False;
   end record;
end Intel_GPU_VM_Image.Growth.Backing;
