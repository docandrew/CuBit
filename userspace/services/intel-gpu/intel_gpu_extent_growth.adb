with Intel_GPU_Record_Growth;
package body Intel_GPU_Extent_Growth is
   function Capacity return Positive is (Allocator.Extent_Capacity (Pool));
   procedure Publish (Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin Allocator.Extend_Extents (Pool, Base, Bytes, Accepted); end Publish;
   package Growth is new Intel_GPU_Record_Growth (Storage, Capacity, Publish);
   use type Growth.Phase;
   Controller : Growth.Controller;
   Configured, Failed : Boolean := False;
   procedure Step
     (Arena_ID : Unsigned_64; Index : Positive;
      Pages : Intel_GPU_Buffer_Reply.Layout.Page_Count; Generation : Unsigned_32;
      Buffer : out Intel_GPU_Buffer_Reply.Extent_View; Success, Pending : out Boolean) is
      Empty : Intel_GPU_Buffer_Reply.Extent_View;
      Needed : Natural;
      OK : Boolean;
   begin
      Buffer := Empty; Success := False; Pending := False;
      if Failed then return; end if;
      if not Owner_Ready then Failed := True; return; end if;
      if not Configured then
         Growth.Configure (Controller, Metadata_Bytes, Positive'Last, OK);
         if not OK then Failed := True; return; end if;
         Configured := True;
      end if;
      if Growth.Snapshot (Controller).State = Growth.Failed then
         Failed := True; return;
      elsif Growth.Snapshot (Controller).State /= Growth.Idle then
         Growth.Step (Controller);
         Pending := True;
         return;
      end if;
      Needed := Allocator.Required_Extent_Metadata (Pool);
      if Needed /= 0 then
         Growth.Request (Controller, Needed, OK);
         if not OK then Failed := True; return; end if;
         Pending := True;
         return;
      end if;
      Allocator.Step_Buffer (Pool, Arena_ID, Index, Pages, Generation, Buffer, Success, Pending);
   end Step;
end Intel_GPU_Extent_Growth;
