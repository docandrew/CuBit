with Intel_GPU_GGTT_Publish;
with Intel_GPU_GGTT;
with Intel_GPU_Submission_Image;
package body Intel_GPU_Application_Image.Publication is
   function GPU_Address (Object : State) return Unsigned_64 is (Object.Published);
   procedure Publish
     (Object : in out State; Source : VM.Image; Backing : Tables.Mappings;
      Allocation : Intel_GPU_Buffer_Reply.Backing;
      Reservations : in out Intel_GPU_GGTT_Reservations.Ledger;
      Status : out Result) is
      Bytes : constant Unsigned_64 := Intel_GPU_Submission_Image.GGTT_Bytes;
      First_Page : constant Unsigned_64 :=
        Intel_GPU_Buffer_Reply.Page_Address (Allocation, 0);
      Prepared_First, Selected : Unsigned_64 := 0;
      function Resolve_Page (Base, Offset : Unsigned_64) return Unsigned_64 is
        (if First_Page /= 0 and then Base = First_Page then
           Intel_GPU_Buffer_Reply.Page_Address (Allocation, Offset) else 0);
      function Allowed (First, Size : Unsigned_64) return Boolean is
        (Owner_Ready and then Range_Allowed (First, Size));
      procedure Prepare_Backing (First, Size : Unsigned_64; Success : out Boolean) is
      begin
         Prepare (Object, Source, Backing, Allocation, First, Size, Success);
         if Success then Prepared_First := First; end if;
      end Prepare_Backing;
      procedure Write_Checked (Index, Value : Unsigned_64; Success : out Boolean) is
      begin
         Success := False;
         if Prepared_First = 0 or else not Allowed (Prepared_First, Bytes) or else
           Index < Prepared_First / 4096 or else
           Index - Prepared_First / 4096 >= Bytes / 4096 or else
           Value /= Intel_GPU_GGTT.Encode_System_Page
             (Resolve_Page (First_Page, (Index - Prepared_First / 4096) * 4096))
         then return; end if;
         Write_PTE (Index, Value, Success);
      end Write_Checked;
      package Publisher is new Intel_GPU_GGTT_Publish
        (Allowed, Prepare_Backing, Read_PTE, Write_Checked, Invalidate,
         Resolve_Page => Resolve_Page);
      Attempt : Publisher.Attempt;
      Outcome : Publisher.Result;
      use type Publisher.Result;
   begin
      Status := Rejected;
      if Object.Publication_Attempted or else Object.Attempted then return; end if;
      Object.Publication_Attempted := True;
      if not Allocation.Ready or else not Owner_Ready then return; end if;
      Publisher.Publish_Available (Attempt, Reservations, First_Page,
                                   Bytes, 4096, Selected, Outcome);
      if Outcome = Publisher.Published then
         Object.Published := Selected;
         Status := Published;
      elsif Outcome = Publisher.Quarantined then
         Status := Quarantined;
      else
         Status := Mapping_Failed;
      end if;
   end Publish;
end Intel_GPU_Application_Image.Publication;
