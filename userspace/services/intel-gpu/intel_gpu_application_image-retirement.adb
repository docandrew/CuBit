with Intel_GPU_GGTT_Retire;
with Intel_GPU_Submission_Image;
package body Intel_GPU_Application_Image.Retirement is
   procedure Execute
     (Object : in out State; Ledger : Intel_GPU_GGTT_Reservations.Ledger;
      Scratch_DMA : Unsigned_64; Status : out Result) is
      package Replies renames Intel_GPU_Buffer_Reply;
      First_Page : constant Unsigned_64 := Replies.Page_Address (Object.Allocation, 0);
      function Resolve_Page (Base, Offset : Unsigned_64) return Unsigned_64 is
        (if Base = First_Page and then First_Page /= 0 then
           Replies.Page_Address (Object.Allocation, Offset) else 0);
      function Allowed (First, Bytes : Unsigned_64) return Boolean is
        (not Object.Updating and then Gate (First, Bytes));
      package Retirer is new Intel_GPU_GGTT_Retire
        (Allowed, Read_PTE, Write_PTE, Invalidate_And_Wait, Resolve_Page);
      Attempt : Retirer.Attempt;
      Outcome : Retirer.Result;
   begin
      Status := Rejected;
      if Object.Retirement_Attempted or else Object.Published = 0 or else
        Object.Updating or else not Replies.Valid (Object.Allocation)
      then return; end if;
      Object.Retirement_Attempted := True;
      Retirer.Execute (Attempt, Ledger, Object.Published, First_Page,
        Intel_GPU_Submission_Image.GGTT_Bytes, Scratch_DMA, Outcome);
      Status := (case Outcome is
        when Retirer.Rejected => Rejected,
        when Retirer.Quarantined => Quarantined,
        when Retirer.Detached => Detached);
   end Execute;
end Intel_GPU_Application_Image.Retirement;
