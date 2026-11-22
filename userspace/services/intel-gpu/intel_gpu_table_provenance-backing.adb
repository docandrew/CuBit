with Intel_GPU_ADLN_PPGTT;
package body Intel_GPU_Table_Provenance.Backing is
   procedure Resolve_Owned_Page
     (Session, Ticket, Offset : Unsigned_64;
      CPU, DMA : out Unsigned_64; Accepted : out Boolean)
   is
      Selected : Intel_GPU_Buffer_Reply.Backing;
      First, Relative, Page : Unsigned_64;
      Found : Boolean;
   begin
      CPU := 0;
      DMA := 0;
      Accepted := False;
      if Session = 0 or else Ticket = 0 or else Offset mod 4096 /= 0
        or else not Owner_Ready or else Ticket_Session (Ticket) /= Session
      then
         return;
      end if;
      Select_Slice (Session, Ticket, Selected, First, Found);
      if not Found or else First mod 4096 /= 0 or else Offset < First
        or else not Intel_GPU_Buffer_Reply.Valid (Selected)
      then
         return;
      end if;
      Relative := Offset - First;
      if Selected.Bytes < 4096 or else Relative > Selected.Bytes - 4096 then
         return;
      end if;
      Page := Intel_GPU_Buffer_Reply.Page_Address (Selected, Relative);
      if not Intel_GPU_ADLN_PPGTT.Valid_DMA_Page (Page)
        or else not Owner_Ready or else Ticket_Session (Ticket) /= Session
      then
         return;
      end if;
      CPU := Selected.CPU_Address + Relative;
      DMA := Page;
      Accepted := True;
   end Resolve_Owned_Page;
end Intel_GPU_Table_Provenance.Backing;
