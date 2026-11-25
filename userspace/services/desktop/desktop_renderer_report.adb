------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with CuBit.Graphics_Report.Posting;
with CuBit.Handles;
with CuBit.Messages;
with Desktop_Endpoints;

package body Desktop_Renderer_Report with SPARK_Mode is

   Output_Prefix : constant String := "output-";

   function Output_Name (Output : GR.Subject_Id) return GR.Name is
      Image : constant String := GR.Subject_Id'Image (Output);
   begin
      return GR.To_Name (Output_Prefix & Image (Image'First + 1 .. Image'Last));
   end Output_Name;

   function Output_Entry (Item : Facts; Output : GR.Subject_Id; Since : GR.Milliseconds)
     return GR.Report_Entry
   is
      Base : constant GR.Report_Entry :=
        (Kind => GR.Output, Id => Output, Subject => Output_Name (Output), Path => GR.Software,
         Why => GR.Startup_Timed_Out, Detail => 0, Device => GR.To_Name ("cpu"), Since => Since,
         Stale => False);
   begin
      if Item.Full_Output then
         return (Base with delta Path => GR.Hardware, Why => GR.None, Device => GR.To_Name ("intel-gpu"));
      elsif Item.CPU_Policy then
         return (Base with delta Why => GR.Policy, Detail => GR.Policy_Setting'Pos (GR.Composition_CPU));
      elsif Item.Abandoned then
         return Base;
      elsif not Item.Allowed or else not Item.Evidence.Admitted then
         --  No render session: the broker's reason is procmgr's entry for
         --  Desktop; here, refused.
         return (Base with delta Why => GR.Admission_Refused, Detail => GR.Admission_Outcome'Pos (GR.Rejected));
      else
         --  The session was admitted; Mesa or a later stage failed.
         return (Base with delta Why => GR.ICD_Init_Failed, Detail => Item.VK_Result);
      end if;
   end Output_Entry;

   function Fallback_Entry (Cause : GR.Fallback_Cause; Output : GR.Subject_Id; Since : GR.Milliseconds)
     return GR.Report_Entry is
     ((Kind => GR.Output, Id => Output, Subject => Output_Name (Output), Path => GR.Software,
       Why => GR.Fell_Back, Detail => GR.Fallback_Cause'Pos (Cause), Device => GR.To_Name ("cpu"),
       Since => Since, Stale => False));

   procedure Post (Item : GR.Report_Entry) with SPARK_Mode => Off is
      Delivered : Boolean := False;
   begin
      if CuBit.Handles.Valid (Desktop_Endpoints.Graphics_Poster) then
         CuBit.Graphics_Report.Posting.Post (Desktop_Endpoints.Graphics_Poster, Item, Delivered);
      end if;
      CuBit.Messages.debugPrint
        ("desktop: renderer report " & GR.Image (Item.Subject) & " " & GR.Path_Name (Item.Path)
         & " " & GR.Reason_Name (Item.Why) & " detail" & GR.Detail_Value'Image (Item.Detail)
         & (if Delivered then " (posted)" else " (not posted: no graphics-poster endpoint or devmgr busy)")
         & ASCII.LF);
   end Post;

end Desktop_Renderer_Report;
