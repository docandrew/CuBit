with Vulkan_Upload_Record_FFI; with System;
package body Vulkan_Upload_Recording with SPARK_Mode is
   use type U.Phase, U.Transfer_Direction, S.I.Phase, System.Address;
   procedure Record_Transfer (Submission : in out V.State; Context : C.State;
      Upload : U.State; Source : S.State; Index : V.Source_Slot;
      Plan : G.Plan; Discard : Boolean; Accepted : out Boolean) is
   begin
      V.Admit_Draw (Submission, Accepted);
      if not Accepted then return; end if;
      if not G.Valid (Plan) or else G.Capacity (Plan) > U.Capacity (Upload) or else
         U.Current (Upload) /= U.Live or else U.Direction (Upload) /= U.Upload or else
         S.Current (Source) /= S.I.Live or else
         not U.Parent_Held (Upload, Context) or else not S.Parent_Held (Source, Context) or else
         C.Context (Context) /= V.Owner_Context (Submission) or else V.Source_Present (Submission, Index)
      then Accepted := False;
      else
         Vulkan_Upload_Record_FFI.Record_Transfer (V.Owner_Context (Submission),
           U.Description (Upload), S.Description (Source), Plan, Discard, Accepted);
      end if;
      if not Accepted then V.Reject_Frame (Submission); end if;
   end Record_Transfer;
end Vulkan_Upload_Recording;
