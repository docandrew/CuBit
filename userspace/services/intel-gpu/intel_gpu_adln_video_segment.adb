package body Intel_GPU_ADLN_Video_Segment with SPARK_Mode is
   Preparser_Off : constant Command_Word :=
     MI_ARB_Check or Preparser_Disable_Bit or 1;
   Preparser_On : constant Command_Word := MI_ARB_Check or Preparser_Disable_Bit;
   Arbitration_On : constant Command_Word := MI_ARB_On_Off or MI_ARB_Enable;
   Arbitration_Off : constant Command_Word := MI_ARB_On_Off;

   function Invalidate (Engine : Video_Engine) return Invalidate_Words is
      AUX : constant Command_Word := AUX_Invalidate_Register (Engine);
   begin
      return
        [Preparser_Off,
         MI_Flush_DW or Flush_DW_Length_DW or Flush_Store_Index or
           Flush_Op_Store_DW or Flush_Invalidate_TLB or Flush_Invalidate_BSD,
         PPHWSP_Scratch_Offset, 0, 0,
         MI_LRI_One_Remapped, AUX, AUX_Invalidate,
         MI_Semaphore_Poll_Register_EQ, 0, AUX, 0, 0,
         Preparser_On];
   end Invalidate;

   function Breadcrumb (Target : Timeline_Target; Value : Unsigned_64)
     return Breadcrumb_Words
   is
      Store : constant Command_Word :=
        MI_Flush_DW or Flush_DW_Length_QW or Flush_Op_Store_DW or
          (if Target.Kind = PPHWSP_Index then Flush_Store_Index else 0);
      Address : constant Command_Word :=
        Unsigned_32 (Target.Address mod 2 ** 32) or
          (if Target.Kind = GGTT_Address then Flush_Use_GTT else 0);
   begin
      return
        [MI_Flush_DW or Flush_DW_Length_DW, 0, 0, 0,  -- stall, no post-sync
         Store, Address, 0,
         Unsigned_32 (Value mod 2 ** 32), Unsigned_32 (Value / 2 ** 32),
         MI_User_Interrupt, Arbitration_On, MI_ARB_Check, MI_NOOP, MI_NOOP];
   end Breadcrumb;

   function Build_Batch
     (Engine : Video_Engine; Batch_GPU : Unsigned_64;
      Target : Timeline_Target; Value : Unsigned_64) return Segment
   is
      Result : Segment;
   begin
      if Batch_GPU = 0 or else Batch_GPU >= 2 ** 48 or else
        Batch_GPU mod 8 /= 0 or else not Valid_Target (Target) or else
        Value = 0
      then return Result; end if;
      declare
         Before : constant Invalidate_Words := Invalidate (Engine);
         After : constant Breadcrumb_Words := Breadcrumb (Target, Value);
      begin
         for I in Before'Range loop
            Result.Words (I) := Before (I);
         end loop;
         Result.Words (14 .. 19) :=
           [Arbitration_On, MI_BB_Start_PPGTT,
            Unsigned_32 (Batch_GPU mod 2 ** 32), Unsigned_32 (Batch_GPU / 2 ** 32),
            Arbitration_Off, MI_NOOP];
         for I in After'Range loop
            Result.Words (20 + I) := After (I);
            pragma Loop_Invariant
              (for all J in After'First .. I => Result.Words (20 + J) = After (J));
            pragma Loop_Invariant
              (Result.Words (15) = MI_BB_Start_PPGTT and
               Result.Words (16) = Unsigned_32 (Batch_GPU mod 2 ** 32) and
               Result.Words (17) = Unsigned_32 (Batch_GPU / 2 ** 32));
         end loop;
      end;
      Result.Valid := True;
      return Result;
   end Build_Batch;

   function Build_Signal (Target : Timeline_Target; Value : Unsigned_64)
     return Signal_Segment is
   begin
      if not Valid_Target (Target) or else Value = 0 then
         return (others => <>);
      end if;
      return (Valid => True, Words => Breadcrumb (Target, Value));
   end Build_Signal;
end Intel_GPU_ADLN_Video_Segment;
