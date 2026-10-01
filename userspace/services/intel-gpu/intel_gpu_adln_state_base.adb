with Intel_GPU_Submission_Backing;
package body Intel_GPU_ADLN_State_Base with SPARK_Mode is
   pragma Compile_Time_Error
     (Intel_GPU_Submission_Backing.Render_State_GPU_VA mod 4096 /= 0 or else
      Intel_GPU_Submission_Backing.Shader_GPU_VA mod 4096 /= 0,
      "state base addresses must be page aligned");
   function Build (MOCS : Unsigned_32) return Image is
      Result : Image;
      Empty_Base, State_Base, Shader_Base : Unsigned_64;
      function Low (V : Unsigned_64) return Unsigned_32 is
        (Unsigned_32 (V and 16#FFFF_FFFF#));
      function High (V : Unsigned_64) return Unsigned_32 is
        (Unsigned_32 (Shift_Right (V, 32)));
   begin
      if MOCS = 0 or else MOCS > 126 or else MOCS mod 2 /= 0 then
         return Result;
      end if;
      Empty_Base := Encode (Base_Control'(MOCS => B7 (MOCS), others => <>));
      State_Base := Encode (Base_Control'
        (MOCS => B7 (MOCS), Address_Pages =>
           B52 (Intel_GPU_Submission_Backing.Render_State_GPU_VA / 4096), others => <>));
      Shader_Base := Encode (Base_Control'
        (MOCS => B7 (MOCS), Address_Pages =>
           B52 (Intel_GPU_Submission_Backing.Shader_GPU_VA / 4096), others => <>));
      Result.Data :=
        [Intel_GPU_ADLN_Vertex_Fetch.Encode
           (Header'(Length => 20, Subopcode => 1, Opcode => 1,
                    Subtype_Code => 0, others => <>)),
         Low (Empty_Base), High (Empty_Base),
         Encode (Stateless_Control'(MOCS => B7 (MOCS), others => <>)),
         Low (State_Base), High (State_Base), Low (State_Base), High (State_Base),
         Low (Empty_Base), High (Empty_Base), Low (Shader_Base), High (Shader_Base),
         Encode (Page_Bound'(others => <>)),
         Encode (Page_Bound'(Pages => 1, others => <>)),
         Encode (Page_Bound'(others => <>)),
         Encode (Page_Bound'(Pages => 1, others => <>)),
         Low (Empty_Base), High (Empty_Base),
         Encode (Bindless_Surface_Bound'(others => <>)),
         Low (Empty_Base), High (Empty_Base),
         Encode (Bindless_Sampler_Bound'(others => <>))];
      -- Bindless surface size zero is not a disable. This trusted shader never
      -- uses bindless accesses; its base zero is unmapped in this private VM.
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_ADLN_State_Base;
