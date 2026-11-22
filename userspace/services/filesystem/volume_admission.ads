--  Admission is not a name lookup. Only explicit absence allows automatic
--  search to continue; malformed metadata and transport failures never do.
package Volume_Admission with SPARK_Mode is
   type Admission_Result is
     (Admitted, Provider_Not_Ready, No_Device, Insufficient_Resources,
      Grant_Rejected, Invalid_Session, Device_Error, Invalid_Description,
      Unsupported_Filesystem, Invalid_Filesystem);

   function May_Search_Next (Result : Admission_Result) return Boolean is
     (Result in Provider_Not_Ready | No_Device);

   function Description (Result : Admission_Result) return String is
     (case Result is
         when Admitted => "ready",
         when Provider_Not_Ready => "provider not ready",
         when No_Device => "no device offered",
         when Insufficient_Resources => "insufficient memory",
         when Grant_Rejected => "grant rejected",
         when Invalid_Session => "invalid block session",
         when Device_Error => "device I/O error",
         when Invalid_Description => "invalid device description",
         when Unsupported_Filesystem => "unsupported filesystem",
         when Invalid_Filesystem => "invalid filesystem geometry");
end Volume_Admission;
