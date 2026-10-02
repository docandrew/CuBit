--  The hosted preview has no logstore: logs.recent is not offered.
package body CCL_Log_IO is
   function Available return Boolean is (False);
   procedure Recent
     (Service : String; Contract : CCL.Objects.Binding;
      Image : out CCL.Objects.Image; Success : out Boolean)
   is
      pragma Unreferenced (Service);
   begin
      Image := CCL.Objects.Empty (Contract);
      Success := False;
   end Recent;
end CCL_Log_IO;
