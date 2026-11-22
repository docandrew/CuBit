pragma Ada_2022;
with ACPI_FADT.Transactions;
package body ACPI_Backend_PM_Timer with SPARK_Mode is
   use Interfaces;
   package T renames ACPI_FADT.Transactions;
   use type R.Status, R.Space_Kind, W.Outcome;
   function Prepare
     (Item : F.Description; Window : W.Granted_Window;
      Stamp, Register_ID : Unsigned_64;
      Delegable : Boolean := False) return Result
   is
      Block : constant R.Block_Result := R.Describe (Item, F.PM_Timer, Unsigned_64'Last);
      Config : P.Configuration;
      Admission : W.Outcome;
   begin
      if Block.Code /= R.Described then
         return (Code => Timer_Unavailable, Block_Status => Block.Code,
                 Admission => W.Invalid_Record);
      end if;
      Config :=
        (Tag => Stamp, Base => Block.Base, Length => Unsigned_64 (Block.Length),
         Space => (if Block.Space = R.System_Memory then P.Memory_Space else P.IO_Space),
         Readable => True, Writable => False,
         Widths => [T.Dword_Access => True, others => False],
         Register_Authority => (Resource_ID => Register_ID, Readable => True,
           Writable => False, Delegable => Delegable, Write_Mask => 0),
         Register_Offset => 0, Register_Width => T.Dword_Access);
      Admission := W.Check (Window, Config, W.Unestablished);
      if Admission /= W.Admitted then
         return (Code => Admission_Refused, Block_Status => Block.Code,
                 Admission => Admission);
      end if;
      return (Code => Ready, Block_Status => Block.Code, Admission => Admission,
              Configuration => Config, Source => Block.Source,
              Extended_Rejected => Block.Extended_Rejected);
   end Prepare;
end ACPI_Backend_PM_Timer;
