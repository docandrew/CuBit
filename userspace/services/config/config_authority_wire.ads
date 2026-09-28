with Config_Authority;

--  Value-only decoding of the existing counted Config scope records.
--  The IPC shell authenticates the administrator and snapshots the input.
--  This package never installs authority or interprets zero records as wildcard.
package Config_Authority_Wire with Pure, SPARK_Mode is
   use type Config_Authority.Rule_Set;
   Entry_Bytes : constant := 72;
   Maximum_Bytes : constant := Entry_Bytes * Config_Authority.Maximum_Rules;

   procedure Decode
     (Data : String; Rules : out Config_Authority.Rule_Set; Accepted : out Boolean)
     with Post => (if not Accepted then Rules = Config_Authority.Empty_Rules);
end Config_Authority_Wire;
