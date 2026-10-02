with Interfaces; use Interfaces;
with CuBit.Messages;

--  Registered services by name: the service catalog's names
--  (userspace/ccl/catalogs/native-runtime-services.ccl) for the drivers that
--  register, so a person can say "netstack" where the kernel says driver 3.
--  The process behind a name is SYSINFO_REGISTERED_DRIVER's answer.
package CuBit.Service_Names is
   NO_DRIVER : constant Unsigned_64 := 0;

   --  The registered-driver id named Name, or NO_DRIVER.
   function Driver_Of (Name : String) return Unsigned_64;

   --  The process registered as Name now, or 0 when none is.
   function Process_Of (Name : String) return Unsigned_64;
private
   use CuBit.Messages;
   MAX_NAME : constant := 16;
   type Entry_Name is record
      Text : String (1 .. MAX_NAME) := (others => ' ');
      Length : Natural range 0 .. MAX_NAME := 0;
   end record;
   function Named (Text : String) return Entry_Name is
     ((Text => Text & (1 .. MAX_NAME - Text'Length => ' '),
       Length => Text'Length))
     with Pre => Text'Length <= MAX_NAME;
   type Service is record
      Name : Entry_Name;
      Driver : Unsigned_64 := NO_DRIVER;
   end record;
   type Service_Table is array (Positive range <>) of Service;
   SERVICES : constant Service_Table :=
     ((Named ("keyboard"), DRIVER_KEYBOARD),
      (Named ("ata"), DRIVER_ATA),
      (Named ("network-stack"), DRIVER_NETSTACK),
      (Named ("netstack"), DRIVER_NETSTACK),
      (Named ("process-manager"), DRIVER_PROCMGR),
      (Named ("procmgr"), DRIVER_PROCMGR),
      (Named ("nvme"), DRIVER_NVME),
      (Named ("filesystem"), DRIVER_FS),
      (Named ("device-manager"), DRIVER_DEVMGR),
      (Named ("devmgr"), DRIVER_DEVMGR),
      (Named ("hda"), DRIVER_HDA),
      (Named ("mixer"), DRIVER_MIXER),
      (Named ("mouse"), DRIVER_MOUSE),
      (Named ("config"), DRIVER_CONFIG),
      (Named ("netmgr"), DRIVER_NETMGR),
      (Named ("logstore"), DRIVER_LOGSTORE),
      (Named ("ipc-test"), DRIVER_IPCTEST),
      (Named ("desktop"), DRIVER_DESKTOP),
      (Named ("display"), DRIVER_DISPLAY),
      (Named ("gpu"), DRIVER_GPU),
      (Named ("ccl-test-host"), DRIVER_CCL_TEST),
      (Named ("clock"), DRIVER_CLOCK));
end CuBit.Service_Names;
