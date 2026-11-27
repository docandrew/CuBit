pragma Ada_2022;
with Notification_ABI;
with PerCPUData;
with Process;
with Process.Notifications;
with Process.Private_Futex;
with Process.Wait_Timers;

package body Syscall.Waits is

    package NA renames Notification_ABI;

    procedure handleWait (arg0, arg1, arg2, arg3 : Unsigned_64; retval : out Unsigned_64) is
        tsc : Unsigned_64 := 0;
        passed : Boolean := False;
    begin
        if arg0 = NA.Notification then
            retval := Process.Notifications.wait (arg1, arg3);
        elsif arg0 = NA.Private_Word then
            if arg3 /= NA.Now and then arg3 /= NA.Forever then
                Process.Wait_Timers.Deadline_Of (arg3, tsc, passed);
            end if;
            retval := Process.Private_Futex.wait
              (address  => arg1,
               expected => arg2,
               timed    => arg3 /= NA.Forever,
               passed   => arg3 = NA.Now or else passed,
               tsc      => tsc);
        else
            retval := NA.Bad_Argument;
        end if;
    end handleWait;

    procedure handleWake (arg0, arg1, arg2 : Unsigned_64; retval : out Unsigned_64) is
    begin
        if arg0 = NA.Notification then
            retval := Process.Notifications.wake (arg1);
        elsif arg0 = NA.Private_Word then
            retval := Process.Private_Futex.wake (PerCPUData.getCurrentPID, arg1, arg2);
        else
            retval := NA.Bad_Argument;
        end if;
    end handleWake;

end Syscall.Waits;
