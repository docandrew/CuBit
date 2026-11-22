------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Which of a configuration's interfaces xhci.drv drives.
--
--  @description
--  A hub is driven as a hub; a mass-storage interface as the optical
--  drive. Otherwise every boot-protocol HID interface whose role is still
--  free: a composite device (a wireless receiver with a boot keyboard and a
--  boot mouse interface, common for USB mice) gets BOTH interrupt
--  endpoints, not the keyboard alone. Before 2026-10-24 the driver took the
--  keyboard interface of such a receiver and ignored its mouse.
------------------------------------------------------------------------------
with USB_Configurations; use USB_Configurations;

package USB_Device_Roles with SPARK_Mode => On is

   type Role is (Unsupported, Hub_Role, Storage_Role, HID_Role);

   function Role_Name (R : Role) return String is
     (case R is
         when Unsupported => "unsupported", when Hub_Role => "hub",
         when Storage_Role => "storage", when HID_Role => "hid");

   type Selection is record
      Kind     : Role := Unsupported;
      Mouse    : Boolean := False;
      Keyboard : Boolean := False;
   end record;

   function Select_Roles
     (C : Configuration; Mouse_Free, Keyboard_Free, Storage_Free : Boolean)
      return Selection
   is
     (if C.Hub.Present then (Kind => Hub_Role, others => False)
      elsif C.Storage.Present and then Storage_Free then (Kind => Storage_Role, others => False)
      elsif (C.Mouse.Present and then Mouse_Free) or else (C.Keyboard.Present and then Keyboard_Free)
      then (Kind => HID_Role,
            Mouse => C.Mouse.Present and then Mouse_Free,
            Keyboard => C.Keyboard.Present and then Keyboard_Free)
      else (Kind => Unsupported, others => False))
   with Post =>
     (if Select_Roles'Result.Kind = HID_Role then
        Select_Roles'Result.Mouse = (C.Mouse.Present and then Mouse_Free)
        and then Select_Roles'Result.Keyboard = (C.Keyboard.Present and then Keyboard_Free)
        and then (Select_Roles'Result.Mouse or else Select_Roles'Result.Keyboard))
     and then (if not C.Hub.Present and then not (C.Storage.Present and then Storage_Free)
                 and then C.Mouse.Present and then Mouse_Free
               then Select_Roles'Result.Kind = HID_Role and then Select_Roles'Result.Mouse);

end USB_Device_Roles;
