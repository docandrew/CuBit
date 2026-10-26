------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The installer's side of Open_Disk (CuBit.Disk_Install). Calling it
--  grants nothing: devmgr answers only through the disk-install badge.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Handles;

package CuBit.Disk_Install.Client is
   type Opened_Disk (Opened : Boolean := False) is record
      case Opened is
         when True  => Device : CuBit.Handles.Endpoint;
         when False => null;
      end case;
   end record;

   --  Service: the disk-install handle (devmgr refuses any other endpoint
   --  to it, which lacks the badge).
   function Open
     (Service : CuBit.Handles.Endpoint; Item : Open_Request) return Opened_Disk;
end CuBit.Disk_Install.Client;
