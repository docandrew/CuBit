--  Test-only fault injection (CUBIT_DESKTOP_FAULT=exit-once), for the
--  headless desktop-restart test (BUG-001). Never in a product image.
package Desktop_Fault_Policy is
   --  Exit with a failure once the first frame is on screen, unless this
   --  desktop took the display over from an owner that ended: the first
   --  desktop fails, the one procmgr restarts lives.
   Exit_After_First_Frame : constant Boolean := True;
end Desktop_Fault_Policy;
