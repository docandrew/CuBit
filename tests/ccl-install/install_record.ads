with CuBit.Installer_Protocol;

--  The last request the stand-in installer received.
package Install_Record is
   Last : CuBit.Installer_Protocol.Install_Request;
   Seen : Boolean := False;
end Install_Record;
