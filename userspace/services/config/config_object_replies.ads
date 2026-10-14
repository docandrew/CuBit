with CuBit.Handles;

--  Setting the current request's reply aside, for Config_Object_Native.
package Config_Object_Replies is
   procedure Save (Handle : out CuBit.Handles.Reply; Saved : out Boolean);
end Config_Object_Replies;
