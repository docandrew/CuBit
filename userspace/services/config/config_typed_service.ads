with Config_Object_Service;
with Config_Object_Native;

--  Native instance for config.svc's owning event loop and trusted attachment.
--  The durable worker
--  receives only Config-authorized requests, never public client access.
package Config_Typed_Service is new Config_Object_Service (Config_Object_Native);
