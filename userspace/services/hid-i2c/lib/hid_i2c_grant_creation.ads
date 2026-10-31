pragma Ada_2022;
package HID_I2C_Grant_Creation with SPARK_Mode, Pure is
   type Status is (Created, Refused, Uncertain);
   -- Created supplies a complete live reference. Refused proves no grant was
   -- created. Uncertain supplies no usable reference and cannot prove absence.
end HID_I2C_Grant_Creation;
