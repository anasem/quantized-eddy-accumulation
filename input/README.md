Downloaded high-frequency data that are used as input to the simulation will be
located here. You can put your own high-frequency raw data as one RDS file.

for example: `raw_data_input.rds`

If you want to use your own high-frequency data as input for the simulation, the
input file needs to have the following variables:

| Variable Name | Description                 | Units                    |
|---------------|-----------------------------|--------------------------|
| time          | unique time stamp POSIXct   | seconds since unix epoch |
| CO2           | wet molar density           | $\mu mol\ m^{-3}$        |
| H2O           | wet molar density           | $mmol\ m^{-3}$           |
| Ts            | sonic temperature           | Kelvin                   |
| u, v, w       | 3D wind velocity components | $m\ s^{-1}$              |

