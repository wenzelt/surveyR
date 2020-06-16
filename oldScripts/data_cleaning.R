smartDeviceNames = c(
  "Smart Coffee Maker",
  "Smart Dishwasher",
  "Smart Door Lock",
  "Smart Doorbell",
  "Smart Electricity Meter",
  "Smart Electrical Outlet",
  "Smart Fridge",
  "Smart Gardening Equipment",
  "Smart Heating/Cooling System",
  "Smart Home Monitoring System",
  "Smart Lightbulb",
  "Smart Oven",
  "Smart Robot",
  "Smart Speaker",
  "Smart Stove",
  "Smart TV",
  "Smart Thermostat",
  "Smart Toy",
  "Smart Vacuum Cleaner",
  "Smart Washing Machine"
)


ssot_merged_likertCheck <-
  merge(
    x = ssot_filtered_sosci,
    y = select(likertScaleCheck, participant_id, var_flag),
    by = "participant_id" ,
    all.x = TRUE
  )
ssot_clean = subset(ssot_merged_likertCheck, var_flag == 0)
ssot_flagged = subset(ssot_merged_likertCheck, var_flag == 1)

description = describe(nums)

devices <-
  select(singleSourceOfTruthAppended, participant_id, R101_01:R101_20)
colnames(devices)[2:21] <- smartDeviceNames
devices_long <-
  gather(devices, device_code, truthful,-participant_id)
participant_devices <- subset(devices_long, truthful == TRUE)

### check for people with low variance in question blocks

no_variance = subset(
  singleSourceOfTruthAppended,
  sebis_DeviceSecurement_avg == sebis_ProactiveAwareness_avg |
    sebis_DeviceSecurement_avg == sebis_UpdatingBehaviour_avg |
    sebis_ProactiveAwareness_avg == sebis_UpdatingBehaviour_avg &&
    sebis_DeviceSecurement_avg == 7 |
    sebis_ProactiveAwareness_avg == 7 |
    sebis_UpdatingBehaviour_avg == 7
)

no_variance = subset(singleSourceOfTruthAppended,
                     ((
                       sebis_DeviceSecurement_avg == 7.0 ||
                         sebis_DeviceSecurement_avg == 1.0
                     ) &&
                       (
                         sebis_UpdatingBehaviour_avg == 7.0 ||
                           sebis_UpdatingBehaviour_avg == 1.0
                       ) || (
                         sebis_DeviceSecurement_avg == 7.0 ||
                           sebis_DeviceSecurement_avg == 1.0
                       ) && (
                         sebis_ProactiveAwareness_avg == 7.0 ||
                           sebis_ProactiveAwareness_avg == 1.0
                       ) || (
                         sebis_ProactiveAwareness_avg == 7.0 ||
                           sebis_ProactiveAwareness_avg == 1.0
                       ) && (
                         sebis_UpdatingBehaviour_avg == 7.0 ||
                           sebis_UpdatingBehaviour_avg == 1.0
                       )
                     ))

no_variance = subset(
  singleSourceOfTruthAppended,
  sebis_DeviceSecurement_avg == 7.00 &&
    sebis_UpdatingBehaviour_avg == 7.00
)
